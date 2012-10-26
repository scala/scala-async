/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.runtime.universe

import scala.concurrent.{ Future, Promise }
import scala.util.control.NonFatal
import scala.collection.mutable.{ ListBuffer, Builder }

/*
 * @author Philipp Haller
 */
class ExprBuilder[C <: Context with Singleton](val c: C) {
  builder =>
  
  import c.universe._
  import Flag._
  
  /* Make a partial function literal handling case #num:
   * 
   *     {
   *       case any if any == num => rhs
   *     }
   */
  def mkHandler(num: Int, rhs: c.Expr[Unit]): c.Expr[PartialFunction[Int, Unit]] = {
/*
    val numLiteral = c.Expr[Int](Literal(Constant(num)))
    
    reify(new PartialFunction[Int, Unit] {
      def isDefinedAt(`x$1`: Int) =
        `x$1` == numLiteral.splice
      def apply(`x$1`: Int) = `x$1` match {
        case any: Int if any == numLiteral.splice =>
          rhs.splice
      }
    })
*/
    val rhsTree = c.resetAllAttrs(rhs.tree.duplicate)
    val handlerTree = mkHandlerTree(num, rhsTree)
    c.Expr(handlerTree).asInstanceOf[c.Expr[PartialFunction[Int, Unit]]]
  }
  
  def mkIncrStateTree(): c.Tree =
    Assign(
      Ident(newTermName("state")),
      Apply(Select(Ident(newTermName("state")), newTermName("$plus")), List(Literal(Constant(1)))))
  
  def mkHandlerTree(num: Int, rhs: c.Tree): c.Tree = {
    val partFunIdent = Ident(c.mirror.staticClass("scala.PartialFunction"))
    val intIdent = Ident(definitions.IntClass)
    val unitIdent = Ident(definitions.UnitClass)
    
    Block(List(
      // anonymous subclass of PartialFunction[Int, Unit]
      ClassDef(Modifiers(FINAL), newTypeName("$anon"), List(), Template(List(AppliedTypeTree(partFunIdent, List(intIdent, unitIdent))),
        emptyValDef, List(

          DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
            Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),

          DefDef(Modifiers(), newTermName("isDefinedAt"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("x$1"), intIdent, EmptyTree))), TypeTree(),
            Apply(Select(Ident(newTermName("x$1")), newTermName("$eq$eq")), List(Literal(Constant(num))))),
          
          DefDef(Modifiers(), newTermName("apply"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("x$1"), intIdent, EmptyTree))), TypeTree(),
            Match(Ident(newTermName("x$1")), List(
              CaseDef(
                // pattern
                Bind(newTermName("any"), Typed(Ident(nme.WILDCARD), intIdent)),
                // guard
                Apply(Select(Ident(newTermName("any")), newTermName("$eq$eq")), List(Literal(Constant(num)))),
                rhs
              )
            ))
          )
          
        ))
      )),
      Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List())
    )
  }
  
  class AsyncState(stats: List[c.Tree]) {
    val body: c.Tree =
      if (stats.size == 1) stats.head
      else Block(stats: _*)
    
    def mkHandlerTreeForState(num: Int): c.Tree =
      mkHandlerTree(num, Block((stats :+ mkIncrStateTree()): _*))
    
    def varDefForResult: Option[c.Tree] =
      None
  }
  
  abstract class AsyncStateWithAwait(stats: List[c.Tree]) extends AsyncState(stats) {
    val awaitable: c.Tree
    val resultName: c.universe.TermName
    val resultType: c.universe.Type
    
    /* Make an `onComplete` invocation:
     * 
     *     awaitable.onComplete {
     *       case tr =>
     *         resultName = tr.get
     *         resume()
     *     }
     */
    def mkOnCompleteTree: c.Tree = {
      val assignTree =
        Assign(
          Ident(resultName.toString),
          Select(Ident("tr"), c.universe.newTermName("get"))
        )
      val handlerTree =
        Match(
          EmptyTree,
          List(
            CaseDef(Bind(c.universe.newTermName("tr"), Ident("_")), EmptyTree,
              Block(assignTree, Apply(Ident("resume"), List())) // rhs of case
            )
          )
        )
      Apply(
        Select(awaitable, c.universe.newTermName("onComplete")),
        List(handlerTree)
      )
    }
    
    /* Make an `onComplete` invocation which increments the state upon resuming:
     * 
     *     awaitable.onComplete {
     *       case tr =>
     *         resultName = tr.get
     *         state += 1
     *         resume()
     *     }
     */
    def mkOnCompleteIncrStateTree: c.Tree = {
      val tryGetTree =
        Assign(
          Ident(resultName.toString),
          Select(Ident("tr"), c.universe.newTermName("get"))
        )
      val handlerTree =
        Match(
          EmptyTree,
          List(
            CaseDef(Bind(c.universe.newTermName("tr"), Ident("_")), EmptyTree,
              Block(tryGetTree, mkIncrStateTree(), Apply(Ident("resume"), List())) // rhs of case
            )
          )
        )
      Apply(
        Select(awaitable, c.universe.newTermName("onComplete")),
        List(handlerTree)
      )
    }

    /* Make a partial function literal handling case #num:
     * 
     *     {
     *       case any if any == num =>
     *         stats
     *         awaitable.onComplete {
     *           case tr =>
     *             resultName = tr.get
     *             resume()
     *         }
     *     }
     */
    def mkHandlerForState(num: Int): c.Expr[PartialFunction[Int, Unit]] = {
      assert(awaitable != null)
      builder.mkHandler(num, c.Expr[Unit](Block((stats :+ mkOnCompleteTree): _*)))
    }
    
    /* Make a partial function literal handling case #num:
     * 
     *     {
     *       case any if any == num =>
     *         stats
     *         awaitable.onComplete {
     *           case tr =>
     *             resultName = tr.get
     *             state += 1
     *             resume()
     *         }
     *     }
     */
    override def mkHandlerTreeForState(num: Int): c.Tree = {
      assert(awaitable != null)
      builder.mkHandlerTree(num, Block((stats :+ mkOnCompleteIncrStateTree): _*))
    }
    
    //TODO: complete for other primitive types, how to handle value classes?
    override def varDefForResult: Option[c.Tree] = {
      val rhs =
        if (resultType <:< definitions.IntTpe) Literal(Constant(0))
        else if (resultType <:< definitions.LongTpe) Literal(Constant(0L))
        else if (resultType <:< definitions.BooleanTpe) Literal(Constant(false))
        else Literal(Constant(null))
      Some(
        ValDef(Modifiers(Flag.MUTABLE), resultName, TypeTree(resultType), rhs)
      )
    }
  }
  
  /*
   * Builder for a single state of an async method.
   */
  class AsyncStateBuilder extends Builder[c.Tree, AsyncState] {
    self =>
    
    /* Statements preceding an await call. */
    private val stats = ListBuffer[c.Tree]()
    
    /* Argument of an await call. */
    var awaitable: c.Tree = null
    
    /* Result name of an await call. */
    var resultName: c.universe.TermName = null
    
    /* Result type of an await call. */
    var resultType: c.universe.Type = null
    
    def += (stat: c.Tree): this.type = {
      stats += c.resetAllAttrs(stat.duplicate)
      this
    }
    
    def result(): AsyncState =
      if (awaitable == null)
        new AsyncState(stats.toList)
      else
        new AsyncStateWithAwait(stats.toList) {
          val awaitable = self.awaitable
          val resultName = self.resultName
          val resultType = self.resultType
        }
    
    def clear(): Unit = {
      stats.clear()
      awaitable = null
      resultName = null
      resultType = null
    }
    
    /* Result needs to be created as a var at the beginning of the transformed method body, so that
     * it is visible in subsequent states of the state machine.
     *
     * @param awaitArg         the argument of await
     * @param awaitResultName  the name of the variable that the result of await is assigned to
     * @param awaitResultType  the type of the result of await
     */
    def complete(awaitArg: c.Tree, awaitResultName: c.universe.TermName, awaitResultType: Tree): this.type = {
      awaitable = c.resetAllAttrs(awaitArg.duplicate)
      resultName = awaitResultName
      resultType = awaitResultType.tpe
      this
    }
    
    override def toString: String = {
      val statsBeforeAwait = stats.mkString("\n")
      s"ASYNC STATE:\n$statsBeforeAwait \nawaitable: $awaitable \nresult name: $resultName"
    }
  }

}


/*
 * @author Philipp Haller
 */
object Async extends AsyncUtils {

  def async[T](body: T): Future[T] = macro asyncImpl[T]
  
  def await[T](awaitable: Future[T]): T = ???
  
  def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[Future[T]] = {
    import c.universe._
    
    val builder = new ExprBuilder[c.type](c)
    val awaitMethod = awaitSym(c)
    
    body.tree match {
      case Block(stats, expr) =>
        val asyncStates = ListBuffer[builder.AsyncState]()
        var stateBuilder = new builder.AsyncStateBuilder // current state builder
        
        for (stat <- stats) stat match {
          // the val name = await(..) pattern
          case ValDef(mods, name, tpt, Apply(fun, args)) if fun.symbol == awaitMethod =>
            asyncStates += stateBuilder.complete(args(0), name, tpt).result // complete with await
            stateBuilder = new builder.AsyncStateBuilder

          case _ =>
            stateBuilder += stat
        }
        // complete last state builder (representing the expressions after the last await)
        asyncStates += (stateBuilder += expr).result
        
        vprintln("states of current method:")
        asyncStates foreach vprintln

        /* Builds the handler expression for a sequence of async states.
         * Also returns the index of the last state.
         */
        def buildHandlerExpr(): (c.Expr[PartialFunction[Int, Unit]], Int) = {
          //var handlerExpr = asyncStates(0).mkHandlerForState(1) // state 0 but { case 1 => ... }
          var handlerTree = asyncStates(0).mkHandlerTreeForState(0)
          var handlerExpr = c.Expr(handlerTree).asInstanceOf[c.Expr[PartialFunction[Int, Unit]]]

          var i = 1
          for (asyncState <- asyncStates.tail.init) {
            //val handlerForNextState = asyncStates(i).mkHandlerForState(i+1)
            val handlerTreeForNextState = asyncState.mkHandlerTreeForState(i)
            val currentHandlerTreeNaked = c.resetAllAttrs(handlerExpr.tree.duplicate)
            handlerExpr = c.Expr(
              Apply(Select(currentHandlerTreeNaked, newTermName("orElse")), List(handlerTreeForNextState))
            ).asInstanceOf[c.Expr[PartialFunction[Int, Unit]]]
            i += 1
          }
          // asyncStates(i) does not end with `await` (asyncStates(i).awaitable == null)
          (handlerExpr, i)
        }
        val (handlerExpr, indexOfLastState) = buildHandlerExpr()
        
        vprintln(s"GENERATED handler expr ($indexOfLastState):")
        vprintln(handlerExpr)
        
        val localVarDefs = ListBuffer[c.Tree]()
        for (state <- asyncStates.init) // exclude last state (doesn't have await result)
          localVarDefs ++= state.varDefForResult.toList
        // pad up to 5 var defs
        if (localVarDefs.size < 5)
          for (_ <- localVarDefs.size until 5) localVarDefs += EmptyTree
        
        val handlerForLastState: c.Expr[PartialFunction[Int, Unit]] = {
          val tree = Apply(Select(Ident("result"), c.universe.newTermName("success")),
                           List(asyncStates(indexOfLastState).body))
          //builder.mkHandler(indexOfLastState + 1, c.Expr[Unit](tree))
          builder.mkHandler(indexOfLastState, c.Expr[Unit](tree))
        }
        
        vprintln("GENERATED handler for last state:")
        vprintln(handlerForLastState)
        
        val methodBody = reify {
          val result = Promise[T]()
          var state = 0
          
          c.Expr(localVarDefs(0)).splice
          c.Expr(localVarDefs(1)).splice
          c.Expr(localVarDefs(2)).splice
          c.Expr(localVarDefs(3)).splice
          c.Expr(localVarDefs(4)).splice
          
          def resume(): Unit = {
            //state += 1
            
            var handler: PartialFunction[Int, Unit] =
              handlerExpr.splice
            
            try {
              (handler orElse handlerForLastState.splice)(state)
            } catch {
              case NonFatal(t) => result.failure(t)
            }
          }

          resume()
          result.future
        }
        
        //println("ASYNC: Generated method body:")
        //println(c.universe.showRaw(methodBody))
        methodBody

      case _ =>
        // issue error message
        reify {
          sys.error("expression not supported by async")
        }
    }
  }

}
