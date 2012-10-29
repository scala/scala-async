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
class ExprBuilder[C <: Context with Singleton](val c: C) extends AsyncUtils {
  builder =>
  
  import c.universe._
  import Flag._
  
  private val awaitMethod = awaitSym(c)
  
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
  
  def mkStateTree(nextState: Int): c.Tree =
    Assign(
      Ident(newTermName("state")),
      Literal(Constant(nextState)))
  
  def mkVarDefTree(resultType: c.universe.Type, resultName: c.universe.TermName): c.Tree = {
    val rhs =
      if (resultType <:< definitions.IntTpe) Literal(Constant(0))
      else if (resultType <:< definitions.LongTpe) Literal(Constant(0L))
      else if (resultType <:< definitions.BooleanTpe) Literal(Constant(false))
      else Literal(Constant(null))
    ValDef(Modifiers(Flag.MUTABLE), resultName, TypeTree(resultType), rhs)
  }
  
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
  
  class AsyncState(stats: List[c.Tree], protected val state: Int, protected val nextState: Int) {
    val body: c.Tree =
      if (stats.size == 1) stats.head
      else Block(stats: _*)
    
    val varDefs: List[(c.universe.TermName, c.universe.Type)] = List()
    
    def mkHandlerTreeForState(): c.Tree =
      mkHandlerTree(state, Block((stats :+ mkStateTree(nextState)): _*))
    
    def mkHandlerTreeForState(nextState: Int): c.Tree =
      mkHandlerTree(state, Block((stats :+ mkStateTree(nextState)): _*))
    
    def varDefForResult: Option[c.Tree] =
      None
    
    def allVarDefs: List[c.Tree] =
      varDefForResult.toList ++ varDefs.map(p => mkVarDefTree(p._2, p._1))
    
    override val toString: String =
      s"AsyncState #$state, next = $nextState"
  }
  
  class AsyncStateWithIf(stats: List[c.Tree], state: Int)
      extends AsyncState(stats, state, 0) { // nextState unused, since encoded in then and else branches
    
    override def mkHandlerTreeForState(): c.Tree =
      mkHandlerTree(state, Block(stats: _*))
    
    //TODO mkHandlerTreeForState(nextState: Int)
    
    override val toString: String =
      s"AsyncStateWithIf #$state, next = $nextState"
  }
  
  abstract class AsyncStateWithAwait(stats: List[c.Tree], state: Int, nextState: Int)
      extends AsyncState(stats, state, nextState) {
    val awaitable: c.Tree
    val resultName: c.universe.TermName
    val resultType: c.universe.Type
    
    override val toString: String =
      s"AsyncStateWithAwait #$state, next = $nextState"
    
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

    /* Make an `onComplete` invocation which sets the state to `nextState` upon resuming:
     * 
     *     awaitable.onComplete {
     *       case tr =>
     *         resultName = tr.get
     *         state = `nextState`
     *         resume()
     *     }
     */
    def mkOnCompleteStateTree(nextState: Int): c.Tree = {
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
              Block(tryGetTree, mkStateTree(nextState), Apply(Ident("resume"), List())) // rhs of case
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
    override def mkHandlerTreeForState(): c.Tree = {
      assert(awaitable != null)
      mkHandlerTree(state, Block((stats :+ mkOnCompleteIncrStateTree): _*))
    }
    
    override def mkHandlerTreeForState(nextState: Int): c.Tree = {
      assert(awaitable != null)
      mkHandlerTree(state, Block((stats :+ mkOnCompleteStateTree(nextState)): _*))
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
  class AsyncStateBuilder(state: Int) extends Builder[c.Tree, AsyncState] {
    self =>
    
    /* Statements preceding an await call. */
    private val stats = ListBuffer[c.Tree]()
    
    /* Argument of an await call. */
    var awaitable: c.Tree = null
    
    /* Result name of an await call. */
    var resultName: c.universe.TermName = null
    
    /* Result type of an await call. */
    var resultType: c.universe.Type = null
    
    var nextState: Int = state + 1
    
    private val varDefs = ListBuffer[(c.universe.TermName, c.universe.Type)]()
    
    def += (stat: c.Tree): this.type = {
      stats += c.resetAllAttrs(stat.duplicate)
      this
    }
    
    //TODO do not ignore `mods`
    def addVarDef(mods: Any, name: c.universe.TermName, tpt: c.Tree): this.type = {
      varDefs += (name -> tpt.tpe)
      this
    }
    
    def result(): AsyncState =
      if (awaitable == null)
        new AsyncState(stats.toList, state, nextState) {
          override val varDefs = self.varDefs.toList
        }
      else
        new AsyncStateWithAwait(stats.toList, state, nextState) {
          val awaitable = self.awaitable
          val resultName = self.resultName
          val resultType = self.resultType
          override val varDefs = self.varDefs.toList
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
    def complete(awaitArg: c.Tree, awaitResultName: c.universe.TermName, awaitResultType: Tree, nextState: Int = state + 1): this.type = {
      awaitable = c.resetAllAttrs(awaitArg.duplicate)
      resultName = awaitResultName
      resultType = awaitResultType.tpe
      this.nextState = nextState
      this
    }
    
    def complete(nextState: Int): this.type = {
      this.nextState = nextState
      this
    }
    
    def resultWithIf(condTree: c.Tree, thenState: Int, elseState: Int): AsyncState = {
      // 1. build changed if-else tree
      // 2. insert that tree at the end of the current state
      val cond = c.resetAllAttrs(condTree.duplicate)
      this += If(cond, mkStateTree(thenState), mkStateTree(elseState))
      new AsyncStateWithIf(stats.toList, state) {
        override val varDefs = self.varDefs.toList
      }
    }
    
    override def toString: String = {
      val statsBeforeAwait = stats.mkString("\n")
      s"ASYNC STATE:\n$statsBeforeAwait \nawaitable: $awaitable \nresult name: $resultName"
    }
  }

  /* current issue:
  def m2(y: Int): Future[Int] = async {
    val f = m1(y)
    if (y > 0) {
      val x = await(f)
      x + 2
    } else {
      val x = await(f)
      x - 2
    }
  }

   */
  class AsyncBlockBuilder(stats: List[c.Tree], expr: c.Tree, startState: Int, endState: Int, budget: Int) {
    val asyncStates = ListBuffer[builder.AsyncState]()
    
    private var stateBuilder = new builder.AsyncStateBuilder(startState) // current state builder
    private var currState = startState
    
    private var remainingBudget = budget
    
    // populate asyncStates
    for (stat <- stats) stat match {
      // the val name = await(..) pattern
      case ValDef(mods, name, tpt, Apply(fun, args)) if fun.symbol == awaitMethod =>
        asyncStates += stateBuilder.complete(args(0), name, tpt).result // complete with await
        currState += 1
        stateBuilder = new builder.AsyncStateBuilder(currState)
        
      case ValDef(mods, name, tpt, rhs) =>
        stateBuilder.addVarDef(mods, name, tpt)
        stateBuilder += // instead of adding `stat` we add a simple assignment
          Assign(Ident(name), c.resetAllAttrs(rhs.duplicate))
        
      case If(cond, thenp, elsep) =>
        val ifBudget: Int = remainingBudget / 2
        remainingBudget -= ifBudget
        println(s"ASYNC IF: ifBudget = $ifBudget")
        // state that we continue with after if-else: currState + ifBudget
        
        val thenBudget: Int = ifBudget / 2
        val elseBudget = ifBudget - thenBudget
        
        asyncStates +=
          stateBuilder.resultWithIf(cond, currState + 1, currState + thenBudget)
        
        val thenBuilder = thenp match {
          case Block(thenStats, thenExpr) =>
            new AsyncBlockBuilder(thenStats, thenExpr, currState + 1, currState + ifBudget, thenBudget)
          case _ =>
            new AsyncBlockBuilder(List(thenp), Literal(Constant(())), currState + 1, currState + ifBudget, thenBudget)
        }
        println("ASYNC IF: states of thenp:")
        for (s <- thenBuilder.asyncStates)
          println(s.toString)
        
        // insert states of thenBuilder into asyncStates
        asyncStates ++= thenBuilder.asyncStates
        
        val elseBuilder = elsep match {
          case Block(elseStats, elseExpr) =>
            new AsyncBlockBuilder(elseStats, elseExpr, currState + thenBudget, currState + ifBudget, elseBudget)
          case _ =>
            new AsyncBlockBuilder(List(elsep), Literal(Constant(())), currState + thenBudget, currState + ifBudget, elseBudget)
        }
        // insert states of elseBuilder into asyncStates
        asyncStates ++= elseBuilder.asyncStates
        
        // create new state builder for state `currState + ifBudget`
        currState = currState + ifBudget
        stateBuilder = new builder.AsyncStateBuilder(currState)
        
      case _ =>
        stateBuilder += stat
    }
    // complete last state builder (representing the expressions after the last await)
    stateBuilder += expr
    val lastState = stateBuilder.complete(endState).result
    asyncStates += lastState
    
    /* Builds the handler expression for a sequence of async states.
     */
    def mkHandlerExpr(): c.Expr[PartialFunction[Int, Unit]] = {
      assert(asyncStates.size > 1)
      
      println(s"!!ASYNC mkHandlerExpr: asyncStates.size = ${asyncStates.size}")
      println(s"!!ASYNC state 0: ${asyncStates(0)}")
      
      var handlerTree =
        if (asyncStates.size > 2) asyncStates(0).mkHandlerTreeForState()
        else asyncStates(0).mkHandlerTreeForState(endState)
      var handlerExpr =
        c.Expr(handlerTree).asInstanceOf[c.Expr[PartialFunction[Int, Unit]]]
      
      if (asyncStates.size == 2)
        handlerExpr
      else if (asyncStates.size == 3) {
        // asyncStates(1) must continue with endState
        val handlerTreeForLastState = asyncStates(1).mkHandlerTreeForState(endState)
        val currentHandlerTreeNaked = c.resetAllAttrs(handlerExpr.tree.duplicate)
        c.Expr(
          Apply(Select(currentHandlerTreeNaked, newTermName("orElse")),
                List(handlerTreeForLastState))).asInstanceOf[c.Expr[PartialFunction[Int, Unit]]]
      } else { // asyncStates.size > 3
        var i = startState + 1
      
        println("!!ASYNC start for loop")
        
        // do not traverse first state: asyncStates.tail
        // do not traverse last state:  asyncStates.tail.init
        // handle second to last state specially: asyncStates.tail.init.init
        for (asyncState <- asyncStates.tail.init.init) {
          println(s"!!ASYNC current asyncState: $asyncState")
          val handlerTreeForNextState = asyncState.mkHandlerTreeForState()
          val currentHandlerTreeNaked = c.resetAllAttrs(handlerExpr.tree.duplicate)
          handlerExpr = c.Expr(
            Apply(Select(currentHandlerTreeNaked, newTermName("orElse")),
                  List(handlerTreeForNextState))).asInstanceOf[c.Expr[PartialFunction[Int, Unit]]]
          i += 1
        }
        
        val lastState = asyncStates.tail.init.last
        println(s"!!ASYNC current asyncState (forced to $endState): $lastState")
        val handlerTreeForLastState = lastState.mkHandlerTreeForState(endState)
        val currentHandlerTreeNaked = c.resetAllAttrs(handlerExpr.tree.duplicate)
        c.Expr(
          Apply(Select(currentHandlerTreeNaked, newTermName("orElse")),
                List(handlerTreeForLastState))).asInstanceOf[c.Expr[PartialFunction[Int, Unit]]]
      }
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
        val asyncBlockBuilder = new builder.AsyncBlockBuilder(stats, expr, 0, 1000, 1000)
        
        vprintln(s"states of current method (${ asyncBlockBuilder.asyncStates }):")
        asyncBlockBuilder.asyncStates foreach vprintln

        val handlerExpr = asyncBlockBuilder.mkHandlerExpr()
        
        vprintln(s"GENERATED handler expr:")
        vprintln(handlerExpr)
        
        val localVarDefs = ListBuffer[c.Tree]()
        for (state <- asyncBlockBuilder.asyncStates.init) // exclude last state (doesn't have await result)
          localVarDefs ++= //state.varDefForResult.toList
            state.allVarDefs
        // pad up to 5 var defs
        if (localVarDefs.size < 5)
          for (_ <- localVarDefs.size until 5) localVarDefs += EmptyTree
        
        val handlerForLastState: c.Expr[PartialFunction[Int, Unit]] = {
          val tree = Apply(Select(Ident("result"), c.universe.newTermName("success")),
                           List(asyncBlockBuilder.asyncStates.last.body))
          //builder.mkHandler(indexOfLastState + 1, c.Expr[Unit](tree))
          builder.mkHandler(1000, c.Expr[Unit](tree))
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
