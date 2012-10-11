/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.runtime.universe

import scala.concurrent.{ Future, Promise }
import scala.util.control.NonFatal
import scala.collection.mutable.ListBuffer

class ExprBuilder[C <: Context with Singleton](val c: C) {
  import c.universe.{ reify, Literal, Constant }

  /* Make a partial function literal handling case #num:
   * 
   *     {
   *       case any if any == num => rhs
   *     }
   */
  def mkHandler(num: Int, rhs: c.Expr[Unit]): c.Expr[PartialFunction[Int, Unit]] = {
    val numLiteral = c.Expr[Int](Literal(Constant(num)))
    
    reify(new PartialFunction[Int, Unit] {
      def isDefinedAt(`x$1`: Int) =
        `x$1` == numLiteral.splice
      def apply(`x$1`: Int) = `x$1` match {
        case any: Int if any == numLiteral.splice =>
          rhs.splice
      }
    })
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
    
    class AsyncStateBuilder {
      /* Statements preceding an await call. */
      private val stats = ListBuffer[c.Tree]()
      
      /* Argument of an await call. */
      var awaitable: c.Tree = null
      
      /* Result name of an await call. */
      var resultName: c.universe.TermName = null
      
      /* Result type of an await call. */
      var resultType: c.universe.Type = null
      
      def += (stat: c.Tree): Unit =
        stats += stat
      
      /* Result needs to be created as a var at the beginning of the transformed method body, so that
         it is visible in subsequent states of the state machine.
       */
      def complete(awaitArg: c.Tree, awaitResultName: c.universe.TermName, awaitResultType: c.Tree): Unit = {
        awaitable = c.resetAllAttrs(awaitArg.duplicate)
        resultName = awaitResultName
        resultType = awaitResultType.tpe
      }
      
      override def toString: String = {
        val statsBeforeAwait = stats.mkString("\n")
        s"ASYNC STATE:\n$statsBeforeAwait \nawaitable: $awaitable \nresult name: $resultName"
      }
      
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
        val nakedStats = stats.map(stat => c.resetAllAttrs(stat.duplicate))
        val block = Block((nakedStats :+ mkOnCompleteTree): _*)
        builder.mkHandler(num, c.Expr[Unit](block))
      }
      
      def lastExprTree: c.Tree = {
        assert(awaitable == null)
        if (stats.size == 1)
          c.resetAllAttrs(stats(0).duplicate)
        else {
          val nakedStats = stats.map(stat => c.resetAllAttrs(stat.duplicate))
          Block(nakedStats: _*)
        }
      }
      
      //TODO: complete for other primitive types, how to handle value classes?
      def varDefForResult: c.Tree = {
        val rhs =
          if (resultType <:< definitions.IntTpe) Literal(Constant(0))
          else if (resultType <:< definitions.LongTpe) Literal(Constant(0L))
          else if (resultType <:< definitions.BooleanTpe) Literal(Constant(false))
          else Literal(Constant(null))
        ValDef(Modifiers(Flag.MUTABLE), resultName, TypeTree(resultType), rhs)
      }
    }
    
    
    body.tree match {
      case Block(stats, expr) =>
        val asyncStates = ListBuffer[AsyncStateBuilder]()
        var stateBuilder = new AsyncStateBuilder // current state builder
        val awaitMethod = awaitSym(c)
        
        for (stat <- stats) {
          stat match {
            // the val name = await(..) pattern
            case ValDef(mods, name, tpt, Apply(fun, args)) if fun.symbol == awaitMethod =>
              stateBuilder.complete(args(0), name, tpt)
              asyncStates += stateBuilder
              stateBuilder = new AsyncStateBuilder
              
            case _ =>
              stateBuilder += stat
          }
        }
        // complete last state builder (representing the expressions after the last await)
        stateBuilder += expr
        asyncStates += stateBuilder
        
        vprintln("states of current method:")
        asyncStates foreach vprintln
        
        // also return index of last state
        def buildHandlerExpr(): (c.Expr[PartialFunction[Int, Unit]], Int) = {
          var handlerExpr = asyncStates(0).mkHandlerForState(1) // state 0 but { case 1 => ... }
          var i = 1
          while (asyncStates(i).awaitable != null) {
            val handlerForNextState = asyncStates(i).mkHandlerForState(i+1)
            val currentHandlerTreeNaked = c.resetAllAttrs(handlerExpr.tree.duplicate)
            handlerExpr = reify {
              c.Expr(currentHandlerTreeNaked).asInstanceOf[c.Expr[PartialFunction[Int, Unit]]].splice orElse handlerForNextState.splice
            }
            i += 1
          }
          // asyncStates(i) does not end with `await`
          (handlerExpr, i)
        }
        
        val (handlerExpr, indexOfLastState) = buildHandlerExpr()
        vprintln(s"GENERATED handler expr ($indexOfLastState):")
        vprintln(handlerExpr)
        
        val localVarDefs = ListBuffer[c.Tree]()
        for (state <- asyncStates.init) // exclude last state (doesn't have await result)
          localVarDefs += state.varDefForResult
        // pad up to 5 var defs
        if (localVarDefs.size < 5)
          for (_ <- localVarDefs.size until 5) localVarDefs += EmptyTree
        
        val handlerForLastState: c.Expr[PartialFunction[Int, Unit]] = {
          val tree = Apply(Select(Ident("result"), c.universe.newTermName("success")),
                           List(asyncStates(indexOfLastState).lastExprTree))
          builder.mkHandler(indexOfLastState + 1, c.Expr[Unit](tree))
        }
        
        vprintln("GENERATED handler for last state:")
        vprintln(handlerForLastState)
        
        reify {
          val result = Promise[T]()
          var state = 0
          
          c.Expr(localVarDefs(0)).splice
          c.Expr(localVarDefs(1)).splice
          c.Expr(localVarDefs(2)).splice
          c.Expr(localVarDefs(3)).splice
          c.Expr(localVarDefs(4)).splice
          
          def resume(): Unit = {
            state += 1
            
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

      case _ =>
        // issue error message
        reify {
          sys.error("expression not supported by async")
        }
    }
  }

}
