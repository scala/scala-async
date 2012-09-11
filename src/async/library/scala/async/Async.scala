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

/*
 * @author Philipp Haller
 */
object Async extends AsyncUtils {

  def async[T](body: T): Future[T] = macro asyncImpl[T]
  
  def await[T](awaitable: Future[T]): T = ???
  
  def asyncImpl[T: c.AbsTypeTag](c: Context)(body: c.Expr[T]): c.Expr[Future[T]] = {
    import c.universe._
    
    def mkHandlers(rhs: c.Expr[Unit]): Seq[c.Expr[PartialFunction[Int, Unit]]] = {
      //TODO: come up with much better way to do this
      val handlers = ListBuffer[c.Expr[PartialFunction[Int, Unit]]]()
      handlers += reify(new PartialFunction[Int, Unit] {
        def isDefinedAt(x_synth: Int) = x_synth == 0
        def apply(x_synth: Int) = x_synth match { case 0 => rhs.splice }
      })
      handlers += reify(new PartialFunction[Int, Unit] {
        def isDefinedAt(x_synth: Int) = x_synth == 1
        def apply(x_synth: Int) = x_synth match { case 1 => rhs.splice }
      })
      handlers += reify(new PartialFunction[Int, Unit] {
        def isDefinedAt(x_synth: Int) = x_synth == 2
        def apply(x_synth: Int) = x_synth match { case 2 => rhs.splice }
      })
      handlers += reify(new PartialFunction[Int, Unit] {
        def isDefinedAt(x_synth: Int) = x_synth == 3
        def apply(x_synth: Int) = x_synth match { case 3 => rhs.splice }
      })
      handlers
    }
    
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
      
      /* Construct a partial function literal handling case #num.
       */
      def handlerForState(num: Int): c.Expr[PartialFunction[Int, Unit]] = {
        assert(awaitable != null)
        val nakedStats = stats.map(stat => c.resetAllAttrs(stat.duplicate)).toList
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
        val onCompleteTree =
          Apply(
            Select(awaitable, c.universe.newTermName("onComplete")),
            List(handlerTree)
          )
        val block = Block((nakedStats :+ onCompleteTree): _*)
        mkHandlers(c.Expr(block).asInstanceOf[c.Expr[Unit]])(num)
      }
      
      def lastExprTree: c.Tree = {
        assert(awaitable == null)
        if (stats.size == 1)
          c.resetAllAttrs(stats(0).duplicate)
        else {
          val nakedStats = stats.map(stat => c.resetAllAttrs(stat.duplicate)).toList
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
          var handlerExpr = asyncStates(0).handlerForState(1) // state 0 but { case 1 => ... }
          var i = 1
          while (asyncStates(i).awaitable != null) {
            val handlerForNextState = asyncStates(i).handlerForState(i+1)
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
        
        val handlerForLastState: c.Expr[PartialFunction[Int, Unit]] =
          mkHandlers({
            val tree = Apply(Select(Ident("result"), c.universe.newTermName("success")),
                             List(asyncStates(indexOfLastState).lastExprTree))
            c.Expr(tree).asInstanceOf[c.Expr[Unit]]
          })(indexOfLastState + 1)
        
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
