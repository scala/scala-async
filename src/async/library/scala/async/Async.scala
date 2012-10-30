/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import language.experimental.macros

import scala.reflect.macros.Context
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ Future, Promise }
import scala.util.control.NonFatal

/*
 * @author Philipp Haller
 */
object Async extends AsyncUtils {

  def async[T](body: T): Future[T] = macro asyncImpl[T]
  
  def await[T](awaitable: Future[T]): T = ???
  
  def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[Future[T]] = {
    import c.universe._
    import Flag._
    
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
        
        val handlerForLastState: c.Expr[PartialFunction[Int, Unit]] = {
          val tree = Apply(Select(Ident("result"), c.universe.newTermName("success")),
                           List(asyncBlockBuilder.asyncStates.last.body))
          builder.mkHandler(asyncBlockBuilder.asyncStates.last.state, c.Expr[Unit](tree))
        }
        
        vprintln("GENERATED handler for last state:")
        vprintln(handlerForLastState)
        
        val localVarTrees = asyncBlockBuilder.asyncStates.init.flatMap(_.allVarDefs).toList
        
        val unitIdent = Ident(definitions.UnitClass)
        
        val resumeFunTree: c.Tree = DefDef(Modifiers(), newTermName("resume"), List(), List(List()), unitIdent,
          Try(Apply(Select(
            Apply(Select(handlerExpr.tree, newTermName("orElse")), List(handlerForLastState.tree)),
            newTermName("apply")), List(Ident(newTermName("state")))),
            List(
              CaseDef(
                Apply(Select(Select(Select(Ident(newTermName("scala")), newTermName("util")), newTermName("control")), newTermName("NonFatal")), List(Bind(newTermName("t"), Ident(nme.WILDCARD)))),
                EmptyTree,
                Block(List(
                  Apply(Select(Ident(newTermName("result")), newTermName("failure")), List(Ident(newTermName("t"))))),
                  Literal(Constant(()))))), EmptyTree))
        
        val methodBody = reify {
          val result = Promise[T]()
          var state = 0
          
          /*
          def resume(): Unit = {
            try {
              (handlerExpr.splice orElse handlerForLastState.splice)(state)
            } catch {
              case NonFatal(t) => result.failure(t)
            }
          }
          resume()
          */
          
          c.Expr(Block(
            localVarTrees :+ resumeFunTree,
            Apply(Ident(newTermName("resume")), List())
          )).splice
          
          result.future
        }

        //vprintln("ASYNC: Generated method body:")
        //vprintln(c.universe.showRaw(methodBody))
        //vprintln(c.universe.show(methodBody))
        methodBody

      case _ =>
        // issue error message
        reify {
          sys.error("expression not supported by async")
        }
    }
  }

}
