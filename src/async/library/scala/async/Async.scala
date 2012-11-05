/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import language.experimental.macros

import scala.reflect.macros.Context
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ Future, Promise, ExecutionContext, future }
import ExecutionContext.Implicits.global
import scala.util.control.NonFatal
import scala.util.continuations.{ shift, reset, cpsParam }

/* Extending `ControlThrowable`, by default, also avoids filling in the stack trace. */
class FallbackToCpsException extends scala.util.control.ControlThrowable

/*
 * @author Philipp Haller
 */
object Async extends AsyncUtils {

  def async[T](body: T): Future[T] = macro asyncImpl[T]
  
  def await[T](awaitable: Future[T]): T = ???
  
  /* Fall back for `await` when it is called at an unsupported position.
   */
  def awaitCps[T, U](awaitable: Future[T], p: Promise[U]): T @cpsParam[U, Unit] =
    shift {
      (k: (T => U)) =>
        awaitable onComplete {
          case tr => p.success(k(tr.get))
        }
    }
  
  def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[Future[T]] = {
    import c.universe._
    import Flag._
    
    val builder = new ExprBuilder[c.type](c)
    val awaitMethod = awaitSym(c)
    
    try {
      body.tree match {
        case Block(stats, expr) =>
          val asyncBlockBuilder = new builder.AsyncBlockBuilder(stats, expr, 0, 1000, 1000, Map())

          vprintln(s"states of current method:")
          asyncBlockBuilder.asyncStates foreach vprintln

          val handlerExpr = asyncBlockBuilder.mkCombinedHandlerExpr()

          vprintln(s"GENERATED handler expr:")
          vprintln(handlerExpr)

          val handlerForLastState: c.Expr[PartialFunction[Int, Unit]] = {
            val tree = Apply(Select(Ident("result"), newTermName("success")),
              List(asyncBlockBuilder.asyncStates.last.body))
            builder.mkHandler(asyncBlockBuilder.asyncStates.last.state, c.Expr[Unit](tree))
          }

          vprintln("GENERATED handler for last state:")
          vprintln(handlerForLastState)

          val localVarTrees = asyncBlockBuilder.asyncStates.init.flatMap(_.allVarDefs).toList
          
          /*
            def resume(): Unit = {
              try {
                (handlerExpr.splice orElse handlerForLastState.splice)(state)
              } catch {
                case NonFatal(t) => result.failure(t)
              }
            }
           */
          val resumeFunTree: c.Tree = DefDef(Modifiers(), newTermName("resume"), List(), List(List()), Ident(definitions.UnitClass),
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
          
          reify {
            val result = Promise[T]()
            var state = 0
            future {
              c.Expr(Block(
                localVarTrees :+ resumeFunTree,
                Apply(Ident(newTermName("resume")), List()))).splice
            }
            result.future
          }

        case _ =>
          // issue error message
          reify {
            sys.error("expression not supported by async")
          }
      }
    } catch {
      case _: FallbackToCpsException =>
        // replace `await` invocations with `awaitCps` invocations
        val awaitReplacer = new Transformer {
          val awaitCpsMethod = awaitCpsSym(c)
          override def transform(tree: Tree): Tree = tree match {
            case Apply(fun @ TypeApply(_, List(futArgTpt)), args) if fun.symbol == awaitMethod =>
              val typeApp = treeCopy.TypeApply(fun, Ident(awaitCpsMethod), List(TypeTree(futArgTpt.tpe), TypeTree(body.tree.tpe)))
              treeCopy.Apply(tree, typeApp, args.map(arg => c.resetAllAttrs(arg.duplicate)) :+ Ident(newTermName("p")))
              
            case _ =>
              super.transform(tree)
          }
        }
        
        val newBody = awaitReplacer.transform(body.tree)
        
        reify {
          val p = Promise[T]()
          future {
            reset {
              c.Expr(c.resetAllAttrs(newBody.duplicate)).asInstanceOf[c.Expr[T]].splice
            }
          }
          p.future
        }
    }
  }

}
