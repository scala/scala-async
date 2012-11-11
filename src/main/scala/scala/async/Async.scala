/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import language.experimental.macros

import scala.reflect.macros.Context
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Promise, ExecutionContext, future}
import ExecutionContext.Implicits.global
import scala.util.control.NonFatal
import AsyncUtils.vprintln


/*
 * @author Philipp Haller
 */
object Async extends AsyncBase {
  lazy val futureSystem = ScalaConcurrentFutureSystem
  type FS = ScalaConcurrentFutureSystem.type

  def async[T](body: T) = macro asyncImpl[T]

  override def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[Future[T]] = super.asyncImpl[T](c)(body)
}

object AsyncId extends AsyncBase {
  lazy val futureSystem = IdentityFutureSystem
  type FS = IdentityFutureSystem.type

  def async[T](body: T) = macro asyncImpl[T]

  override def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[T] = super.asyncImpl[T](c)(body)
}

/**
 * A base class for the `async` macro. Subclasses must provide:
 *
 * - Concrete types for a given future system
 * - Tree manipulations to create and complete the equivalent of Future and Promise
 * in that system.
 * - The `async` macro declaration itself, and a forwarder for the macro implementation.
 * (The latter is temporarily needed to workaround a bug in the macro system)
 *
 * The default implementation, [[scala.async.Async]], binds the macro to `scala.concurrent._`.
 */
abstract class AsyncBase {
  self =>

  type FS <: FutureSystem
  val futureSystem: FS

  /**
   * A call to `await` must be nested in an enclosing `async` block.
   *
   * A call to await does not block the thread, rather it is a delimiter
   * used by the enclosing `async` macro. Code following the `await`
   * call.
   *
   * @param awaitable The future from which a value is awaited
   * @tparam T        The type of that value
   * @return          The value
   */
  // TODO Replace with `@compileTimeOnly when this is implemented SI-6539
  @deprecated("`await` must be enclosed in an `async` block", "0.1")
  def await[T](awaitable: futureSystem.Fut[T]): T = ???

  def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[futureSystem.Fut[T]] = {
    import c.universe._
    import Flag._

    val builder = new ExprBuilder[c.type, futureSystem.type](c, self.futureSystem)

    import builder.defn._
    import builder.name
    import builder.futureSystemOps

    body.tree match {
      case Block(stats, expr) =>
        val asyncBlockBuilder = new builder.AsyncBlockBuilder(stats, expr, 0, 1000, 1000, Map())

        asyncBlockBuilder.asyncStates foreach vprintln

        val handlerCases: List[CaseDef] = asyncBlockBuilder.mkCombinedHandlerCases[T]()

        val localVarTrees = asyncBlockBuilder.asyncStates.init.flatMap(_.allVarDefs).toList

        /*
          def resume(): Unit = {
            try {
              state match { case 0 => ... ; ... }
            } catch {
              case NonFatal(t) => result.failure(t)
            }
          }
         */
        val nonFatalModule = builder.defn.NonFatalClass
        val resumeFunTree: c.Tree = DefDef(Modifiers(), name.resume, Nil, List(Nil), Ident(definitions.UnitClass),
          Try(
            Match(Ident(name.state), handlerCases),
            List(
              CaseDef(
                Apply(Ident(nonFatalModule), List(Bind(name.tr, Ident(nme.WILDCARD)))),
                EmptyTree,
                Block(List({
                  val t = c.Expr[Throwable](Ident(name.tr))
                  futureSystemOps.completeProm[T](c.Expr[futureSystem.Prom[T]](Ident(name.result)), reify(scala.util.Failure(t.splice))).tree
                }), c.literalUnit.tree))), EmptyTree))


        val prom: Expr[futureSystem.Prom[T]] = reify {
          val result$async = futureSystemOps.createProm[T].splice
          var state$async = 0
          futureSystemOps.future[Unit] {
            c.Expr[Unit](Block(
              localVarTrees :+ resumeFunTree,
              Apply(Ident(name.resume), Nil)))
          }(futureSystemOps.execContext).splice
          result$async
        }
        val result = futureSystemOps.promiseToFuture(prom)
        vprintln(s"${c.macroApplication} \nexpands to:\n ${result.tree}")
        result

      case tree =>
        c.abort(c.macroApplication.pos, s"expression not supported by async: ${tree}")
    }
  }
}
