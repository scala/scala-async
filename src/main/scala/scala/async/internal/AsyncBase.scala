/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async.internal

import scala.reflect.internal.annotations.compileTimeOnly
import scala.reflect.macros.Context

/**
 * A base class for the `async` macro. Subclasses must provide:
 *
 * - Concrete types for a given future system
 * - Tree manipulations to create and complete the equivalent of Future and Promise
 * in that system.
 * - The `async` macro declaration itself, and a forwarder for the macro implementation.
 * (The latter is temporarily needed to workaround bug SI-6650 in the macro system)
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
   * A call to `await` does not block the current thread, rather it is a delimiter
   * used by the enclosing `async` macro. Code following the `await`
   * call is executed asynchronously, when the argument of `await` has been completed.
   *
   * @param awaitable the future from which a value is awaited.
   * @tparam T        the type of that value.
   * @return          the value.
   */
  @compileTimeOnly("`await` must be enclosed in an `async` block")
  def await[T](awaitable: futureSystem.Fut[T]): T = ???

  protected[async] def fallbackEnabled = false

  def asyncImpl[T: c.WeakTypeTag](c: Context)
                                 (body: c.Expr[T])
                                 (execContext: c.Expr[futureSystem.ExecContext]): c.Expr[futureSystem.Fut[T]] = {
    import c.universe._

    val asyncMacro = AsyncMacro(c, futureSystem)

    val code = asyncMacro.asyncTransform[T](
      body.tree.asInstanceOf[asyncMacro.global.Tree],
      execContext.tree.asInstanceOf[asyncMacro.global.Tree],
      fallbackEnabled)(implicitly[c.WeakTypeTag[T]].asInstanceOf[asyncMacro.global.WeakTypeTag[T]])

    AsyncUtils.vprintln(s"async state machine transform expands to:\n ${code}")
    c.Expr[futureSystem.Fut[T]](code.asInstanceOf[Tree])
  }
}
