/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.concurrent.{Future, ExecutionContext}
import scala.async.internal.{AsyncBase, ScalaConcurrentFutureSystem}

object Async extends AsyncBase {
  type FS = ScalaConcurrentFutureSystem.type
  val futureSystem: FS = ScalaConcurrentFutureSystem

  def async[T](body: T)(implicit execContext: ExecutionContext): Future[T] = macro asyncImpl[T]

  override def asyncImpl[T: c.WeakTypeTag](c: Context)
                                          (body: c.Expr[T])
                                          (execContext: c.Expr[futureSystem.ExecContext]): c.Expr[Future[T]] = {
    super.asyncImpl[T](c)(body)(execContext)
  }
}
