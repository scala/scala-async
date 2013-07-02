/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package continuations

import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.concurrent.{ExecutionContext, Future}

trait CPSBasedAsync extends CPSBasedAsyncBase with ScalaConcurrentCPSFallback

object CPSBasedAsync extends CPSBasedAsync {

  def async[T](body: T)(implicit execContext: ExecutionContext): Future[T] = macro asyncImpl[T]

  override def asyncImpl[T: c.WeakTypeTag](c: Context)
                                          (body: c.Expr[T])
                                          (execContext: c.Expr[ExecutionContext]): c.Expr[Future[T]] = {
    super.asyncImpl[T](c)(body)(execContext)
  }
}
