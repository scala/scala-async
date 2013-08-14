/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package continuations

import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.util.continuations._

/* Specializes `AsyncBaseWithCPSFallback` to always fall back to CPS, yielding a purely CPS-based
 * implementation of async/await.
 */
trait CPSBasedAsyncBase extends AsyncBaseWithCPSFallback {

  override def asyncImpl[T: c.WeakTypeTag](c: Context)
                                          (body: c.Expr[T])
                                          (execContext: c.Expr[futureSystem.ExecContext]): c.Expr[futureSystem.Fut[T]] = {
    super.cpsBasedAsyncImpl[T](c)(body)(execContext)
  }
}
