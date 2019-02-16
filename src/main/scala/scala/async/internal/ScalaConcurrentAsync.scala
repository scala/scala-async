/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package async
package internal

import scala.reflect.macros.whitebox
import scala.concurrent.Future

object ScalaConcurrentAsync extends AsyncBase {
  type FS = ScalaConcurrentFutureSystem.type
  val futureSystem: FS = ScalaConcurrentFutureSystem

  override def asyncImpl[T: c.WeakTypeTag](c: whitebox.Context)
                                          (body: c.Expr[T])
                                          (execContext: c.Expr[futureSystem.ExecContext]): c.Expr[Future[T]] = {
    super.asyncImpl[T](c)(body)(execContext)
  }
}
