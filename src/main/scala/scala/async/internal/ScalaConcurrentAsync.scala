package scala
package async
package internal

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.concurrent.Future

object ScalaConcurrentAsync extends AsyncBase {
  type FS = ScalaConcurrentFutureSystem.type
  val futureSystem: FS = ScalaConcurrentFutureSystem

  override def asyncImpl[T: c.WeakTypeTag](c: Context)
                                          (body: c.Expr[T])
                                          (execContext: c.Expr[futureSystem.ExecContext]): c.Expr[Future[T]] = {
    super.asyncImpl[T](c)(body)(execContext)
  }
}
