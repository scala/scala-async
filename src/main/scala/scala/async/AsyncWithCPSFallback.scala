package scala.async

import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.util.continuations._

object AsyncWithCPSFallback extends AsyncBase {

  import scala.concurrent.{Future, ExecutionContext}
  import ExecutionContext.Implicits.global

  lazy val futureSystem = ScalaConcurrentFutureSystem
  type FS = ScalaConcurrentFutureSystem.type

  /* Fall-back for `await` when it is called at an unsupported position.
   */
  override def awaitFallback[T, U](awaitable: futureSystem.Fut[T], p: futureSystem.Prom[U]): T @cpsParam[U, Unit] =
    shift {
      (k: (T => U)) =>
        awaitable onComplete {
          case tr => p.success(k(tr.get))
        }
    }

  override def fallbackEnabled = true

  def async[T](body: T) = macro asyncImpl[T]

  override def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[Future[T]] = super.asyncImpl[T](c)(body)
}
