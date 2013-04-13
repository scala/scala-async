/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package continuations

import scala.util.continuations._
import scala.concurrent.{Future, Promise, ExecutionContext}

trait ScalaConcurrentCPSFallback {
  self: AsyncBaseWithCPSFallback =>

  import ExecutionContext.Implicits.global

  lazy val futureSystem = ScalaConcurrentFutureSystem
  type FS = ScalaConcurrentFutureSystem.type

  /* Fall-back for `await` when it is called at an unsupported position.
   */
  override def awaitFallback[T](awaitable: futureSystem.Fut[T]): T @cps[Future[Any]] =
    shift {
      (k: (T => Future[Any])) =>
        val fr = Promise[Any]()
        awaitable onComplete {
          case tr => fr completeWith k(tr.get)
        }
        fr.future
    }

}
