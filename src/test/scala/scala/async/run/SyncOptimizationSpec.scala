package scala.async.run

import org.junit.Test
import scala.async.Async._
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits._

class SyncOptimizationSpec {

  @Test
  def awaitOnCompletedFutureRunsOnSameThread: Unit = {
    val future = async {
      val thread1 = Thread.currentThread
      val f = await(Future.successful(1))
      val thread2 = Thread.currentThread
      assert(thread1 == thread2)
    }
    Await.result(future, 10.seconds)
  }
}
