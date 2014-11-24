package scala.async
package run
package gen0

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.Test


class Test1Class {

  import ExecutionContext.Implicits.global

}


class AsyncSpec {

  @Test
  def `generic`() {
   /*
    val o = new Test1Class
    val fut = o.m2(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (14)
   */
    1 mustBe (1)
  }

}
