/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package ifelse0

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test


class TestIfElseClass {

  import ExecutionContext.Implicits.global

  def m1(x: Int): Future[Int] = future {
    Thread.sleep(1000)
    x + 2
  }

  def m2(y: Int): Future[Int] = async {
    val f = m1(y)
    var z = 0
    if (y > 0) {
      val x1 = await(f)
      z = x1 + 2
    } else {
      val x2 = await(f)
      z = x2 - 2
    }
    z
  }
}


@RunWith(classOf[JUnit4])
class IfElseSpec {

  @Test def `support await in a simple if-else expression`() {
    val o = new TestIfElseClass
    val fut = o.m2(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (14)
  }
}
