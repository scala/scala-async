/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package ifelse3

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test


class TestIfElse3Class {

  import ExecutionContext.Implicits.global

  def base(x: Int): Future[Int] = future {
    Thread.sleep(1000)
    x + 2
  }

  def m(y: Int): Future[Int] = async {
    val f = base(y)
    var z = 0
    if (y > 0) {
      val x1 = await(f)
      var w = x1 + 2
      z = w + 2
    } else {
      val x2 = await(f)
      var w = x2 + 2
      z = w - 2
    }
    z
  }
}


@RunWith(classOf[JUnit4])
class IfElse3Spec extends MinimalScalaTest {

  @Test
  def `variables of the same name in different blocks`() {
    val o = new TestIfElse3Class
    val fut = o.m(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (16)
  }
}
