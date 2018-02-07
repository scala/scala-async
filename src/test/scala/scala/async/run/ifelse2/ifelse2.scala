/*
 * Copyright (C) 2012-2014 Lightbend Inc. <http://www.lightbend.com>
 */

package scala.async
package run
package ifelse2

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.Test


class TestIfElse2Class {

  import ExecutionContext.Implicits.global

  def base(x: Int): Future[Int] = future {
    x + 2
  }

  def m(y: Int): Future[Int] = async {
    val f = base(y)
    var z = 0
    if (y > 0) {
      val x = await(f)
      z = x + 2
    } else {
      val x = await(f)
      z = x - 2
    }
    z
  }
}

class IfElse2Spec {

  @Test
  def `variables of the same name in different blocks`() {
    val o = new TestIfElse2Class
    val fut = o.m(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (14)
  }
}
