/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package hygiene

import language.{reflectiveCalls, postfixOps}
import concurrent._
import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4


class HygieneClass {

  import ExecutionContext.Implicits.global

  def m1(x: Int): Future[Int] = future {
    x + 2
  }

  def m2(y: Int) = {
    val state = 23
    val result: Any = "result"
    def resume(): Any = "resume"
    async {
      val f1 = m1(state)
      val x = await(f1)
      val y = await(future(result))
      val z = await(future(resume()))
      (x, y, z)
    }
  }
}

@RunWith(classOf[JUnit4])
class HygieneSpec {

  @Test def `is hygenic`() {
    val o = new HygieneClass
    val fut = o.m2(10)
    val res = Await.result(fut, 2 seconds)
    res._1 mustBe (25)
    res._2 mustBe ("result")
    res._3 mustBe ("resume")
  }
}
