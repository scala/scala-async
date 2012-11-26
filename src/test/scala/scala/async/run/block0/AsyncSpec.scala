/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package block0

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4


class Test1Class {

  import ExecutionContext.Implicits.global

  def m1(x: Int): Future[Int] = future {
    x + 2
  }

  def m2(y: Int): Future[Int] = async {
    val f = m1(y)
    val x = await(f)
    x + 2
  }

  def m3(y: Int): Future[Int] = async {
    val f1 = m1(y)
    val x1 = await(f1)
    val f2 = m1(y + 2)
    val x2 = await(f2)
    x1 + x2
  }
}


@RunWith(classOf[JUnit4])
class AsyncSpec {

  @Test
  def `simple await`() {
    val o = new Test1Class
    val fut = o.m2(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (14)
  }

  @Test
  def `several awaits in sequence`() {
    val o = new Test1Class
    val fut = o.m3(10)
    val res = Await.result(fut, 4 seconds)
    res mustBe (26)
  }
}
