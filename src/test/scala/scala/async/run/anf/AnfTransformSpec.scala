/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package anf

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4


class AnfTestClass {

  import ExecutionContext.Implicits.global

  def base(x: Int): Future[Int] = future {
    x + 2
  }

  def m(y: Int): Future[Int] = async {
    val f = base(y)
    await(f)
  }

  def m2(y: Int): Future[Int] = async {
    val f = base(y)
    val f2 = base(y + 1)
    await(f) + await(f2)
  }

  def m3(y: Int): Future[Int] = async {
    val f = base(y)
    var z = 0
    if (y > 0) {
      z = await(f) + 2
    } else {
      z = await(f) - 2
    }
    z
  }

  def m4(y: Int): Future[Int] = async {
    val f = base(y)
    val z = if (y > 0) {
      await(f) + 2
    } else {
      await(f) - 2
    }
    z + 1
  }
}


@RunWith(classOf[JUnit4])
class AnfTransformSpec {

  @Test
  def `simple ANF transform`() {
    val o = new AnfTestClass
    val fut = o.m(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (12)
  }

  @Test
  def `simple ANF transform 2`() {
    val o = new AnfTestClass
    val fut = o.m2(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (25)
  }

  @Test
  def `simple ANF transform 3`() {
    val o = new AnfTestClass
    val fut = o.m3(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (14)
  }

  @Test
  def `ANF transform of assigning the result of an if-else`() {
    val o = new AnfTestClass
    val fut = o.m4(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (15)
  }
}
