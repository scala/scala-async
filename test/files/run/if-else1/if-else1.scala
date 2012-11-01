/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

import language.{ reflectiveCalls, postfixOps }
import scala.concurrent.{ Future, ExecutionContext, future, Await }
import scala.concurrent.duration._
import scala.async.Async.{ async, await }

object Test extends App {

  IfElse1Spec.check()

}

class TestIfElse1Class {
  import ExecutionContext.Implicits.global
  
  def base(x: Int): Future[Int] = future {
    Thread.sleep(1000)
    x + 2
  }
  
  def m1(y: Int): Future[Int] = async {
    val f = base(y)
    var z = 0
    if (y > 0) {
      if (y > 100)
        5
      else {
        val x1 = await(f)
        z = x1 + 2
      }
    } else {
      val x2 = await(f)
      z = x2 - 2
    }
    z
  }
  
  def m2(y: Int): Future[Int] = async {
    val f = base(y)
    var z = 0
    if (y > 0) {
      if (y < 100) {
        val x1 = await(f)
        z = x1 + 2
      }
      else
        5
    } else {
      val x2 = await(f)
      z = x2 - 2
    }
    z
  }
  
  def m3(y: Int): Future[Int] = async {
    val f = base(y)
    var z = 0
    if (y < 0) {
      val x2 = await(f)
      z = x2 - 2
    } else {
      if (y > 100)
        5
      else {
        val x1 = await(f)
        z = x1 + 2
      }
    }
    z
  }
  
  def m4(y: Int): Future[Int] = async {
    val f = base(y)
    var z = 0
    if (y < 0) {
      val x2 = await(f)
      z = x2 - 2
    } else {
      if (y < 100) {
        val x1 = await(f)
        z = x1 + 2
      } else
        5
    }
    z
  }

}


object IfElse1Spec extends MinimalScalaTest {

  "An async method" should {
    "support await in a nested if-else expression" in {
      val o = new TestIfElse1Class
      val fut = o.m1(10)
      val res = Await.result(fut, 2 seconds)
      res mustBe(14)
    }
  }

  "An async method" should {
    "support await in a nested if-else expression" in {
      val o = new TestIfElse1Class
      val fut = o.m2(10)
      val res = Await.result(fut, 2 seconds)
      res mustBe(14)
    }
  }

  "An async method" should {
    "support await in a nested if-else expression" in {
      val o = new TestIfElse1Class
      val fut = o.m3(10)
      val res = Await.result(fut, 2 seconds)
      res mustBe(14)
    }
  }

  "An async method" should {
    "support await in a nested if-else expression" in {
      val o = new TestIfElse1Class
      val fut = o.m4(10)
      val res = Await.result(fut, 2 seconds)
      res mustBe(14)
    }
  }

}
