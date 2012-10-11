/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import language.{ reflectiveCalls, postfixOps }
import scala.concurrent.{ Future, ExecutionContext, future, Await }
import scala.concurrent.duration._
import scala.async.Async.{ async, await }


object Test extends App {

  AsyncSpec.check()

}


class Test1Class {
  import ExecutionContext.Implicits.global
  
  def m1(x: Int): Future[Int] = future {
    Thread.sleep(2000)
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

  // currently fails with: error: not found: value f2
/*
  def m4(y: Int): Future[Int] = async {
    val f1 = m1(y)
    val f2 = m1(y + 2)
    val x1 = await(f1)
    println("between two awaits")
    val x2 = await(f2)
    x1 + x2
  }
*/
}


object AsyncSpec extends MinimalScalaTest {

  "An async method" should {
    "support a simple await" in {
      val o = new Test1Class
      val fut = o.m2(10)
      val res = Await.result(fut, 2 seconds)
      res mustBe(14)
    }
  }
  
  "An async method" should {
    "support several awaits in sequence" in {
      val o = new Test1Class
      val fut = o.m3(10)
      val res = Await.result(fut, 10 seconds)
      res mustBe(26)
    }
  }

}
