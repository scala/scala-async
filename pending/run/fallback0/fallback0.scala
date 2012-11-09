/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

import language.{ reflectiveCalls, postfixOps }
import scala.concurrent.{ Future, ExecutionContext, future, Await }
import scala.concurrent.duration._
import scala.async.Async.{ async, await, awaitCps }

object Test extends App {

  Fallback0Spec.check()

}

class TestFallback0Class {
  import ExecutionContext.Implicits.global
  
  def m1(x: Int): Future[Int] = future {
    x + 2
  }
  
  def m2(y: Int): Future[Int] = async {
    val f = m1(y)
    var z = 0
    val res = await(f) + 5
    if (res > 0) {
      z = 2
    } else {
      z = 4
    }
    z
  }
}


object Fallback0Spec extends MinimalScalaTest {

  "An async method" should {
    "support await in a simple if-else expression" in {
      val o = new TestFallback0Class
      val fut = o.m2(10)
      val res = Await.result(fut, 2 seconds)
      res mustBe(2)
    }
  }

}
