/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

import language.{ reflectiveCalls, postfixOps }
import scala.concurrent.{ Future, ExecutionContext, future, Await }
import scala.concurrent.duration._
import scala.async.Async.{ async, await }


object Test extends App {

  Block1Spec.check()

}


class Test1Class {
  import ExecutionContext.Implicits.global
  
  def m1(x: Int): Future[Int] = future {
    Thread.sleep(1000)
    x + 2
  }
  
  def m4(y: Int): Future[Int] = async {
    val f1 = m1(y)
    val f2 = m1(y + 2)
    val x1 = await(f1)
    println("between two awaits")
    val x2 = await(f2)
    x1 + x2
  }
}


object Block1Spec extends MinimalScalaTest {

  "An async method" should {
    "support a simple await" in {
      val o = new Test1Class
      val fut = o.m4(10)
      val res = Await.result(fut, 2 seconds)
      res mustBe(26)
    }
  }

}
