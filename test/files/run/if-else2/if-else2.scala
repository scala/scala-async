/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

import language.{ reflectiveCalls, postfixOps }
import scala.concurrent.{ Future, ExecutionContext, future, Await }
import scala.concurrent.duration._
import scala.async.Async.{ async, await }

object Test extends App {

  IfElse2Spec.check()

}

class TestIfElse2Class {
  import ExecutionContext.Implicits.global
  
  def base(x: Int): Future[Int] = future {
    Thread.sleep(1000)
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


object IfElse2Spec extends MinimalScalaTest {

  "An async method" should {
    "allow variables of the same name in different blocks" in {
      val o = new TestIfElse2Class
      val fut = o.m(10)
      val res = Await.result(fut, 2 seconds)
      res mustBe(14)
    }
  }

}
