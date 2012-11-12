/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

import language.{ reflectiveCalls, postfixOps }
import scala.concurrent.{ Future, ExecutionContext, future, Await, Promise }
import scala.concurrent.duration._
import scala.async.EndTaskException
import scala.async.Async.{ async, await, awaitCps }
import scala.util.continuations.reset

object TestManual extends App {

  Fallback0ManualSpec.check()

}

class TestFallback0ManualClass {
  import ExecutionContext.Implicits.global
  
  def m1(x: Int): Future[Int] = future {
    x + 2
  }
  
  def m2(y: Int): Future[Int] = {
    val p = Promise[Int]()
    future { reset {
      val f = m1(y)
      var z = 0
      val res = awaitCps(f, p) + 5
      if (res > 0) {
        z = 2
      } else {
        z = 4
      }
      z
    } }
    p.future
  }
  
  /* that isn't even supported by current CPS plugin
  def m3(y: Int): Future[Int] = {
    val p = Promise[Int]()
    future { reset {
      val f = m1(y)
      var z = 0
      val res: Option[Int] = Some(5)
      res match {
        case None    => z = 4
        case Some(a) => z = awaitCps(f, p) - 10
      }
      z
    } }
    p.future
  }
  */
}


object Fallback0ManualSpec extends MinimalScalaTest {

  "An async method" should {
    "support await in a simple if-else expression" in {
      val o = new TestFallback0ManualClass
      val fut = o.m2(10)
      val res = Await.result(fut, 2 seconds)
      res mustBe(2)
    }
  }

}
