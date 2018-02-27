/*
 * Copyright (C) 2012-2014 Lightbend Inc. <http://www.lightbend.com>
 */

package scala.async
package run
package match0

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Future, ExecutionContext, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.Test
import scala.async.internal.AsyncId


class TestMatchClass {

  import ExecutionContext.Implicits.global

  def m1(x: Int): Future[Int] = Future {
    x + 2
  }

  def m2(y: Int): Future[Int] = async {
    val f = m1(y)
    var z = 0
    y match {
      case 10 =>
        val x1 = await(f)
        z = x1 + 2
      case 20 =>
        val x2 = await(f)
        z = x2 - 2
    }
    z
  }

  def m3(y: Int): Future[Int] = async {
    val f = m1(y)
    var z = 0
    y match {
      case 0 =>
        val x2 = await(f)
        z = x2 - 2
      case 1 =>
        val x1 = await(f)
        z = x1 + 2
    }
    z
  }
}


class MatchSpec {

  @Test def `support await in a simple match expression`() {
    val o = new TestMatchClass
    val fut = o.m2(10) // matches first case
    val res = Await.result(fut, 2 seconds)
    res mustBe (14)
  }

  @Test def `support await in a simple match expression 2`() {
    val o = new TestMatchClass
    val fut = o.m3(1) // matches second case
    val res = Await.result(fut, 2 seconds)
    res mustBe (5)
  }

  @Test def `support await in a match expression with binds`() {
    val result = AsyncId.async {
      val x = 1
      Option(x) match {
        case op @ Some(x) =>
          assert(op.contains(1))
          x + AsyncId.await(x)
        case None => AsyncId.await(0)
      }
    }
    result mustBe (2)
  }

  @Test def `support await referring to pattern matching vals`() {
    import AsyncId.{async, await}
    val result = async {
      val x = 1
      val opt = Some("")
      await(0)
      val o @ Some(y) = opt

      {
        val o @ Some(y) = Some(".")
      }

      await(0)
      await((o, y.isEmpty))
    }
    result mustBe ((Some(""), true))
  }

  @Test def `await in scrutinee`() {
    import AsyncId.{async, await}
    val result = async {
      await(if ("".isEmpty) await(1) else ???) match {
        case x if x < 0 => ???
        case y: Int => y * await(3)
      }
    }
    result mustBe (3)
  }

  @Test def duplicateBindName() {
    import AsyncId.{async, await}
    def m4(m: Any) = async {
      m match {
        case buf: String =>
          await(0)
        case buf: Double =>
          await(2)
      }
    }
    m4("") mustBe 0
  }

  @Test def bugCastBoxedUnitToStringMatch() {
    import scala.async.internal.AsyncId.{async, await}
    def foo = async {
      val p2 = await(5)
      "foo" match {
        case p3: String =>
          p2.toString
      }
    }
    foo mustBe "5"
  }

  @Test def bugCastBoxedUnitToStringIf() {
    import scala.async.internal.AsyncId.{async, await}
    def foo = async {
      val p2 = await(5)
      if (true) p2.toString else p2.toString
    }
    foo mustBe "5"
  }
}
