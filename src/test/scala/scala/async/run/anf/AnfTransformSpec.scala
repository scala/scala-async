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

  def futureUnitIfElse(y: Int): Future[Unit] = async {
    val f = base(y)
    if (y > 0) {
      State.result = await(f) + 2
    } else {
      State.result = await(f) - 2
    }
  }
}

object State {
  @volatile var result: Int = 0
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

  @Test
  def `Unit-typed if-else in tail position`() {
    val o = new AnfTestClass
    val fut = o.futureUnitIfElse(10)
    Await.result(fut, 2 seconds)
    State.result mustBe (14)
  }

  @Test
  def `inlining block does not produce duplicate definition`() {
    import scala.async.AsyncId

    AsyncId.async {
      val f = 12
      val x = AsyncId.await(f)

      {
        type X = Int
        val x: X = 42
        println(x)
      }
      type X = Int
      x: X
    }
  }

  @Test
  def `inlining block in tail position does not produce duplicate definition`() {
    import scala.async.AsyncId

    AsyncId.async {
      val f = 12
      val x = AsyncId.await(f)

      {
        val x = 42
        x
      }
    } mustBe (42)
  }

  @Test
  def `match as expression 1`() {
    import ExecutionContext.Implicits.global
    val result = AsyncId.async {
      val x = "" match {
        case _ => AsyncId.await(1) + 1
      }
      x
    }
    result mustBe (2)
  }

  @Test
  def `match as expression 2`() {
    import ExecutionContext.Implicits.global
    val result = AsyncId.async {
      val x = "" match {
        case "" if false => AsyncId.await(1) + 1
        case _ => 2 + AsyncId.await(1)
      }
      val y = x
      "" match {
        case _ => AsyncId.await(y) + 100
      }
    }
    result mustBe (103)
  }

  @Test
  def nestedAwaitAsBareExpression() {
    import ExecutionContext.Implicits.global
    import _root_.scala.async.AsyncId.{async, await}
    val result = async {
      await(await("").isEmpty)
    }
    result mustBe (true)
  }

  @Test
  def nestedAwaitInBlock() {
    import ExecutionContext.Implicits.global
    import _root_.scala.async.AsyncId.{async, await}
    val result = async {
      ()
      await(await("").isEmpty)
    }
    result mustBe (true)
  }

  @Test
  def nestedAwaitInIf() {
    import ExecutionContext.Implicits.global
    import _root_.scala.async.AsyncId.{async, await}
    val result = async {
      if ("".isEmpty)
        await(await("").isEmpty)
      else 0
    }
    result mustBe (true)
  }
}
