/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package toughtype

import language.{reflectiveCalls, postfixOps}
import scala.concurrent._
import scala.concurrent.duration._
import scala.async.Async._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4


object ToughTypeObject {

  import ExecutionContext.Implicits.global

  class Inner

  def m2 = async[(List[_], ToughTypeObject.Inner)] {
    val y = await(future[List[_]](Nil))
    val z = await(future[Inner](new Inner))
    (y, z)
  }
}

@RunWith(classOf[JUnit4])
class ToughTypeSpec {

  @Test def `propogates tough types`() {
    val fut = ToughTypeObject.m2
    val res: (List[_], scala.async.run.toughtype.ToughTypeObject.Inner) = Await.result(fut, 2 seconds)
    res._1 mustBe (Nil)
  }

  @Test def patternMatchingPartialFunction() {
    import AsyncId.{await, async}
    async {
      await(1)
      val a = await(1)
      val f = { case x => x + a }: PartialFunction[Int, Int]
      await(f(2))
    } mustBe 3
  }

  @Test def patternMatchingPartialFunctionNested() {
    import AsyncId.{await, async}
    async {
      await(1)
      val neg1 = -1
      val a = await(1)
      val f = { case x => ({case x => neg1 * x}: PartialFunction[Int, Int])(x + a) }: PartialFunction[Int, Int]
      await(f(2))
    } mustBe -3
  }

  @Test def patternMatchingFunction() {
    import AsyncId.{await, async}
    async {
      await(1)
      val a = await(1)
      val f = { case x => x + a }: Function[Int, Int]
      await(f(2))
    } mustBe 3
  }

  @Test def existentialBind() {
    import AsyncId.{await, async}
    def m7(a: Any) = async {
      a match {
        case s: Seq[_] =>
          val x = s.size
          var ss = s
          ss = s
          await(x)
      }
    }
    m7(Nil) mustBe 0
  }
}
