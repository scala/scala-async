/*
 * Copyright (C) 2012-2014 Lightbend Inc. <http://www.lightbend.com>
 */

package scala.async
package neg

import org.junit.Test

class NakedAwait {
  @Test
  def `await only allowed in async neg`(): Unit = {
    expectError("`await` must be enclosed in an `async` block") {
      """
        | import _root_.scala.async.Async._
        | await[Any](null)
      """.stripMargin
    }
  }

  @Test
  def `await not allowed in by-name argument`(): Unit = {
    expectError("await must not be used under a by-name argument.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | def foo(a: Int)(b: => Int) = 0
        | async { foo(0)(await(0)) }
      """.stripMargin
    }
  }

  @Test
  def `await not allowed in boolean short circuit argument 1`(): Unit = {
    expectError("await must not be used under a by-name argument.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | async { true && await(false) }
      """.stripMargin
    }
  }

  @Test
  def `await not allowed in boolean short circuit argument 2`(): Unit = {
    expectError("await must not be used under a by-name argument.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | async { true || await(false) }
      """.stripMargin
    }
  }

  @Test
  def nestedObject(): Unit = {
    expectError("await must not be used under a nested object.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | async { object Nested { await(false) } }
      """.stripMargin
    }
  }

  @Test
  def nestedTrait(): Unit = {
    expectError("await must not be used under a nested trait.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | async { trait Nested { await(false) } }
      """.stripMargin
    }
  }

  @Test
  def nestedClass(): Unit = {
    expectError("await must not be used under a nested class.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | async { class Nested { await(false) } }
      """.stripMargin
    }
  }

  @Test
  def nestedFunction(): Unit = {
    expectError("await must not be used under a nested function.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | async { () => { await(false) } }
      """.stripMargin
    }
  }

  @Test
  def nestedPatMatFunction(): Unit = {
    expectError("await must not be used under a nested class.") { // TODO more specific error message
      """
        | import _root_.scala.async.internal.AsyncId._
        | async { { case x => { await(false) } } : PartialFunction[Any, Any] }
      """.stripMargin
    }
  }

  @Test
  def tryBody(): Unit = {
    expectError("await must not be used under a try/catch.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | async { try { await(false) } catch { case _ => } }
      """.stripMargin
    }
  }

  @Test
  def catchBody(): Unit = {
    expectError("await must not be used under a try/catch.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | async { try { () } catch { case _ => await(false) } }
      """.stripMargin
    }
  }

  @Test
  def finallyBody(): Unit = {
    expectError("await must not be used under a try/catch.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | async { try { () } finally { await(false) } }
      """.stripMargin
    }
  }

  @Test
  def guard(): Unit = {
    expectError("await must not be used under a pattern guard.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | async { 1 match { case _ if await(true) => } }
      """.stripMargin
    }
  }

  @Test
  def nestedMethod(): Unit = {
    expectError("await must not be used under a nested method.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | async { def foo = await(false) }
      """.stripMargin
    }
  }

  @Test
  def returnIllegal(): Unit = {
    expectError("return is illegal") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | def foo(): Any = async { return false }
        | ()
        |
        |""".stripMargin
    }
  }

  @Test
  def lazyValIllegal(): Unit = {
    expectError("await must not be used under a lazy val initializer") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | def foo(): Any = async { val x = { lazy val y = await(0); y } }
        | ()
        |
        |""".stripMargin
    }
  }
}
