/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package neg

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class NakedAwait {
  @Test
  def `await only allowed in async neg`() {
    expectError("`await` must be enclosed in an `async` block", "-deprecation -Xfatal-warnings") {
      """
        | import _root_.scala.async.Async._
        | await[Any](null)
      """.stripMargin
    }
  }

  @Test
  def `await not allowed in by-name argument`() {
    expectError("await must not be used under a by-name argument.") {
      """
        | import _root_.scala.async.AsyncId._
        | def foo(a: Int)(b: => Int) = 0
        | async { foo(0)(await(0)) }
      """.stripMargin
    }
  }

  @Test
  def `await not allowed in boolean short circuit argument 1`() {
    expectError("await must not be used under a by-name argument.") {
      """
        | import _root_.scala.async.AsyncId._
        | async { true && await(false) }
      """.stripMargin
    }
  }

  @Test
  def `await not allowed in boolean short circuit argument 2`() {
    expectError("await must not be used under a by-name argument.") {
      """
        | import _root_.scala.async.AsyncId._
        | async { true || await(false) }
      """.stripMargin
    }
  }

  @Test
  def nestedObject() {
    expectError("await must not be used under a nested object.") {
      """
        | import _root_.scala.async.AsyncId._
        | async { object Nested { await(false) } }
      """.stripMargin
    }
  }

  @Test
  def nestedTrait() {
    expectError("await must not be used under a nested trait.") {
      """
        | import _root_.scala.async.AsyncId._
        | async { trait Nested { await(false) } }
      """.stripMargin
    }
  }

  @Test
  def nestedClass() {
    expectError("await must not be used under a nested class.") {
      """
        | import _root_.scala.async.AsyncId._
        | async { class Nested { await(false) } }
      """.stripMargin
    }
  }

  @Test
  def nestedFunction() {
    expectError("await must not be used under a nested function.") {
      """
        | import _root_.scala.async.AsyncId._
        | async { () => { await(false) } }
      """.stripMargin
    }
  }

  @Test
  def tryBody() {
    expectError("await must not be used under a try/catch.") {
      """
        | import _root_.scala.async.AsyncId._
        | async { try { await(false) } catch { case _ => } }
      """.stripMargin
    }
  }

  @Test
  def catchBody() {
    expectError("await must not be used under a try/catch.") {
      """
        | import _root_.scala.async.AsyncId._
        | async { try { () } catch { case _ => await(false) } }
      """.stripMargin
    }
  }

  @Test
  def finallyBody() {
    expectError("await must not be used under a try/catch.") {
      """
        | import _root_.scala.async.AsyncId._
        | async { try { () } finally { await(false) } }
      """.stripMargin
    }
  }

  @Test
  def nestedMethod() {
    expectError("await must not be used under a nested method.") {
      """
        | import _root_.scala.async.AsyncId._
        | async { def foo = await(false) }
      """.stripMargin
    }
  }

  @Test
  def returnIllegal() {
    expectError("return is illegal") {
      """
        | import _root_.scala.async.AsyncId._
        | def foo(): Any = async { return false }
        | ()
        |
        |""".stripMargin
    }
  }

  // TODO Anf transform if to have a simple condition.
  @Test
  def ifCondition() {
    expectError("await must not be used under a condition.") {
      """
        | import _root_.scala.async.AsyncId._
        | async { if (await(true)) () }
        |""".stripMargin
    }
  }
}
