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
}
