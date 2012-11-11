package scala.async
package run
package noawait

import AsyncId._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class NoAwaitSpec {
  @Test
  def `async block without await`() {
    def foo = 1
    async {
      foo
      foo
    } mustBe (foo)
  }

  @Test
  def `async block without await 2`() {
    async {
      def x = 0
      if (x > 0) 0 else 1
    } mustBe (1)
  }

  @Test
  def `async expr without await`() {
    def foo = 1
    async(foo) mustBe (foo)
  }
}
