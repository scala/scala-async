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
}
