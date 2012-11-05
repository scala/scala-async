package scala.async
package neg

import java.io.File
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import tools.reflect.ToolBoxError

@RunWith(classOf[JUnit4])
class SampleNegSpec extends MinimalScalaTest {
  val f = new File("/Users/jason/code/scala-async/test/files/run/await0")

  @Test
  def `missing symbol`() {
    intercept[ToolBoxError] {
      eval {
        """
          | kaboom
        """.stripMargin
      }
    }.getMessage mustContain "not found: value kaboom"

  }
}
