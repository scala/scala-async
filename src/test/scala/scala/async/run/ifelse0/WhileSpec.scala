package scala.async
package run
package ifelse0

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class WhileSpec {

  @Test
  def whiling1() {
    import AsyncId._

    val result = async {
      var xxx: Int = 0
      var y = 0
      while (xxx < 3) {
        y = await(xxx)
        xxx = xxx + 1
      }
      y
    }
    result mustBe (2)
  }

  @Test
  def whiling2() {
    import AsyncId._

    val result = async {
      var xxx: Int = 0
      var y = 0
      while (false) {
        y = await(xxx)
        xxx = xxx + 1
      }
      y
    }
    result mustBe (0)
  }
}