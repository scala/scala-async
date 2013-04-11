package scala.async
package run
package nesteddef

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class NestedDef {

  @Test
  def nestedDef() {
    import AsyncId._
    val result = async {
      val a = 0
      val x = await(a) - 1
      val local = 43
      def bar(d: Double) = -d + a + local
      def foo(z: Any) = (a.toDouble, bar(x).toDouble, z)
      foo(await(2))
    }
    result mustBe ((0d, 44d, 2))
  }


  @Test
  def nestedFunction() {
    import AsyncId._
    val result = async {
      val a = 0
      val x = await(a) - 1
      val local = 43
      val bar = (d: Double) => -d + a + local
      val foo = (z: Any) => (a.toDouble, bar(x).toDouble, z)
      foo(await(2))
    }
    result mustBe ((0d, 44d, 2))
  }
}
