package scala.async

package object neg {
  def eval(code: String): Any = {
    val m = scala.reflect.runtime.currentMirror
    import scala.tools.reflect.ToolBox
    val tb = m.mkToolBox()
    val result = tb.eval(tb.parse(code))
    result
  }
}
