package scala.async

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import AsyncId._

@RunWith(classOf[JUnit4])
class TreeInterrogation {
  @Test
  def `a minimal set of vals are lifted to vars`() {
    val cm = reflect.runtime.currentMirror
    val tb = mkToolbox("-cp target/scala-2.10/classes")
    val tree = tb.parse(
      """| import _root_.scala.async.AsyncId._
        | async {
        |   val x = await(1)
        |   val y = x * 2
        |   val z = await(x * 3)
        |   z
        | }""".stripMargin)
    val tree1 = tb.typeCheck(tree)

    //println(cm.universe.show(tree1))

    import tb.mirror.universe._
    val functions = tree1.collect {
      case f: Function => f
    }
    functions.size mustBe 1

    val varDefs = tree1.collect {
      case ValDef(mods, name, _, _) if mods.hasFlag(Flag.MUTABLE) => name
    }
    varDefs.map(_.decoded).toSet mustBe (Set("state$async", "onCompleteHandler$async", "await$1", "await$2"))
  }

  //@Test
  def sandbox() {
    sys.props("scala.async.debug") = true.toString
    sys.props("scala.async.trace") = false.toString

    val cm = reflect.runtime.currentMirror
    val tb = mkToolbox("-cp target/scala-2.10/classes")
    val tree = tb.parse(
      """ import _root_.scala.async.AsyncId._
        | async {
        |   var sum = 0
        |   var i = 0
        |   while (i < 5) {
        |     var j = 0
        |     while (j < 5) {
        |       sum += await(i) * await(j)
        |       j += 1
        |     }
        |     i += 1
        |   }
        |   sum
        | }
        | """.stripMargin)
    println(tree)
    val tree1 = tb.typeCheck(tree.duplicate)
    println(cm.universe.show(tree1))
    println(tb.eval(tree))
  }
}
