/*
 * Copyright (C) 2012-2014 Lightbend Inc. <http://www.lightbend.com>
 */

package scala.async

import org.junit.Test
import scala.async.internal.AsyncId
import AsyncId._
import tools.reflect.ToolBox

class TreeInterrogation {
  @Test
  def `a minimal set of vals are lifted to vars`(): Unit = {
    val cm = reflect.runtime.currentMirror
    val tb = mkToolbox(s"-cp $toolboxClasspath")
    val tree = tb.parse(
      """| import _root_.scala.async.internal.AsyncId._
        | async {
        |   val x = await(1)
        |   val y = x * 2
        |   def foo(a: Int) = { def nested = 0; a } // don't lift `nested`.
        |   val z = await(x * 3)
        |   foo(z)
        |   z
        | }""".stripMargin)
    val tree1 = tb.typeCheck(tree)

    //println(cm.universe.show(tree1))

    import tb.u._
    val functions = tree1.collect {
      case f: Function => f
      case t: Template => t
    }
    functions.size mustBe 1

    val varDefs = tree1.collect {
      case vd @ ValDef(mods, name, _, _) if mods.hasFlag(Flag.MUTABLE) && vd.symbol.owner.isClass => name
    }
    varDefs.map(_.decoded.trim).toSet.toList.sorted mustStartWith (List("await$async$", "await$async", "state$async"))

    val defDefs = tree1.collect {
      case t: Template =>
        val stats: List[Tree] = t.body
        stats.collect {
          case dd : DefDef
            if !dd.symbol.isImplementationArtifact
              && !dd.symbol.asTerm.isAccessor && !dd.symbol.asTerm.isSetter => dd.name
        }
    }.flatten
    defDefs.map(_.decoded.trim) mustStartWith List("foo$async$", "<init>", "apply", "apply")
  }
}

object TreeInterrogationApp extends App {
  def withDebug[T](t: => T): T = {
    def set(level: String, value: Boolean) = System.setProperty(s"scala.async.$level", value.toString)
    val levels = Seq("trace", "debug")
    def setAll(value: Boolean) = levels.foreach(set(_, value))

    setAll(value = true)
    try t finally setAll(value = false)
  }

  withDebug {
    val cm = reflect.runtime.currentMirror
    val tb = mkToolbox(s"-cp ${toolboxClasspath} -Xprint:typer")
    import scala.async.internal.AsyncId._
    val tree = tb.parse(
      """
        | import scala.async.internal.AsyncId._
        | trait QBound { type D; trait ResultType { case class Inner() }; def toResult: ResultType = ??? }
        | trait QD[Q <: QBound] {
        |     val operation: Q
        |     type D = operation.D
        | }
        |
        |     async {
        |      if (!"".isEmpty) {
        |      val treeResult = null.asInstanceOf[QD[QBound]]
        |      await(0)
        |      val y = treeResult.operation
        |      type RD = treeResult.operation.D
        |      (null: Object) match {
        |        case (_, _: RD) => ???
        |        case _ => val x = y.toResult; x.Inner()
        |      }
        |      await(1)
        |      (y, null.asInstanceOf[RD])
        |      ""
        |      }
        |
        |    }
        |
        | """.stripMargin)
    println(tree)
    val tree1 = tb.typeCheck(tree.duplicate)
    println(cm.universe.show(tree1))

    println(tb.eval(tree))
  }

}
