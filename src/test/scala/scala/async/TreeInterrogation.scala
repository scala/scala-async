/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import AsyncId._
import tools.reflect.ToolBox

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
      case ValDef(mods, name, _, _) if mods.hasFlag(Flag.MUTABLE) => name
    }
    varDefs.map(_.decoded.trim).toSet mustBe (Set("state$async", "await$1", "await$2"))
    varDefs.map(_.decoded.trim).toSet mustBe (Set("state$async", "await$1", "await$2"))

    val defDefs = tree1.collect {
      case t: Template =>
        val stats: List[Tree] = t.body
        stats.collect {
          case dd : DefDef
            if !dd.symbol.isImplementationArtifact
              && !dd.symbol.asTerm.isAccessor && !dd.symbol.asTerm.isSetter => dd.name
        }
    }.flatten
    defDefs.map(_.decoded.trim).toSet mustBe (Set("foo$1", "apply", "resume$async", "<init>"))
  }
}

object TreeInterrogation extends App {
  def withDebug[T](t: => T) {
    AsyncUtils.trace = true
    AsyncUtils.verbose = true
    try t
    finally {
      AsyncUtils.trace = false
      AsyncUtils.verbose = false
    }
  }

  withDebug {
    val cm = reflect.runtime.currentMirror
    val tb = mkToolbox("-cp target/scala-2.10/classes -Xprint:all")
    val tree = tb.parse(
      """ import _root_.scala.async.AsyncId._
        | val state = 23
        | val result: Any = "result"
        | def resume(): Any = "resume"
        | val res = async {
        |   val f1 = async { state + 2 }
        |   val x  = await(f1)
        |   val y  = await(async { result })
        |   val z  = await(async { resume() })
        |   (x, y, z)
        | }
        | ()
        | """.stripMargin)
    println(tree)
    val tree1 = tb.typeCheck(tree.duplicate)
    println(cm.universe.show(tree1))
    println(tb.eval(tree))
  }
}
