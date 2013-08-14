/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala

import reflect._
import tools.reflect.{ToolBox, ToolBoxError}

package object async {


  implicit class objectops(obj: Any) {
    def mustBe(other: Any) = assert(obj == other, obj + " is not " + other)

    def mustEqual(other: Any) = mustBe(other)
  }

  implicit class stringops(text: String) {
    def mustContain(substring: String) = assert(text contains substring, text)
  }

  def intercept[T <: Throwable : ClassTag](body: => Any): T = {
    try {
      body
      throw new Exception(s"Exception of type ${classTag[T]} was not thrown")
    } catch {
      case t: Throwable =>
        if (classTag[T].runtimeClass != t.getClass) throw t
        else t.asInstanceOf[T]
    }
  }

  def eval(code: String, compileOptions: String = ""): Any = {
    val tb = mkToolbox(compileOptions)
    tb.eval(tb.parse(code))
  }

  def mkToolbox(compileOptions: String = ""): ToolBox[_ <: scala.reflect.api.Universe] = {
    val m = scala.reflect.runtime.currentMirror
    import scala.tools.reflect.ToolBox
    m.mkToolBox(options = compileOptions)
  }

  def scalaBinaryVersion: String = {
    val Pattern = """(\d+\.\d+)\..*""".r
    scala.util.Properties.versionNumberString match {
      case Pattern(v) => v
      case _          => ""
    }
  }

  def toolboxClasspath = {
    val f = new java.io.File(s"target/scala-${scalaBinaryVersion}/classes")
    if (!f.exists) sys.error(s"output directory ${f.getAbsolutePath} does not exist.")
    f.getAbsolutePath
  }

  def expectError(errorSnippet: String, compileOptions: String = "",
                  baseCompileOptions: String = s"-cp ${toolboxClasspath}")(code: String) {
    intercept[ToolBoxError] {
      eval(code, compileOptions + " " + baseCompileOptions)
    }.getMessage mustContain errorSnippet
  }
}
