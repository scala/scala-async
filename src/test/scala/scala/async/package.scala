/*
 * Copyright (C) 2012-2014 Lightbend Inc. <http://www.lightbend.com>
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

    def mustStartWith(prefix: String) = assert(text startsWith prefix, text)
  }

  implicit class listops(list: List[String]) {
    def mustStartWith(prefixes: List[String]) = {
      assert(list.length == prefixes.size, ("expected = " + prefixes.length + ", actual = " + list.length, list))
      list.zip(prefixes).foreach{ case (el, prefix) => el mustStartWith prefix }
    }
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

  import scala.tools.nsc._, reporters._
  def mkGlobal(compileOptions: String = ""): Global = {
    val settings = new Settings()
    settings.processArgumentString(compileOptions)
    val initClassPath = settings.classpath.value
    settings.embeddedDefaults(getClass.getClassLoader)
    if (initClassPath == settings.classpath.value)
      settings.usejavacp.value = true // not running under SBT, try to use the Java claspath instead
    val reporter = new StoreReporter
    new Global(settings, reporter)
  }

  def toolboxClasspath = BuildInfo.classDirectory

  def expectError(errorSnippet: String, compileOptions: String = "",
                  baseCompileOptions: String = s"-cp ${toolboxClasspath}")(code: String) {
    intercept[ToolBoxError] {
      eval(code, compileOptions + " " + baseCompileOptions)
    }.getMessage mustContain errorSnippet
  }
}
