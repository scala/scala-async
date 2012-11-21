package scala.async

import language.reflectiveCalls
import language.postfixOps
import language.implicitConversions

import scala.reflect.{ClassTag, classTag}

import scala.collection.mutable
import scala.concurrent.{Future, Awaitable, CanAwait}
import java.util.concurrent.{TimeoutException, CountDownLatch, TimeUnit}
import scala.concurrent.duration.Duration
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import async._
import tools.reflect.ToolBoxError


trait TestUtils {

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

  def mkToolbox(compileOptions: String = "") = {
    val m = scala.reflect.runtime.currentMirror
    import scala.tools.reflect.ToolBox
    m.mkToolBox(options = compileOptions)
  }

  def expectError(errorSnippet: String, compileOptions: String = "", baseCompileOptions: String = "-cp target/scala-2.10/classes")(code: String) {
    intercept[ToolBoxError] {
      eval(code, compileOptions + " " + baseCompileOptions)
    }.getMessage mustContain errorSnippet
  }
}
