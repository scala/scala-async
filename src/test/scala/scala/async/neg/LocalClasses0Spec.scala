/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package neg

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class LocalClasses0Spec {
  @Test
  def localClassCrashIssue16() {
    import scala.async.AsyncId.{async, await}
    async {
      class B { def f = 1 }
      await(new B()).f
    } mustBe 1
  }

  @Test
  def nestedCaseClassAndModuleAllowed() {
    import AsyncId.{await, async}
    async {
      trait Base { def base = 0}
      await(0)
      case class Person(name: String) extends Base
      val fut = async { "bob" }
      val x = Person(await(fut))
      x.base
      x.name
    } mustBe "bob"
  }
}
