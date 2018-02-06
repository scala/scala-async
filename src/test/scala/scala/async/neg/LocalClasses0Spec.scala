/*
 * Copyright (C) 2012-2014 Lightbend Inc. <http://www.lightbend.com>
 */

package scala.async
package neg

import org.junit.Test
import scala.async.internal.AsyncId

class LocalClasses0Spec {
  @Test
  def localClassCrashIssue16() {
    import AsyncId.{async, await}
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
