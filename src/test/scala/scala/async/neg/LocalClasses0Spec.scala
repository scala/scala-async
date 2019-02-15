/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.async
package neg

import org.junit.Test
import scala.async.internal.AsyncId

class LocalClasses0Spec {
  @Test
  def localClassCrashIssue16(): Unit = {
    import AsyncId.{async, await}
    async {
      class B { def f = 1 }
      await(new B()).f
    } mustBe 1
  }

  @Test
  def nestedCaseClassAndModuleAllowed(): Unit = {
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
