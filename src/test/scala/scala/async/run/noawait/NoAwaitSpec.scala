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
package run
package noawait

import scala.async.internal.AsyncId
import AsyncId._
import org.junit.Test

class NoAwaitSpec {
  @Test
  def `async block without await`(): Unit = {
    def foo = 1
    async {
      foo
      foo
    } mustBe (foo)
  }

  @Test
  def `async block without await 2`(): Unit = {
    async {
      def x = 0
      if (x > 0) 0 else 1
    } mustBe (1)
  }

  @Test
  def `async expr without await`(): Unit = {
    def foo = 1
    async(foo) mustBe (foo)
  }
}
