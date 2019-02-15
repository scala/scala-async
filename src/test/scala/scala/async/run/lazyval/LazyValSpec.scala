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
package lazyval

import org.junit.Test
import scala.async.internal.AsyncId._

class LazyValSpec {
  @Test
  def lazyValAllowed(): Unit = {
    val result = async {
      var x = 0
      lazy val y = { x += 1; 42 }
      assert(x == 0, x)
      val z = await(1)
      val result = y + x
      assert(x == 1, x)
      identity(y)
      assert(x == 1, x)
      result
    }
    result mustBe 43
  }
}

