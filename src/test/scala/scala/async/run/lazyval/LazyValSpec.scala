/*
 * Copyright (C) 2012-2014 Lightbend Inc. <http://www.lightbend.com>
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

