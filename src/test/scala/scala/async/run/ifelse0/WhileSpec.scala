/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package ifelse0

import org.junit.Test
import scala.async.internal.AsyncId

class WhileSpec {

  @Test
  def whiling1() {
    import AsyncId._

    val result = async {
      var xxx: Int = 0
      var y = 0
      while (xxx < 3) {
        y = await(xxx)
        xxx = xxx + 1
      }
      y
    }
    result mustBe (2)
  }

  @Test
  def whiling2() {
    import AsyncId._

    val result = async {
      var xxx: Int = 0
      var y = 0
      while (false) {
        y = await(xxx)
        xxx = xxx + 1
      }
      y
    }
    result mustBe (0)
  }

  @Test
  def nestedWhile() {
    import AsyncId._

    val result = async {
      var sum = 0
      var i = 0
      while (i < 5) {
        var j = 0
        while (j < 5) {
          sum += await(i) * await(j)
          j += 1
        }
        i += 1
      }
      sum
    }
    result mustBe (100)
  }

  @Test
  def whileExpr() {
    import AsyncId._

    val result = async {
      var cond = true
      while (cond) {
        cond = false
        await { 22 }
      }
    }
    result mustBe ()
  }

  @Test def doWhile() {
    import AsyncId._
    val result = async {
      var b = 0
      var x = ""
      await(do {
        x += "1"
        x += await("2")
        x += "3"
        b += await(1)
      } while (b < 2))
      await(x)
    }
    result mustBe "123123"
  }

  @Test def whileAwaitCondition() {
    import AsyncId._
    val result = async {
      var b = true
      while(await(b)) {
        b = false
      }
      await(b)
    }
    result mustBe false
  }

  @Test def doWhileAwaitCondition() {
    import AsyncId._
    val result = async {
      var b = true
      do {
        b = false
      } while(await(b))
      b
    }
    result mustBe false
  }
}
