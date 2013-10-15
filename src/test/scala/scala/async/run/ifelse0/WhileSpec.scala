/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
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
}
