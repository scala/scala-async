/*
 * Copyright (C) 2012-2013 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package live

import org.junit.Test

import internal.AsyncTestLV
import AsyncTestLV._

class LiveVariablesSpec {

  @Test
  def liveVars1() {
    val f = async { 1 }

    def m1(x: Int): Int =
      async { x + 1 }

    def m2(x: Int): String =
      async { x.toString }

    def m3() = async {
      val a = await(f)          // await$1$1
      // a == 1
      val b = await(m1(a))      // await$2$1
      // b == 2
      assert(AsyncTestLV.log.exists(_ == ("await$1$1" -> 0)))
      val res = await(m2(b))    // await$3$1
      assert(AsyncTestLV.log.exists(_ == ("await$2$1" -> 0)))
      res
    }

    assert(m3() == "2")
  }

}
