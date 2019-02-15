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
package hygiene

import org.junit.Test
import scala.async.internal.AsyncId

class HygieneSpec {

  import AsyncId.{async, await}

  @Test
  def `is hygenic`(): Unit = {
    val state = 23
    val result: Any = "result"
    def resume(): Any = "resume"
    val res = async {
      val f1 = state + 2
      val x  = await(f1)
      val y  = await(result)
      val z  = await(resume())
      (x, y, z)
    }
    res mustBe ((25, "result", "resume"))
  }

  @Test
  def `external var as result of await`(): Unit = {
    var ext = 0
    async {
      ext = await(12)
    }
    ext mustBe (12)
  }

  @Test
  def `external var as result of await 2`(): Unit = {
    var ext = 0
    val inp = 10
    async {
      if (inp > 0)
        ext = await(12)
      else
        ext = await(10)
    }
    ext mustBe (12)
  }

  @Test
  def `external var as result of await 3`(): Unit = {
    var ext = 0
    val inp = 10
    async {
      val x = if (inp > 0)
        await(12)
      else
        await(10)
      ext = x + await(2)
    }
    ext mustBe (14)
  }

  @Test
  def `is hygenic nested`(): Unit = {
    val state = 23
    val result: Any = "result"
    def resume(): Any = "resume"
    import AsyncId.{await, async}
    val res = async {
      val f1 = async { state + 2 }
      val x  = await(f1)
      val y  = await(async { result })
      val z  = await(async(await(async { resume() })))
      (x, y, z)
    }
    res._1 mustBe (25)
    res._2 mustBe ("result")
    res._3 mustBe ("resume")
  }
}
