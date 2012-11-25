/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package hygiene

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class HygieneSpec {

  import scala.async.AsyncId.{async, await}

  @Test
  def `is hygenic`() {
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

/* TODO:
[error] /Users/phaller/git/async/src/test/scala/scala/async/run/hygiene/Hygiene.scala:52: not found: value tr$1
[error]       val f1 = async { state + 2 }
[error]                      ^
  @Test
  def `is hygenic`() {
    val state = 23
    val result: Any = "result"
    def resume(): Any = "resume"
    val res = async {
      val f1 = async { state + 2 }
      val x  = await(f1)
      val y  = await(async { result })
      val z  = await(async { resume() })
      (x, y, z)
    }
    res._1 mustBe (25)
    res._2 mustBe ("result")
    res._3 mustBe ("resume")
  }
*/

  @Test
  def `external var as result of await`() {
    var ext = 0
    async {
      ext = await(12)
    }
    ext mustBe (12)
  }

  @Test
  def `external var as result of await 2`() {
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
  def `external var as result of await 3`() {
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

//  @Test def `this reference is maintained`() {
//    object Test {
//      def blerg = 1
//      def check() {
//        AsyncId.async {
//          assert(this.blerg == 1)
//          assert(this == Test, this.getClass)
//        }
//      }
//    }
//    Test.check()
//  }
}
