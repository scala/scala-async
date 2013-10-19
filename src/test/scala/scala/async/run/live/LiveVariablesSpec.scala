/*
 * Copyright (C) 2012-2013 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package live

import org.junit.Test

import internal.AsyncTestLV
import AsyncTestLV._

case class Cell[T](v: T)

class Meter(val len: Long) extends AnyVal

case class MCell[T](var v: T)


class LiveVariablesSpec {

  @Test
  def `zero out fields of reference type`() {
    val f = async { Cell(1) }

    def m1(x: Cell[Int]): Cell[Int] =
      async { Cell(x.v + 1) }

    def m2(x: Cell[Int]): String =
      async { x.v.toString }

    def m3() = async {
      val a: Cell[Int] = await(f)      // await$1$1
      // a == Cell(1)
      val b: Cell[Int] = await(m1(a))  // await$2$1
      // b == Cell(2)
      assert(AsyncTestLV.log.exists(_ == ("await$1$1" -> Cell(1))))
      val res = await(m2(b))           // await$3$1
      assert(AsyncTestLV.log.exists(_ == ("await$2$1" -> Cell(2))))
      res
    }

    assert(m3() == "2")
  }

  @Test
  def `zero out fields of type Any`() {
    val f = async { Cell(1) }

    def m1(x: Cell[Int]): Cell[Int] =
      async { Cell(x.v + 1) }

    def m2(x: Any): String =
      async { x.toString }

    def m3() = async {
      val a: Cell[Int] = await(f)      // await$4$1
      // a == Cell(1)
      val b: Any = await(m1(a))        // await$5$1
      // b == Cell(2)
      assert(AsyncTestLV.log.exists(_ == ("await$4$1" -> Cell(1))))
      val res = await(m2(b))           // await$6$1
      assert(AsyncTestLV.log.exists(_ == ("await$5$1" -> Cell(2))))
      res
    }

    assert(m3() == "Cell(2)")
  }

  @Test
  def `do not zero out fields of primitive type`() {
    val f = async { 1 }

    def m1(x: Int): Cell[Int] =
      async { Cell(x + 1) }

    def m2(x: Any): String =
      async { x.toString }

    def m3() = async {
      val a: Int = await(f)            // await$7$1
      // a == 1
      val b: Any = await(m1(a))        // await$8$1
      // b == Cell(2)
      assert(!AsyncTestLV.log.exists(p => p._1 == "await$7$1"))
      val res = await(m2(b))           // await$9$1
      assert(AsyncTestLV.log.exists(_ == ("await$8$1" -> Cell(2))))
      res
    }

    assert(m3() == "Cell(2)")
  }

  @Test
  def `zero out fields of value class type`() {
    val f = async { Cell(1) }

    def m1(x: Cell[Int]): Meter =
      async { new Meter(x.v + 1) }

    def m2(x: Any): String =
      async { x.toString }

    def m3() = async {
      val a: Cell[Int] = await(f)      // await$10$1
      // a == Cell(1)
      val b: Meter = await(m1(a))      // await$11$1
      // b == Meter(2)
      assert(AsyncTestLV.log.exists(_ == ("await$10$1" -> Cell(1))))
      val res = await(m2(b.len))       // await$12$1
      assert(AsyncTestLV.log.exists(entry => entry._1 == "await$11$1" && entry._2.asInstanceOf[Meter].len == 2L))
      res
    }

    assert(m3() == "2")
  }

  @Test
  def `zero out fields after use in loop`() {
    val f = async { MCell(1) }

    def m1(x: MCell[Int], y: Int): Int =
      async { x.v + y }

    def m3() = async {
      // state #1
      val a: MCell[Int] = await(f)     // await$13$1
      // state #2
      var y = MCell(0)

      while (a.v < 10) {
        // state #4
        a.v = a.v + 1
        y = MCell(await(a).v + 1)      // await$14$1
        // state #7
      }

      // state #3
      assert(AsyncTestLV.log.exists(entry => entry._1 == "await$14$1"))

      val b = await(m1(a, y.v))        // await$15$1
      // state #8
      assert(AsyncTestLV.log.exists(_ == ("a$1" -> MCell(10))))
      assert(AsyncTestLV.log.exists(_ == ("y$1" -> MCell(11))))
      b
    }

    assert(m3() == 21)
  }

}
