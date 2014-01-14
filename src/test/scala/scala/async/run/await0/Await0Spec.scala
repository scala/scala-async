/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package await0

/**
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

import language.{reflectiveCalls, postfixOps}

import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.Test

class Await0Class {

  import ExecutionContext.Implicits.global

  def m1(x: Double): Future[Double] = future {
    x + 2.0
  }

  def m2(x: Float): Future[Float] = future {
    x + 2.0f
  }

  def m3(x: Char): Future[Char] = future {
    (x.toInt + 2).toChar
  }

  def m4(x: Short): Future[Short] = future {
    (x + 2).toShort
  }

  def m5(x: Byte): Future[Byte] = future {
    (x + 2).toByte
  }

  def m0(y: Int): Future[Double] = async {
    val f1 = m1(y.toDouble)
    val x1: Double = await(f1)

    val f2 = m2(y.toFloat)
    val x2: Float = await(f2)

    val f3 = m3(y.toChar)
    val x3: Char = await(f3)

    val f4 = m4(y.toShort)
    val x4: Short = await(f4)

    val f5 = m5(y.toByte)
    val x5: Byte = await(f5)

    x1 + x2 + 2.0
  }
}

class Await0Spec {

  @Test
  def `An async method support a simple await`() {
    val o = new Await0Class
    val fut = o.m0(10)
    val res = Await.result(fut, 10 seconds)
    res mustBe (26.0)
  }
}
