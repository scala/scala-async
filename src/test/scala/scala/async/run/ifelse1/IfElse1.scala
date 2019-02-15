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
package ifelse1

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Future, ExecutionContext, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.Test


class TestIfElse1Class {

  import ExecutionContext.Implicits.global

  def base(x: Int): Future[Int] = Future {
    x + 2
  }

  def m1(y: Int): Future[Int] = async {
    val f = base(y)
    var z = 0
    if (y > 0) {
      if (y > 100)
        5
      else {
        val x1 = await(f)
        z = x1 + 2
      }
    } else {
      val x2 = await(f)
      z = x2 - 2
    }
    z
  }

  def m2(y: Int): Future[Int] = async {
    val f = base(y)
    var z = 0
    if (y > 0) {
      if (y < 100) {
        val x1 = await(f)
        z = x1 + 2
      }
      else
        5
    } else {
      val x2 = await(f)
      z = x2 - 2
    }
    z
  }

  def m3(y: Int): Future[Int] = async {
    val f = base(y)
    var z = 0
    if (y < 0) {
      val x2 = await(f)
      z = x2 - 2
    } else {
      if (y > 100)
        5
      else {
        val x1 = await(f)
        z = x1 + 2
      }
    }
    z
  }

  def m4(y: Int): Future[Int] = async {
    val f = base(y)
    var z = 0
    if (y < 0) {
      val x2 = await(f)
      z = x2 - 2
    } else {
      if (y < 100) {
        val x1 = await(f)
        z = x1 + 2
      } else
        5
    }
    z
  }

  def pred: Future[Boolean] = async(true) 

  def m5: Future[Boolean] = async {
    if(if(if(if(if(if(if(if(if(if(if(if(if(if(if(if(if(if(if(if(if(await(pred))
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false)
      await(pred)
    else
      false
  }
}

class IfElse1Spec {

  @Test
  def `await in a nested if-else expression`(): Unit = {
    val o = new TestIfElse1Class
    val fut = o.m1(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (14)
  }

  @Test
  def `await in a nested if-else expression 2`(): Unit = {
    val o = new TestIfElse1Class
    val fut = o.m2(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (14)
  }


  @Test
  def `await in a nested if-else expression 3`(): Unit = {
    val o = new TestIfElse1Class
    val fut = o.m3(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (14)
  }


  @Test
  def `await in a nested if-else expression 4`(): Unit = {
    val o = new TestIfElse1Class
    val fut = o.m4(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (14)
  }

  @Test
  def `await in deeply-nested if-else conditions`(): Unit = {
    val o = new TestIfElse1Class
    val fut = o.m5
    val res = Await.result(fut, 2 seconds)
    res mustBe true
  }
}
