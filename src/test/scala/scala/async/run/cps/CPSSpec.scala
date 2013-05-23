/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package cps

import scala.concurrent.{Future, Promise, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.continuations.CPSBasedAsync._
import scala.util.continuations._

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
/*
@RunWith(classOf[JUnit4])
class CPSSpec {

  import ExecutionContext.Implicits.global

  def m1(y: Int): Future[Int] = async {
    val f = future { y + 2 }
    val f2 = future { y + 3 }
    val x1 = await(f)
    val x2 = await(f2)
    x1 + x2
  }

  def m2(y: Int): Future[Int] = async {
    val f = future { y + 2 }
    val res = await(f)
    if (y > 0) res + 2
    else res - 2
  }

  @Test
  def testCPSFallback() {
    val fut1 = m1(10)
    val res1 = Await.result(fut1, 2.seconds)
    assert(res1 == 25, s"expected 25, got $res1")

    val fut2 = m2(10)
    val res2 = Await.result(fut2, 2.seconds)
    assert(res2 == 14, s"expected 14, got $res2")
  }

}
*/
