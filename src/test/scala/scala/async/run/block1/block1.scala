/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package block1

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.Test


class Test1Class {

  import ExecutionContext.Implicits.global

  def m1(x: Int): Future[Int] = future {
    x + 2
  }

  def m4(y: Int): Future[Int] = async {
    val f1 = m1(y)
    val f2 = m1(y + 2)
    val x1 = await(f1)
    val x2 = await(f2)
    x1 + x2
  }
}

class Block1Spec {

  @Test def `support a simple await`() {
    val o = new Test1Class
    val fut = o.m4(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (26)
  }
}
