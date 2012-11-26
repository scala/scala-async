/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package exceptions

import scala.async.Async.{async, await}

import scala.concurrent.{future, ExecutionContext, Await}
import ExecutionContext.Implicits._
import scala.concurrent.duration._
import scala.reflect.ClassTag

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ExceptionsSpec {

  @Test
  def `complete future with exception`() {
    val future0 = future[Any] {
      "five!".length
    }

    val future2 = async {
      val a = await(future0.mapTo[Int])                       // result: 5
      val b = await((future { (a * 2).toString }).mapTo[Int]) // result: 10
      val c = await(future { (7 * 2).toString })              // result: "14"
      b + "-" + c
    }

    intercept[ClassCastException] { Await.result(future2, 5.seconds) }
  }

}
