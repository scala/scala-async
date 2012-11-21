/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async
package neg

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{Ignore, Test}

@RunWith(classOf[JUnit4])
class AnfTransformNegSpec {

  @Test
  @Ignore
  def `inlining block produces duplicate definition`() {
    expectError("x is already defined as value x", "-deprecation -Xfatal-warnings") {
      """
        | import scala.concurrent.ExecutionContext.Implicits.global
        | import scala.concurrent.Future
        | import scala.async.Async._
        | 
        | async {
        |   val f = Future { 12 }
        |   val x = await(f)
        |   
        |   {
        |     val x = 42
        |     println(x)
        |   }
        |   
        |   x
        | }
      """.stripMargin
    }
  }

  @Test
  @Ignore
  def `inlining block in tail position produces duplicate definition`() {
    expectError("x is already defined as value x", "-deprecation -Xfatal-warnings") {
      """
        | import scala.concurrent.ExecutionContext.Implicits.global
        | import scala.concurrent.Future
        | import scala.async.Async._
        | 
        | async {
        |   val f = Future { 12 }
        |   val x = await(f)
        |   
        |   {
        |     val x = 42
        |     x
        |   }
        | }
      """.stripMargin
    }
  }
}
