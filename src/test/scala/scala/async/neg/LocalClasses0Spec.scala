/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package neg

/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class LocalClasses0Spec {

  @Test
  def `reject a local class`() {
    expectError("Local class Person illegal within `async` block", "-cp target/scala-2.10/classes -deprecation -Xfatal-warnings") {
      """
        | import scala.concurrent.ExecutionContext.Implicits.global
        | import scala.async.Async._
        | 
        | async {
        |   case class Person(name: String)
        | }
      """.stripMargin
    }
  }

  @Test
  def `reject a local class 2`() {
    expectError("Local class Person illegal within `async` block", "-cp target/scala-2.10/classes -deprecation -Xfatal-warnings") {
      """
        | import scala.concurrent.{Future, ExecutionContext}
        | import ExecutionContext.Implicits.global
        | import scala.async.Async._
        | 
        | async {
        |   case class Person(name: String)
        |   val fut = Future { 5 }
        |   val x = await(fut)
        |   x
        | }
      """.stripMargin
    }
  }

  @Test
  def `reject a local class 3`() {
    expectError("Local class Person illegal within `async` block", "-cp target/scala-2.10/classes -deprecation -Xfatal-warnings") {
      """
        | import scala.concurrent.{Future, ExecutionContext}
        | import ExecutionContext.Implicits.global
        | import scala.async.Async._
        | 
        | async {
        |   val fut = Future { 5 }
        |   val x = await(fut)
        |   case class Person(name: String)
        |   x
        | }
      """.stripMargin
    }
  }

  @Test
  def `reject a local class with symbols in its name`() {
    expectError("Local class :: illegal within `async` block", "-cp target/scala-2.10/classes -deprecation -Xfatal-warnings") {
      """
        | import scala.concurrent.{Future, ExecutionContext}
        | import ExecutionContext.Implicits.global
        | import scala.async.Async._
        | 
        | async {
        |   val fut = Future { 5 }
        |   val x = await(fut)
        |   case class ::(name: String)
        |   x
        | }
      """.stripMargin
    }
  }

  @Test
  def `reject a nested local class`() {
    expectError("Local class Person illegal within `async` block", "-cp target/scala-2.10/classes -deprecation -Xfatal-warnings") {
      """
        | import scala.concurrent.{Future, ExecutionContext}
        | import ExecutionContext.Implicits.global
        | import scala.async.Async._
        | 
        | async {
        |   val fut = Future { 5 }
        |   val x = 2 + 2
        |   var y = 0
        |   if (x > 0) {
        |     case class Person(name: String)
        |     y = await(fut)
        |   } else {
        |     y = x
        |   }
        |   y
        | }
      """.stripMargin
    }
  }

  @Test
  def `reject a local singleton object`() {
    expectError("Local object Person illegal within `async` block", "-cp target/scala-2.10/classes -deprecation -Xfatal-warnings") {
      """
        | import scala.concurrent.ExecutionContext.Implicits.global
        | import scala.async.Async._
        | 
        | async {
        |   object Person { val name = "Joe" }
        | }
      """.stripMargin
    }
  }

}
