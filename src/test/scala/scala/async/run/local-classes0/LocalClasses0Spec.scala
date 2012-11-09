package scala.async
package run
package await0

/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

import language.{reflectiveCalls, postfixOps}

import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

class LocalClasses0Class {

  import ExecutionContext.Implicits.global
  
  def base(x: Int): Future[Int] = future {
    x + 2
  }
  
  def methodWithLocalClass(): Future[Int] = async {
    case class Person(name: String)
    val fut = base(10)
    val x = await(fut)
    x + 1
  }
}

@RunWith(classOf[JUnit4])
class LocalClasses0Spec {

  @Test
  def `An async method should reject local classes without crashing`() {
    val o = new LocalClasses0Class
    val fut = o.methodWithLocalClass()
    val res = Await.result(fut, 2 seconds)
    res mustBe (13)
  }
}

