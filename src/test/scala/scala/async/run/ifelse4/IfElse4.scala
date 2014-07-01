/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package ifelse3

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.Test


class TestIfElse4Class {

  import ExecutionContext.Implicits.global
  
  class F[A]
  class S[A](val id: String)
  trait P
 
  case class K(f: F[_])

  def result[A](f: F[A]) = async {
    new S[A with P]("foo")
  }
    
  def run(k: K) = async {
    val res = await(result(k.f))
    if(true)
      println(res)
    res 
  }
}

class IfElse4Spec {

  @Test
  def `await result with complex type containing skolem`() {
    val o = new TestIfElse4Class
    val fut = o.run(new o.K(null))
    val res = Await.result(fut, 2 seconds)
    res.id mustBe ("foo")
  }
}
