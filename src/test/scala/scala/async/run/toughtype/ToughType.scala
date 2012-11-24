/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package toughtype

import language.{reflectiveCalls, postfixOps}
import scala.concurrent._
import scala.concurrent.duration._
import scala.async.Async._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4


object ToughTypeObject {

  import ExecutionContext.Implicits.global

  class Inner

  def m2 = async[(List[_], ToughTypeObject.Inner)] {
    val y = await(future[List[_]](Nil))
    val z = await(future[Inner](new Inner))
    (y, z)
  }
}

@RunWith(classOf[JUnit4])
class ToughTypeSpec {

  @Test def `propogates tough types`() {
    val fut = ToughTypeObject.m2
    val res: (List[_], scala.async.run.toughtype.ToughTypeObject.Inner) = Await.result(fut, 2 seconds)
    res._1 mustBe (Nil)
  }
}
