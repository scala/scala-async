/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Await, Future, ExecutionContext, future}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import scala.async.run.anf.State
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class DefaultValueBugSpec {

  import ExecutionContext.Implicits.global

  def m2[T](in: Any, f: Any => T, fut: Future[T]) = async {
    in match {
      case _ =>
        await(fut)
    }
  }

  @Test
  def defaultValueForGenericTpe() {
    val res = m2[Int]("in", _ => 1, async(2))
    val value = Await.result(res, 2.seconds)
    value mustBe (2)
  }
}

