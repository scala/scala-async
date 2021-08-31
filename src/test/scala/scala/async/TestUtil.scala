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

import java.util.concurrent.{CountDownLatch, TimeUnit}

import scala.concurrent.{Awaitable, CanAwait, TimeoutException}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.reflect.{ClassTag, classTag}

object TestUtil {
  object TestLatch {
    val DefaultTimeout: FiniteDuration = Duration(5, TimeUnit.SECONDS)

    def apply(count: Int = 1) = new TestLatch(count)
  }

  class TestLatch(count: Int = 1) extends Awaitable[Unit] {
    private var latch = new CountDownLatch(count)

    def countDown(): Unit = latch.countDown()

    def isOpen: Boolean = latch.getCount == 0

    def open(): Unit = while (!isOpen) countDown()

    def reset(): Unit = latch = new CountDownLatch(count)

    @throws(classOf[TimeoutException])
    def ready(atMost: Duration)(implicit permit: CanAwait): TestLatch.this.type = {
      val opened = latch.await(atMost.toNanos, TimeUnit.NANOSECONDS)
      if (!opened) throw new TimeoutException(s"Timeout of ${(atMost.toString)}.")
      this
    }

    @throws(classOf[Exception])
    def result(atMost: Duration)(implicit permit: CanAwait): Unit = {
      ready(atMost)
    }
  }
  def intercept[T <: Throwable : ClassTag](body: => Any): T = {
    try {
      body
      throw new Exception(s"Exception of type ${classTag[T]} was not thrown")
    } catch {
      case t: Throwable =>
        if (!classTag[T].runtimeClass.isAssignableFrom(t.getClass)) throw t
        else t.asInstanceOf[T]
    }
  }

  implicit class objectops(obj: Any) {
    def mustBe(other: Any): Unit = assert(obj == other, s"$obj is not $other")

    def mustEqual(other: Any): Unit = mustBe(other)
  }
}
