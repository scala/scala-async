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

import concurrent.{CanAwait, Awaitable}
import concurrent.duration.Duration
import java.util.concurrent.{TimeoutException, CountDownLatch, TimeUnit}

object TestLatch {
  val DefaultTimeout = Duration(5, TimeUnit.SECONDS)

  def apply(count: Int = 1) = new TestLatch(count)
}


class TestLatch(count: Int = 1) extends Awaitable[Unit] {
  private var latch = new CountDownLatch(count)

  def countDown() = latch.countDown()

  def isOpen: Boolean = latch.getCount == 0

  def open() = while (!isOpen) countDown()

  def reset() = latch = new CountDownLatch(count)

  @throws(classOf[TimeoutException])
  def ready(atMost: Duration)(implicit permit: CanAwait) = {
    val opened = latch.await(atMost.toNanos, TimeUnit.NANOSECONDS)
    if (!opened) throw new TimeoutException(s"Timeout of ${(atMost.toString)}.")
    this
  }

  @throws(classOf[Exception])
  def result(atMost: Duration)(implicit permit: CanAwait): Unit = {
    ready(atMost)
  }
}
