/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
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
