package scala.async

import language.reflectiveCalls
import language.postfixOps
import language.implicitConversions

import scala.reflect.{ClassTag, classTag}

import scala.collection.mutable
import scala.concurrent.{Future, Awaitable, CanAwait}
import java.util.concurrent.{TimeoutException, CountDownLatch, TimeUnit}
import scala.concurrent.duration.Duration
import org.junit.runner.RunWith
import org.junit.runners.JUnit4


trait Output {
  val buffer = new StringBuilder

  def bufferPrintln(a: Any): Unit = buffer.synchronized {
    buffer.append(a.toString + "\n")
  }
}

trait MinimalScalaTest extends Output {
  implicit class objectops(obj: Any) {
    def mustBe(other: Any) = assert(obj == other, obj + " is not " + other)

    def mustEqual(other: Any) = mustBe(other)
  }

  implicit class stringops(text: String) {
    def mustContain(substring: String) = assert(text contains substring, text)
  }

  def intercept[T <: Throwable : ClassTag](body: => Any): T = {
    try {
      body
      throw new Exception(s"Exception of type ${classTag[T]} was not thrown")
    } catch {
      case t: Throwable =>
        if (classTag[T].runtimeClass != t.getClass) throw t
        else t.asInstanceOf[T]
    }
  }
}


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
