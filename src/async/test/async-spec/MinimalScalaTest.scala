package scala.async

import language.reflectiveCalls
import language.postfixOps
import language.implicitConversions

import scala.reflect.{ ClassTag, classTag }

import scala.collection.mutable
import scala.concurrent.{ Future, Awaitable, CanAwait }
import java.util.concurrent.{ TimeoutException, CountDownLatch, TimeUnit }
import scala.concurrent.util.Duration



trait Output {
  val buffer = new StringBuilder
  
  def bufferPrintln(a: Any): Unit = buffer.synchronized {
    buffer.append(a.toString + "\n")
  }
}


trait MinimalScalaTest extends Output {
  
  val throwables = mutable.ArrayBuffer[Throwable]()
  
  def check() {
    if (throwables.nonEmpty) println(buffer.toString)
  }
  
  implicit def stringops(s: String) = new {
    
    def should[U](snippets: =>U): U = {
      bufferPrintln(s + " should:")
      snippets
    }
    
    def in[U](snippet: =>U): Unit = {
      try {
        bufferPrintln("- " + s)
        snippet
        bufferPrintln("[OK] Test passed.")
      } catch {
        case e: Throwable =>
          bufferPrintln("[FAILED] " + e)
          bufferPrintln(e.getStackTrace().mkString("\n"))
          throwables += e
      }
    }
    
  }
  
  implicit def objectops(obj: Any) = new {
    
    def mustBe(other: Any) = assert(obj == other, obj + " is not " + other)
    def mustEqual(other: Any) = mustBe(other)
    
  }
  
  def intercept[T <: Throwable: ClassTag](body: =>Any): T = {
    try {
      body
      throw new Exception("Exception of type %s was not thrown".format(classTag[T]))
    } catch {
      case t: Throwable =>
        if (classTag[T].runtimeClass != t.getClass) throw t
        else t.asInstanceOf[T]
    }
  }
  
  def checkType[T: ClassTag, S](in: Future[T], refclasstag: ClassTag[S]): Boolean = classTag[T] == refclasstag
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
    if (!opened) throw new TimeoutException("Timeout of %s." format (atMost.toString))
    this
  }
  
  @throws(classOf[Exception])
  def result(atMost: Duration)(implicit permit: CanAwait): Unit = {
    ready(atMost)
  }
  
}
