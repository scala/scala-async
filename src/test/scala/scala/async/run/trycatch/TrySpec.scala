/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package trycatch

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class TrySpec {

  @Test
  def tryCatch1() {
    import AsyncId._

    val result = async {
      var xxx: Int = 0
      try {
        val y = await(xxx)
        xxx = xxx + 1
        y
      } catch {
        case e: Exception =>
          assert(false)
      }
      xxx
    }
    assert(result == 1)
  }

  @Test
  def tryCatch2() {
    import AsyncId._

    val result = async {
      var xxx: Int = 0
      try {
        val y = await(xxx)
        throw new Exception("test msg")
        assert(false)
        xxx = xxx + 1
        y
      } catch {
        case e: Exception =>
          assert(e.getMessage == "test msg")
          xxx = 7
      }
      xxx
    }
    assert(result == 7)
  }

  @Test
  def nestedTry1() {
    import AsyncId._

    val result = async {
      var xxx = 0
      try {
        try {
          val y = await(xxx)
          throw new IllegalArgumentException("msg")
          assert(false)
          y + 2
        } catch {
          case iae: IllegalArgumentException =>
            xxx = 6
        }
      } catch {
        case nsee: NoSuchElementException =>
          xxx = 7
      }
      xxx
    }
    assert(result == 6)
  }

  @Test
  def nestedTry2() {
    import AsyncId._

    val result = async {
      var xxx = 0
      try {
        try {
          val y = await(xxx)
          throw new NoSuchElementException("msg")
          assert(false)
          y + 2
        } catch {
          case iae: IllegalArgumentException =>
            xxx = 6
        }
      } catch {
        case nsee: NoSuchElementException =>
          xxx = 7
      }
      xxx
    }
    assert(result == 7)
  }

  @Test
  def tryAsExpr() {
    import AsyncId._

    val result = async {
      val xxx: Int = 0
      try {
        val y = await(xxx)
        y + 2
      } catch {
        case e: Exception =>
          assert(false)
          xxx + 4
      }
    }
    assert(result == 2)
  }

  @Test
  def tryFinally1() {
    import AsyncId._

    var xxx: Int = 0
    val result = async {
      try {
        val y = await(xxx)
        y + 2
      } catch {
        case e: Exception =>
          assert(false)
          xxx + 4
      } finally {
        xxx = 5
      }
    }
    assert(result == 2)
    assert(xxx == 5)
  }

  @Test
  def tryFinally2() {
    import AsyncId._

    var xxx: Int = 0
    val result = async {
      try {
        val y = await(xxx)
        throw new Exception("msg")
        assert(false)
        y + 2
      } catch {
        case e: Exception =>
          xxx + 4
      } finally {
        xxx = 6
      }
    }
    assert(result == 4)
    assert(xxx == 6)
  }

  @Test
  def tryFinallyAwait1() {
    import AsyncId._

    var xxx: Int = 0
    var uuu: Int = 10
    val result = async {
      try {
        val y = await(xxx)
        y + 2
      } catch {
        case e: Exception =>
          assert(false)
          xxx + 4
      } finally {
        val v = await(uuu)
        xxx = v
      }
    }
    assert(result == 2)
    assert(xxx == 10)
  }

  @Test
  def tryFinallyAwait2() {
    import AsyncId._

    var xxx: Int = 0
    var uuu: Int = 10
    val result = async {
      try {
        val y = await(xxx)
        throw new Exception("msg")
        assert(false)
        y + 2
      } catch {
        case e: Exception =>
          xxx + 4
      } finally {
        val v = await(uuu)
        xxx = v
      }
    }
    assert(result == 4)
    assert(xxx == 10)
  }

  @Test
  def tryFinallyANF() {
    import Async._
    import scala.concurrent.{ future, ExecutionContext, Await }
    import ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration

    val x = 21
    val fut = future { x * 2 }

    val finalFut = async {
      var offset = 0
      val res = try {
        throw new Exception("problem")
        0 // normal result
      } catch {
        case e: Exception => 1 // exceptional result
      } finally {
        offset = await(fut)
      }
      res + offset
    }
    val finalRes = Await.result(finalFut, Duration.Inf)
    assert(finalRes == 43)
  }

}
