package scala.async
package run
package gen0

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.Test

class TestS[A](a:A)
{

  def awrite(v:A): Future[A] =
      Future successful v

  def aread: Future[A] =
      Future successful a

}

class Test1GenAsyncOp[A] {

  import ExecutionContext.Implicits.global

  def testfun[A](a1:A,a2:A): Future[Boolean] = async {
       val ts = new TestS(a1)
       val s1 = await(ts.awrite(a2))
       val s2 = await(ts.aread)
       s1 == s2
  }

}


class AsyncSpec {

  @Test
  def `operation with futures generic`() {
    val op = new Test1GenAsyncOp[Int]
    val f = op.testfun(1,2)
    val res = Await.result(f, 2 seconds)
    res mustBe (false)
  }

}
