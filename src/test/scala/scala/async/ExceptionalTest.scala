package scala.async

import java.util.concurrent.atomic.AtomicReference

import org.junit.{Assert, Ignore, Test}

import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future, _}
import scala.language.postfixOps
import scala.util.control.NonFatal

class ExceptionalTest {
  @Test
  def nonFatalNotCaughtFutureCombinators(): Unit = {
    check { implicit ec =>
      Future.successful(42).map(x => (x, throw fatal))
    }
  }

  @Test
  @Ignore // TODO https://github.com/scala/scala-async/issues/243
  def nonFatalNotCaughtAsync(): Unit = {
    check { implicit ec =>
      async {
        (await(Future.successful(42)), throw fatal)
      }
    }
  }

  def check(f: ExecutionContext => Future[Any]): Unit = {
    val lastUncaught = new AtomicReference[Throwable]()
    implicit val executor: ExecutionContextExecutor = ExecutionContext.fromExecutor(null, lastUncaught.set(_))
    val future = f(executor)
    Thread.sleep(100)
    Assert.assertSame(fatal, lastUncaught.get())
  }

  private val fatal: Throwable = {
    val t = new VirtualMachineError() {}
    Assert.assertTrue(NonFatal.unapply(t).isEmpty)
    t
  }
}
