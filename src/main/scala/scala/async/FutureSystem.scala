/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import scala.language.higherKinds

import scala.reflect.macros.Context

/**
 * An abstraction over a future system.
 *
 * Used by the macro implementations in [[scala.async.AsyncBase]] to
 * customize the code generation.
 *
 * The API mirrors that of `scala.concurrent.Future`, see the instance
 * [[scala.async.ScalaConcurrentFutureSystem]] for an example of how
 * to implement this.
 */
trait FutureSystem {
  /** A container to receive the final value of the computation */
  type Prom[A]
  /** A (potentially in-progress) computation */
  type Fut[A]
  /** An execution context, required to create or register an on completion callback on a Future. */
  type ExecContext

  trait Ops {
    val context: reflect.macros.Context

    import context.universe._

    /** Lookup the execution context, typically with an implicit search */
    def execContext: Expr[ExecContext]

    /** Create an empty promise */
    def createProm[A: WeakTypeTag]: Expr[Prom[A]]

    /** Extract a future from the given promise. */
    def promiseToFuture[A: WeakTypeTag](prom: Expr[Prom[A]]): Expr[Fut[A]]

    /** Construct a future to asynchronously compute the given expression */
    def future[A: WeakTypeTag](a: Expr[A])(execContext: Expr[ExecContext]): Expr[Fut[A]]

    /** Register an call back to run on completion of the given future */
    def onComplete[A, U](future: Expr[Fut[A]], fun: Expr[scala.util.Try[A] => U],
                         execContext: Expr[ExecContext]): Expr[Unit]

    /** Complete a promise with a value */
    def completeProm[A](prom: Expr[Prom[A]], value: Expr[scala.util.Try[A]]): Expr[Unit]
  }

  def mkOps(c: Context): Ops {val context: c.type}
}

object ScalaConcurrentFutureSystem extends FutureSystem {

  import scala.concurrent._

  type Prom[A] = Promise[A]
  type Fut[A] = Future[A]
  type ExecContext = ExecutionContext

  def mkOps(c: Context): Ops {val context: c.type} = new Ops {
    val context: c.type = c

    import context.universe._

    def execContext: Expr[ExecContext] = c.Expr(c.inferImplicitValue(c.weakTypeOf[ExecutionContext]) match {
      case EmptyTree => c.abort(c.macroApplication.pos, "Unable to resolve implicit ExecutionContext")
      case context => context
    })

    def createProm[A: WeakTypeTag]: Expr[Prom[A]] = reify {
      Promise[A]()
    }

    def promiseToFuture[A: WeakTypeTag](prom: Expr[Prom[A]]) = reify {
      prom.splice.future
    }

    def future[A: WeakTypeTag](a: Expr[A])(execContext: Expr[ExecContext]) = reify {
      Future(a.splice)(execContext.splice)
    }

    def onComplete[A, U](future: Expr[Fut[A]], fun: Expr[scala.util.Try[A] => U],
                         execContext: Expr[ExecContext]): Expr[Unit] = reify {
      future.splice.onComplete(fun.splice)(execContext.splice)
    }

    def completeProm[A](prom: Expr[Prom[A]], value: Expr[scala.util.Try[A]]): Expr[Unit] = reify {
      prom.splice.complete(value.splice)
      context.literalUnit.splice
    }
  }
}

/**
 * A trivial implementation of [[scala.async.FutureSystem]] that performs computations
 * on the current thread. Useful for testing.
 */
object IdentityFutureSystem extends FutureSystem {

  class Prom[A](var a: A)

  type Fut[A] = A
  type ExecContext = Unit

  def mkOps(c: Context): Ops {val context: c.type} = new Ops {
    val context: c.type = c

    import context.universe._

    def execContext: Expr[ExecContext] = c.literalUnit

    def createProm[A: WeakTypeTag]: Expr[Prom[A]] = reify {
      new Prom(null.asInstanceOf[A])
    }

    def promiseToFuture[A: WeakTypeTag](prom: Expr[Prom[A]]) = reify {
      prom.splice.a
    }

    def future[A: WeakTypeTag](t: Expr[A])(execContext: Expr[ExecContext]) = t

    def onComplete[A, U](future: Expr[Fut[A]], fun: Expr[scala.util.Try[A] => U],
                         execContext: Expr[ExecContext]): Expr[Unit] = reify {
      fun.splice.apply(util.Success(future.splice))
      context.literalUnit.splice
    }

    def completeProm[A](prom: Expr[Prom[A]], value: Expr[scala.util.Try[A]]): Expr[Unit] = reify {
      prom.splice.a = value.splice.get
      context.literalUnit.splice
    }
  }
}
