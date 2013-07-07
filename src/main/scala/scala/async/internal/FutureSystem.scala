/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async.internal

import scala.language.higherKinds

import scala.reflect.macros.Context
import scala.reflect.internal.SymbolTable

/**
 * An abstraction over a future system.
 *
 * Used by the macro implementations in [[scala.async.AsyncBase]] to
 * customize the code generation.
 *
 * The API mirrors that of `scala.concurrent.Future`, see the instance
 * [[ScalaConcurrentFutureSystem]] for an example of how
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
    val universe: reflect.internal.SymbolTable

    import universe._
    def Expr[T: WeakTypeTag](tree: Tree): Expr[T] = universe.Expr[T](rootMirror, universe.FixedMirrorTreeCreator(rootMirror, tree))

    def promType[A: WeakTypeTag]: Type
    def execContextType: Type

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

    def spawn(tree: Tree, execContext: Tree): Tree =
      future(Expr[Unit](tree))(Expr[ExecContext](execContext)).tree

    // TODO Why is this needed?
    def castTo[A: WeakTypeTag](future: Expr[Fut[Any]]): Expr[Fut[A]]
  }

  def mkOps(c: SymbolTable): Ops { val universe: c.type }
}

object ScalaConcurrentFutureSystem extends FutureSystem {

  import scala.concurrent._

  type Prom[A] = Promise[A]
  type Fut[A] = Future[A]
  type ExecContext = ExecutionContext

  def mkOps(c: SymbolTable): Ops {val universe: c.type} = new Ops {
    val universe: c.type = c

    import universe._

    def promType[A: WeakTypeTag]: Type = weakTypeOf[Promise[A]]
    def execContextType: Type = weakTypeOf[ExecutionContext]

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
      Expr[Unit](Literal(Constant(()))).splice
    }

    def castTo[A: WeakTypeTag](future: Expr[Fut[Any]]): Expr[Fut[A]] = reify {
      future.splice.asInstanceOf[Fut[A]]
    }
  }
}
