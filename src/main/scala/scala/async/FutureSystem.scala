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
  /** Result of an asynchronous computation */
  type Result[A]
  /** An execution context, required to create or register an on completion callback on a Future. */
  type ExecContext

  trait Ops {
    val c: reflect.macros.Context

    import c.universe._

    /** Lookup the execution context, typically with an implicit search */
    def execContext: Expr[ExecContext]

    def promType[A: WeakTypeTag]: Type
    def resultType[A: WeakTypeTag]: Type
    def execContextType: Type

    /** Create an empty promise */
    def createProm[A: WeakTypeTag]: Expr[Prom[A]]

    /** Extract a future from the given promise. */
    def promiseToFuture[A: WeakTypeTag](prom: Expr[Prom[A]]): Expr[Fut[A]]

    /** Construct a future to asynchronously compute the given expression */
    def future[A: WeakTypeTag](a: Expr[A])(execContext: Expr[ExecContext]): Expr[Fut[A]]

    /** Register an call back to run on completion of the given future */
    def onComplete[A, U](future: Expr[Fut[A]], fun: Expr[Result[A] => U],
                         execContext: Expr[ExecContext]): Expr[Unit]

    /** Complete a promise with a value */
    def completeProm[A: WeakTypeTag](prom: Expr[Prom[A]], value: Expr[A]): Expr[Unit]

    /** Complete a promise with an exception */
    def completePromWithExceptionTopLevel[A: WeakTypeTag](prom: Expr[Prom[A]], exception: Expr[Throwable]): Expr[Unit]

    /** Complete a promise with a failed result */
    def completePromWithFailedResult[A: WeakTypeTag](prom: Expr[Prom[A]], resultName: TermName): Expr[Unit]

    /** Test if the given result is failed */
    def isFailedResult(name: TermName): Expr[Boolean]

    /** Result value of a completion */
    def resultValue(name: TermName, resultType: Type): Tree

    def spawn(tree: Tree): Tree =
      future(c.Expr[Unit](tree))(execContext).tree

    def castTo[A: WeakTypeTag](future: Expr[Fut[Any]]): Expr[Fut[A]]
  }

  def mkOps(ctx: Context): Ops { val c: ctx.type }
}

trait TryBasedFutureSystem extends FutureSystem {

  type Result[A] = scala.util.Try[A]

  trait OpsWithTry extends Ops {
    import c.universe._

    def resultType[A: WeakTypeTag]: Type = c.weakTypeOf[scala.util.Try[A]]

    protected def completePromWithTry[A: WeakTypeTag](prom: Expr[Prom[A]], value: Expr[scala.util.Try[A]]): Expr[Unit]

    def completeProm[A: WeakTypeTag](prom: Expr[Prom[A]], value: Expr[A]): Expr[Unit] =
      completePromWithTry(prom, reify(scala.util.Success(value.splice)))

    def completePromWithExceptionTopLevel[A: WeakTypeTag](prom: Expr[Prom[A]], exception: Expr[Throwable]): Expr[Unit] =
      completePromWithTry(prom, reify(scala.util.Failure(exception.splice)))

    def completePromWithFailedResult[A: WeakTypeTag](prom: Expr[Prom[A]], resultName: TermName): Expr[Unit] = {
      val result = c.Expr[scala.util.Try[A]](
        TypeApply(Select(Ident(resultName), newTermName("asInstanceOf")),
                  List(TypeTree(weakTypeOf[scala.util.Try[A]]))))
      completePromWithTry(prom, result)
    }

    /** `methodSym( (_: Foo).bar(null: A, null: B)` will return the symbol of `bar`, after overload resolution. */
    private def methodSym(apply: c.Expr[Any]): Symbol = {
      val tree2: Tree = c.typeCheck(apply.tree)
      tree2.collect {
        case s: SymTree if s.symbol.isMethod => s.symbol
      }.headOption.getOrElse(sys.error(s"Unable to find a method symbol in ${apply.tree}"))
    }

    lazy val Try_isFailure = methodSym(reify((null: scala.util.Try[Any]).isFailure))
    lazy val Try_get       = methodSym(reify((null: scala.util.Try[Any]).get))

    def isFailedResult(name: TermName): Expr[Boolean] =
      c.Expr[Boolean](Select(Ident(name), Try_isFailure))

    def resultValue(name: TermName, resultType: Type): Tree =
      TypeApply(Select(Select(Ident(name), Try_get), newTermName("asInstanceOf")), List(TypeTree(resultType)))
  }

}

object ScalaConcurrentFutureSystem extends TryBasedFutureSystem {

  import scala.concurrent._

  type Prom[A] = Promise[A]
  type Fut[A] = Future[A]
  type ExecContext = ExecutionContext

  def mkOps(ctx: Context): Ops { val c: ctx.type } = new OpsWithTry {
    val c: ctx.type = ctx

    import c.universe._

    def execContext: Expr[ExecContext] = c.Expr(c.inferImplicitValue(c.weakTypeOf[ExecutionContext]) match {
      case EmptyTree => c.abort(c.macroApplication.pos, "Unable to resolve implicit ExecutionContext")
      case context => context
    })

    def promType[A: WeakTypeTag]: Type = c.weakTypeOf[Promise[A]]
    def execContextType: Type = c.weakTypeOf[ExecutionContext]

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

    protected def completePromWithTry[A: WeakTypeTag](prom: Expr[Prom[A]], value: Expr[scala.util.Try[A]]): Expr[Unit] = reify {
      prom.splice.complete(value.splice)
      c.literalUnit.splice
    }

    def castTo[A: WeakTypeTag](future: Expr[Fut[Any]]): Expr[Fut[A]] = reify {
      future.splice.asInstanceOf[Fut[A]]
    }
  }
}

/**
 * A trivial implementation of [[scala.async.FutureSystem]] that performs computations
 * on the current thread. Useful for testing.
 */
object IdentityFutureSystem extends TryBasedFutureSystem {

  class Prom[A](var a: A)

  type Fut[A] = A
  type ExecContext = Unit

  def mkOps(ctx: Context): Ops { val c: ctx.type } = new OpsWithTry {
    val c: ctx.type = ctx

    import c.universe._

    def execContext: Expr[ExecContext] = c.literalUnit

    def promType[A: WeakTypeTag]: Type = c.weakTypeOf[Prom[A]]
    def execContextType: Type = c.weakTypeOf[Unit]

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
      c.literalUnit.splice
    }

    protected def completePromWithTry[A: WeakTypeTag](prom: Expr[Prom[A]], value: Expr[scala.util.Try[A]]): Expr[Unit] = reify {
      prom.splice.a = value.splice.get
      c.literalUnit.splice
    }

    def castTo[A: WeakTypeTag](future: Expr[Fut[Any]]): Expr[Fut[A]] = ???
  }
}
