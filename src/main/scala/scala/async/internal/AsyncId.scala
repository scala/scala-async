/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async.internal

import language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.internal.SymbolTable

object AsyncId extends AsyncBase {
  lazy val futureSystem = IdentityFutureSystem
  type FS = IdentityFutureSystem.type

  def async[T](body: T) = macro asyncIdImpl[T]

  def asyncIdImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[T] = asyncImpl[T](c)(body)(c.literalUnit)
}

/**
 * A trivial implementation of [[FutureSystem]] that performs computations
 * on the current thread. Useful for testing.
 */
object IdentityFutureSystem extends FutureSystem {

  class Prom[A] {
    var a: A = _
  }

  type Fut[A] = A
  type ExecContext = Unit

  def mkOps(c: SymbolTable): Ops {val universe: c.type} = new Ops {
    val universe: c.type = c

    import universe._

    def execContext: Expr[ExecContext] = Expr[Unit](Literal(Constant(())))

    def promType[A: WeakTypeTag]: Type = weakTypeOf[Prom[A]]
    def execContextType: Type = weakTypeOf[Unit]

    def createProm[A: WeakTypeTag]: Expr[Prom[A]] = reify {
      new Prom()
    }

    def promiseToFuture[A: WeakTypeTag](prom: Expr[Prom[A]]) = reify {
      prom.splice.a
    }

    def future[A: WeakTypeTag](t: Expr[A])(execContext: Expr[ExecContext]) = t

    def onComplete[A, U](future: Expr[Fut[A]], fun: Expr[scala.util.Try[A] => U],
                         execContext: Expr[ExecContext]): Expr[Unit] = reify {
      fun.splice.apply(util.Success(future.splice))
      Expr[Unit](Literal(Constant(()))).splice
    }

    def completeProm[A](prom: Expr[Prom[A]], value: Expr[scala.util.Try[A]]): Expr[Unit] = reify {
      prom.splice.a = value.splice.get
      Expr[Unit](Literal(Constant(()))).splice
    }

    def castTo[A: WeakTypeTag](future: Expr[Fut[Any]]): Expr[Fut[A]] = ???
  }
}
