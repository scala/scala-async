package scala.async.iterators

import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.async.{ AsyncBase, FutureSystem }
import scala.concurrent.{ Promise, Future, ExecutionContext }
import scala.util.Try

object Async extends AsyncBase {

  lazy val futureSystem = IteratorsFutureSystem
  type FS = IteratorsFutureSystem.type

  def async[T](body: T) = macro asyncImpl[T]

  override def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[IteratorState[T]] = super.asyncImpl[T](c)(body)

}

object IteratorsFutureSystem extends FutureSystem {
  type Prom[A]   = IteratorState[A]
  type Fut[A]    = IteratorState[A]
  type Result[A] = Try[A]
  type ExecContext = Unit

  def mkOps(context: Context): Ops { val c: context.type } = new Ops {
    val c: context.type = context
    import c.universe._

    def promType[A: WeakTypeTag]: Type = c.weakTypeOf[Prom[A]]

    def stateMachineType[A: WeakTypeTag]: Type =
      // The generated state machine will extend trait `IteratorState`
      c.weakTypeOf[scala.async.iterators.IteratorState[A]]

    def execContextType: Type = c.weakTypeOf[Unit]
    def resultType[A: WeakTypeTag]: Type = c.weakTypeOf[Result[A]]

    /**
     * @param tree ident referring to state machine
     */
    override def spawn(tree: Tree): Tree = {
      // don't call future here, but return state machine
      tree
    }

    def execContext: Expr[ExecContext] = c.literalUnit

    def castTo[A: WeakTypeTag](future: Expr[Fut[Any]]): Expr[Fut[A]] = ???

    def completeProm[A: WeakTypeTag](prom: Expr[Prom[A]], value: Expr[A]): Expr[Unit] = reify {
      prom.splice.result = value.splice
    }

    def completePromWithExceptionTopLevel[A: WeakTypeTag](prom: Expr[Prom[A]], exception: Expr[Throwable]): Expr[Unit] =
      reify { ??? }

    def completePromWithFailedResult[A: WeakTypeTag](prom: Expr[Prom[A]], resultName: TermName): Expr[Unit] =
      reify { () }

    def createProm[A: WeakTypeTag]: Expr[Prom[A]] = ???

    def createPromTree[A: WeakTypeTag](stateMachine: Tree): Tree = {
      // return `this` state machine
      This(tpnme.EMPTY)
    }

    def future[A: WeakTypeTag](a: Expr[A])(execContext: Expr[ExecContext]): Expr[Fut[A]] =
      reify { ??? }

    def onComplete[A, U](future: Expr[Fut[A]], fun: Expr[Result[A] => U],
                         execContext: Expr[ExecContext]): Expr[Unit] = reify {
      /* do nothing */
    }

    def promiseToFuture[A: WeakTypeTag](prom: Expr[Prom[A]]): Expr[Fut[A]] = prom

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

/*
    lazy val IS_result = methodSym(reify((null: scala.async.iterators.IteratorState[Any]).result))
    lazy val IS_isFailed = methodSym(reify((null: scala.async.iterators.IteratorState[Any]).isFailed))

    // <resultName> = name.result.asInstanceOf[<resultType>]
    def resultValue(name: TermName, resultType: Type): Tree =
      TypeApply(Select(Select(Ident(name), IS_result), newTermName("asInstanceOf")), List(TypeTree(resultType)))

    def isFailedResult(name: TermName): Expr[Boolean] =
      c.Expr[Boolean](Select(Ident(name), IS_isFailed))
*/
  }

}
