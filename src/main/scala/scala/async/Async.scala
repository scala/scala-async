/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Future, Promise, ExecutionContext, future}
import ExecutionContext.Implicits.global
import scala.util.control.NonFatal


/*
 * @author Philipp Haller
 */
object Async extends AsyncBase {
  lazy val futureSystem = ScalaConcurrentFutureSystem
  type FS = ScalaConcurrentFutureSystem.type

  def async[T](body: T) = macro asyncImpl[T]

  override def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[Future[T]] = super.asyncImpl[T](c)(body)
}

object AsyncId extends AsyncBase {
  lazy val futureSystem = IdentityFutureSystem
  type FS = IdentityFutureSystem.type

  def async[T](body: T) = macro asyncImpl[T]

  override def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[T] = super.asyncImpl[T](c)(body)
}

/**
 * A base class for the `async` macro. Subclasses must provide:
 *
 * - Concrete types for a given future system
 * - Tree manipulations to create and complete the equivalent of Future and Promise
 * in that system.
 * - The `async` macro declaration itself, and a forwarder for the macro implementation.
 * (The latter is temporarily needed to workaround bug SI-6650 in the macro system)
 *
 * The default implementation, [[scala.async.Async]], binds the macro to `scala.concurrent._`.
 */
abstract class AsyncBase {
  self =>

  type FS <: FutureSystem
  val futureSystem: FS

  /**
   * A call to `await` must be nested in an enclosing `async` block.
   * 
   * A call to `await` does not block the current thread, rather it is a delimiter
   * used by the enclosing `async` macro. Code following the `await`
   * call is executed asynchronously, when the argument of `await` has been completed.
   * 
   * @param awaitable the future from which a value is awaited.
   * @tparam T        the type of that value.
   * @return          the value.
   */
  // TODO Replace with `@compileTimeOnly when this is implemented SI-6539
  @deprecated("`await` must be enclosed in an `async` block", "0.1")
  def await[T](awaitable: futureSystem.Fut[T]): T = ???

  def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[futureSystem.Fut[T]] = {
    import c.universe._
    import Flag._

    val builder = new ExprBuilder[c.type, futureSystem.type](c, self.futureSystem)

    import builder.defn._
    import builder.name
    import builder.futureSystemOps

    val transform = new AnfTransform[c.type](c)
    val typedBody = c.typeCheck(body.tree)
    val stats1 :+ expr1 = transform.anf.transformToList(typedBody)
    val btree = c.typeCheck(Block(stats1, expr1))

    AsyncUtils.vprintln(s"In file '${c.macroApplication.pos.source.path}':")
    AsyncUtils.vprintln(s"${c.macroApplication}")
    AsyncUtils.vprintln(s"ANF transform expands to:\n $btree")

    val (stats, expr) = btree match {
      case Block(stats, expr) => (stats, expr)
      case tree => (Nil, tree)
    }

    val asyncBlockBuilder = new builder.AsyncBlockBuilder(stats, expr, 0, 1000, 1000, Map())

    asyncBlockBuilder.asyncStates foreach (s => AsyncUtils.vprintln(s))

    val handlerCases: List[CaseDef] = asyncBlockBuilder.mkCombinedHandlerCases[T]()

    val initStates = asyncBlockBuilder.asyncStates.init
    val localVarTrees = asyncBlockBuilder.asyncStates.flatMap(_.allVarDefs).toList

    /*
      lazy val onCompleteHandler = (tr: Try[Any]) => state match {
        case 0 => {
          x11 = tr.get.asInstanceOf[Double];
          state = 1;
          resume()
        }
        ...
     */
    val onCompleteHandler = {
      val onCompleteHandlers = initStates.flatMap(_.mkOnCompleteHandler).toList
      Function(
        List(ValDef(Modifiers(PARAM), name.tr, TypeTree(TryAnyType), EmptyTree)),
        Match(Ident(name.state), onCompleteHandlers))
    }

    /*
      def resume(): Unit = {
        try {
          state match {
            case 0 => {
               f11 = exprReturningFuture
               f11.onComplete(onCompleteHandler)(context)
             }
            ...
           }
        } catch {
          case NonFatal(t) => result.failure(t)
        }
      }
     */
    val resumeFunTree: c.Tree = DefDef(Modifiers(), name.resume, Nil, List(Nil), Ident(definitions.UnitClass),
      Try(
        Match(Ident(name.state), handlerCases),
        List(
          CaseDef(
            Apply(Ident(NonFatalClass), List(Bind(name.tr, Ident(nme.WILDCARD)))),
            EmptyTree,
            Block(List({
              val t = c.Expr[Throwable](Ident(name.tr))
              futureSystemOps.completeProm[T](c.Expr[futureSystem.Prom[T]](Ident(name.result)), reify(scala.util.Failure(t.splice))).tree
            }), c.literalUnit.tree))), EmptyTree))


    val prom: Expr[futureSystem.Prom[T]] = reify {
      // Create the empty promise
      val result$async = futureSystemOps.createProm[T].splice
      // Initialize the state
      var state$async = 0
      // Resolve the execution context
      val execContext$async = futureSystemOps.execContext.splice
      var onCompleteHandler$async: util.Try[Any] => Unit = null

      // Spawn a future to:
      futureSystemOps.future[Unit] {
        c.Expr[Unit](Block(
          // define vars for all intermediate results
          localVarTrees :+
            // define the resume() method
            resumeFunTree :+
            // assign onComplete function. (The var breaks the circular dependency with resume)`
            Assign(Ident(name.onCompleteHandler), onCompleteHandler),
          // and get things started by calling resume()
          Apply(Ident(name.resume), Nil)))
      }(c.Expr[futureSystem.ExecContext](Ident(name.execContext))).splice
      // Return the promise from this reify block...
      result$async
    }
    // ... and return its Future from the macro.
    val result = futureSystemOps.promiseToFuture(prom)

    AsyncUtils.vprintln(s"async state machine transform expands to:\n ${result.tree}")

    result
  }
}
