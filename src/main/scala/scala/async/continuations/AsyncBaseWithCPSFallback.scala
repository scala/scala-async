/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package continuations

import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.util.continuations._

trait AsyncBaseWithCPSFallback extends AsyncBase {

  /* Fall-back for `await` using CPS plugin.
   * 
   * Note: This method is public, but is intended only for internal use.
   */
  def awaitFallback[T](awaitable: futureSystem.Fut[T]): T @cps[futureSystem.Fut[Any]]

  override protected[async] def fallbackEnabled = true

  /* Implements `async { ... }` using the CPS plugin.
   */
  protected def cpsBasedAsyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[futureSystem.Fut[T]] = {
    import c.universe._

    def lookupMember(name: String) = {
      val asyncTrait = c.mirror.staticClass("scala.async.continuations.AsyncBaseWithCPSFallback")
      val tpe = asyncTrait.asType.toType
      tpe.member(newTermName(name)).ensuring(_ != NoSymbol)
    }

    AsyncUtils.vprintln("AsyncBaseWithCPSFallback.cpsBasedAsyncImpl")

    val utils            = TransformUtils[c.type](c)
    val futureSystemOps  = futureSystem.mkOps(c)
    val awaitSym         = utils.defn.Async_await
    val awaitFallbackSym = lookupMember("awaitFallback")

    // replace `await` invocations with `awaitFallback` invocations
    val awaitReplacer = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Apply(fun @ TypeApply(_, List(futArgTpt)), args) if fun.symbol == awaitSym =>
          val typeApp = treeCopy.TypeApply(fun, Ident(awaitFallbackSym), List(TypeTree(futArgTpt.tpe)))
          treeCopy.Apply(tree, typeApp, args.map(arg => c.resetAllAttrs(arg.duplicate)))
        case _ =>
          super.transform(tree)
      }
    }
    val bodyWithAwaitFallback = awaitReplacer.transform(body.tree)

    /* generate an expression that looks like this:
         reset {
           val f = future { ... }
           ...
           val x = awaitFallback(f)
           ...
           future { expr }
         }.asInstanceOf[Future[T]]
     */

    val bodyWithFuture = {
      val tree = bodyWithAwaitFallback match {
        case Block(stmts, expr) => Block(stmts, futureSystemOps.spawn(expr))
        case expr               => futureSystemOps.spawn(expr)
      }
      c.Expr[futureSystem.Fut[Any]](c.resetAllAttrs(tree.duplicate))
    }

    val bodyWithReset: c.Expr[futureSystem.Fut[Any]] = reify {
      reset { bodyWithFuture.splice }
    }
    val bodyWithCast = futureSystemOps.castTo[T](bodyWithReset)

    AsyncUtils.vprintln(s"CPS-based async transform expands to:\n${bodyWithCast.tree}")
    bodyWithCast
  }

  override def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[futureSystem.Fut[T]] = {
    AsyncUtils.vprintln("AsyncBaseWithCPSFallback.asyncImpl")

    val analyzer = AsyncAnalysis[c.type](c, this)

    if (!analyzer.reportUnsupportedAwaits(body.tree))
      super.asyncImpl[T](c)(body)   // no unsupported awaits
    else
      cpsBasedAsyncImpl[T](c)(body) // fallback to CPS
  }
}
