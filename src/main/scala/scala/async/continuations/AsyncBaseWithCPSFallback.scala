/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package continuations

import scala.language.experimental.macros

import scala.reflect.macros.Context
import scala.util.continuations._
import scala.async.internal.{AsyncMacro, AsyncUtils}

trait AsyncBaseWithCPSFallback extends internal.AsyncBase {

  /* Fall-back for `await` using CPS plugin.
   * 
   * Note: This method is public, but is intended only for internal use.
   */
  def awaitFallback[T](awaitable: futureSystem.Fut[T]): T @cps[futureSystem.Fut[Any]]

  override protected[async] def fallbackEnabled = true

  /* Implements `async { ... }` using the CPS plugin.
   */
  protected def cpsBasedAsyncImpl[T: c.WeakTypeTag](c: Context)
                                                   (body: c.Expr[T])
                                                   (execContext: c.Expr[futureSystem.ExecContext]): c.Expr[futureSystem.Fut[T]] = {
    import c.universe._

    def lookupClassMember(clazz: String, name: String) = {
      val asyncTrait = c.mirror.staticClass(clazz)
      val tpe = asyncTrait.asType.toType
      tpe.member(newTermName(name)).ensuring(_ != NoSymbol, s"$clazz.$name")
    }
    def lookupObjectMember(clazz: String, name: String) = {
      val moduleClass = c.mirror.staticModule(clazz).moduleClass
      val tpe = moduleClass.asType.toType
      tpe.member(newTermName(name)).ensuring(_ != NoSymbol, s"$clazz.$name")
    }

    AsyncUtils.vprintln("AsyncBaseWithCPSFallback.cpsBasedAsyncImpl")

    val symTab           = c.universe.asInstanceOf[reflect.internal.SymbolTable]
    val futureSystemOps  = futureSystem.mkOps(symTab)
    val awaitSym         = lookupObjectMember("scala.async.Async", "await")
    val awaitFallbackSym = lookupClassMember("scala.async.continuations.AsyncBaseWithCPSFallback", "awaitFallback")

    // replace `await` invocations with `awaitFallback` invocations
    val awaitReplacer = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Apply(fun @ TypeApply(_, List(futArgTpt)), args) if fun.symbol == awaitSym =>
          val typeApp = treeCopy.TypeApply(fun, atPos(tree.pos)(Ident(awaitFallbackSym)), List(atPos(tree.pos)(TypeTree(futArgTpt.tpe))))
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

    def spawn(expr: Tree) = futureSystemOps.spawn(expr.asInstanceOf[futureSystemOps.universe.Tree], execContext.tree.asInstanceOf[futureSystemOps.universe.Tree]).asInstanceOf[Tree]

    val bodyWithFuture = {
      val tree = bodyWithAwaitFallback match {
        case Block(stmts, expr) => Block(stmts, spawn(expr))
        case expr               => spawn(expr)
      }
      c.Expr[futureSystem.Fut[Any]](c.resetAllAttrs(tree.duplicate))
    }

    val bodyWithReset: c.Expr[futureSystem.Fut[Any]] = reify {
      reset { bodyWithFuture.splice }
    }
    val bodyWithCast = futureSystemOps.castTo[T](bodyWithReset.asInstanceOf[futureSystemOps.universe.Expr[futureSystem.Fut[Any]]]).asInstanceOf[c.Expr[futureSystem.Fut[T]]]

    AsyncUtils.vprintln(s"CPS-based async transform expands to:\n${bodyWithCast.tree}")
    bodyWithCast
  }

  override def asyncImpl[T: c.WeakTypeTag](c: Context)
                                          (body: c.Expr[T])
                                          (execContext: c.Expr[futureSystem.ExecContext]): c.Expr[futureSystem.Fut[T]] = {
    AsyncUtils.vprintln("AsyncBaseWithCPSFallback.asyncImpl")

    val asyncMacro = AsyncMacro(c, this)

    if (!asyncMacro.reportUnsupportedAwaits(body.tree.asInstanceOf[asyncMacro.global.Tree], report = fallbackEnabled))
      super.asyncImpl[T](c)(body)(execContext)   // no unsupported awaits
    else
      cpsBasedAsyncImpl[T](c)(body)(execContext) // fallback to CPS
  }
}
