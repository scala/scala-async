package scala.async.internal

import java.util

object AsyncMacro {
  private val nameCache = new util.WeakHashMap[Object, AsyncNames[_]]()
  def apply(c0: reflect.macros.Context, base: AsyncBase)(body0: c0.Tree): AsyncMacro { val c: c0.type } = {
    import language.reflectiveCalls
    val asyncNames0 = nameCache.synchronized[AsyncNames[_]] {
      nameCache.computeIfAbsent(c0.universe, new java.util.function.Function[Object, AsyncNames[_]] {
        override def apply(t: Object): AsyncNames[_] = new AsyncNames[c0.universe.type](c0.universe)
      })
    }
    new AsyncMacro { self =>
      val c: c0.type                                             = c0
      val asyncNames: AsyncNames[c.universe.type]                = asyncNames0.asInstanceOf[AsyncNames[c.universe.type]]
      val body: c.Tree = body0
      // This member is required by `AsyncTransform`:
      val asyncBase: AsyncBase                                   = base
      // These members are required by `ExprBuilder`:
      val futureSystem: FutureSystem                             = base.futureSystem
      val futureSystemOps: futureSystem.Ops {val c: self.c.type} = futureSystem.mkOps(c)
      var containsAwait: c.Tree => Boolean = containsAwaitCached(body0)
    }
  }
}

private[async] trait AsyncMacro
  extends AnfTransform with TransformUtils with Lifter
  with ExprBuilder with AsyncTransform with AsyncAnalysis with LiveVariables {

  val c: scala.reflect.macros.Context
  val body: c.Tree
  var containsAwait: c.Tree => Boolean
  val asyncNames: AsyncNames[c.universe.type]

  lazy val macroPos: c.universe.Position = c.macroApplication.pos.makeTransparent
  def atMacroPos(t: c.Tree): c.Tree = c.universe.atPos(macroPos)(t)

}
