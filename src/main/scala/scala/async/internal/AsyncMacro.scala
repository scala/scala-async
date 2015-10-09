package scala.async.internal

object AsyncMacro {
  def apply(c0: reflect.macros.Context, base: AsyncBase)(body0: c0.Tree): AsyncMacro { val c: c0.type } = {
    import language.reflectiveCalls
    new AsyncMacro { self =>
      val c: c0.type                                             = c0
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

  lazy val macroPos = c.macroApplication.pos.makeTransparent
  def atMacroPos(t: c.Tree) = c.universe.atPos(macroPos)(t)

}
