package scala.async.internal

object AsyncMacro {
  def apply(c0: reflect.macros.Context, base: AsyncBase): AsyncMacro { val c: c0.type } = {
    import language.reflectiveCalls
    new AsyncMacro { self =>
      val c: c0.type                                             = c0
      // This member is required by `AsyncTransform`:
      val asyncBase: AsyncBase                                   = base
      // These members are required by `ExprBuilder`:
      val futureSystem: FutureSystem                             = base.futureSystem
      val futureSystemOps: futureSystem.Ops {val c: self.c.type} = futureSystem.mkOps(c)
    }
  }
}

private[async] trait AsyncMacro
  extends AnfTransform with TransformUtils with Lifter
  with ExprBuilder with AsyncTransform with AsyncAnalysis with LiveVariables {

  val c: scala.reflect.macros.Context

  lazy val macroPos = c.macroApplication.pos.makeTransparent
  def atMacroPos(t: c.Tree) = c.universe.atPos(macroPos)(t)
}
