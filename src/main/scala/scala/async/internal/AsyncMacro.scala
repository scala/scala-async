package scala.async.internal

object AsyncMacro {
  def apply(c0: reflect.macros.Context, base: AsyncBase)(body0: c0.Tree): AsyncMacro { val c: c0.type } = {
    import language.reflectiveCalls

    // Use an attachment on RootClass as a sneaky place for a per-Global cache
    val att = c0.internal.attachments(c0.universe.rootMirror.RootClass)
    val names = att.get[AsyncNames[_]].getOrElse {
      val names = new AsyncNames[c0.universe.type](c0.universe)
      att.update(names)
      names
    }

    new AsyncMacro { self =>
      val c: c0.type                                             = c0
      val asyncNames: AsyncNames[c.universe.type]                = names.asInstanceOf[AsyncNames[c.universe.type]]
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
