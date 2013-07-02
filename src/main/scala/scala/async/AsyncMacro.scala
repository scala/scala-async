package scala.async

import scala.tools.nsc.Global
import scala.tools.nsc.transform.TypingTransformers

object AsyncMacro {
  def apply(c: reflect.macros.Context, futureSystem0: FutureSystem): AsyncMacro = {
    import language.reflectiveCalls
    val powerContext = c.asInstanceOf[c.type {val universe: Global; val callsiteTyper: universe.analyzer.Typer}]
    new AsyncMacro {
      val global: powerContext.universe.type = powerContext.universe
      val callSiteTyper: global.analyzer.Typer = powerContext.callsiteTyper
      val futureSystem: futureSystem0.type = futureSystem0
      val futureSystemOps: futureSystem.Ops {val universe: global.type} = futureSystem0.mkOps(global)
      val macroApplication: global.Tree = c.macroApplication.asInstanceOf[global.Tree]
    }
  }
}

private[async] trait AsyncMacro
  extends TypingTransformers
  with AnfTransform with TransformUtils with Lifter
  with ExprBuilder with AsyncTransform with AsyncAnalysis {

  val global: Global
  val callSiteTyper: global.analyzer.Typer
  val macroApplication: global.Tree

}
