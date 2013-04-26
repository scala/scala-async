/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async

import scala.language.experimental.macros
import scala.reflect.macros.Context
import scala.reflect.internal.annotations.compileTimeOnly

object Async extends AsyncBase {

  import scala.concurrent.Future

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
  @compileTimeOnly("`await` must be enclosed in an `async` block")
  def await[T](awaitable: futureSystem.Fut[T]): T = ???

  protected[async] def fallbackEnabled = false

  def asyncImpl[T: c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Expr[futureSystem.Fut[T]] = {
    import c.universe._

    val analyzer = AsyncAnalysis[c.type](c, this)
    val utils = TransformUtils[c.type](c)
    import utils.{name, defn}

    analyzer.reportUnsupportedAwaits(body.tree)

    // Transform to A-normal form:
    //  - no await calls in qualifiers or arguments,
    //  - if/match only used in statement position.
    val anfTree: Block = {
      val anf = AnfTransform[c.type](c)
      val restored = utils.restorePatternMatchingFunctions(body.tree)
      val stats1 :+ expr1 = anf(restored)
      val block = Block(stats1, expr1)
      c.typeCheck(block).asInstanceOf[Block]
    }

    // Analyze the block to find locals that will be accessed from multiple
    // states of our generated state machine, e.g. a value assigned before
    // an `await` and read afterwards.
    val renameMap: Map[Symbol, TermName] = {
      analyzer.defTreesUsedInSubsequentStates(anfTree).map {
        vd =>
          (vd.symbol, name.fresh(vd.name.toTermName))
      }.toMap
    }

    val builder = ExprBuilder[c.type, futureSystem.type](c, self.futureSystem, anfTree)
    import builder.futureSystemOps
    val asyncBlock: builder.AsyncBlock = builder.build(anfTree, renameMap)
    import asyncBlock.asyncStates
    logDiagnostics(c)(anfTree, asyncStates.map(_.toString))

    // Important to retain the original declaration order here!
    val localVarTrees = anfTree.collect {
      case vd@ValDef(_, _, tpt, _) if renameMap contains vd.symbol                            =>
        utils.mkVarDefTree(tpt.tpe, renameMap(vd.symbol))
      case dd@DefDef(mods, name, tparams, vparamss, tpt, rhs) if renameMap contains dd.symbol =>
        DefDef(mods, renameMap(dd.symbol), tparams, vparamss, tpt, c.resetAllAttrs(utils.substituteNames(rhs, renameMap)))
    }

    val onCompleteHandler = {
      Function(
        List(ValDef(Modifiers(Flag.PARAM), name.tr, TypeTree(futureSystemOps.resultType[Any]), EmptyTree)),
        asyncBlock.onCompleteHandler)
    }
    val resumeFunTree = asyncBlock.resumeFunTree[T]

    val stateMachineType = utils.applied("scala.async.StateMachine", List(futureSystemOps.promType[T], futureSystemOps.execContextType))

    lazy val stateMachine: ClassDef = {
      val body: List[Tree] = {
        val stateVar = ValDef(Modifiers(Flag.MUTABLE), name.state, TypeTree(definitions.IntTpe), Literal(Constant(0)))
        val result = ValDef(NoMods, name.result, TypeTree(futureSystemOps.promType[T]), futureSystemOps.createProm[T].tree)
        val execContext = ValDef(NoMods, name.execContext, TypeTree(), futureSystemOps.execContext.tree)
        val applyDefDef: DefDef = {
          val applyVParamss = List(List(ValDef(Modifiers(Flag.PARAM), name.tr, TypeTree(futureSystemOps.resultType[Any]), EmptyTree)))
          val applyBody = asyncBlock.onCompleteHandler
          DefDef(NoMods, name.apply, Nil, applyVParamss, TypeTree(definitions.UnitTpe), applyBody)
        }
        val apply0DefDef: DefDef = {
          // We extend () => Unit so we can pass this class as the by-name argument to `Future.apply`.
          // See SI-1247 for the the optimization that avoids creatio
          val applyVParamss = List(List(ValDef(Modifiers(Flag.PARAM), name.tr, TypeTree(futureSystemOps.resultType[Any]), EmptyTree)))
          val applyBody = asyncBlock.onCompleteHandler
          DefDef(NoMods, name.apply, Nil, Nil, TypeTree(definitions.UnitTpe), Apply(Ident(name.resume), Nil))
        }
        List(utils.emptyConstructor, stateVar, result, execContext) ++ localVarTrees ++ List(resumeFunTree, applyDefDef, apply0DefDef)
      }
      val template = {
        Template(List(stateMachineType), emptyValDef, body)
      }
      ClassDef(NoMods, name.stateMachineT, Nil, template)
    }

    def selectStateMachine(selection: TermName) = Select(Ident(name.stateMachine), selection)

    val code: c.Expr[futureSystem.Fut[T]] = {
      val isSimple = asyncStates.size == 1
      val tree =
        if (isSimple)
          Block(Nil, futureSystemOps.spawn(body.tree)) // generate lean code for the simple case of `async { 1 + 1 }`
        else {
          Block(List[Tree](
            stateMachine,
            ValDef(NoMods, name.stateMachine, stateMachineType, Apply(Select(New(Ident(name.stateMachineT)), nme.CONSTRUCTOR), Nil)),
            futureSystemOps.spawn(Apply(selectStateMachine(name.apply), Nil))
          ),
          futureSystemOps.promiseToFuture(c.Expr[futureSystem.Prom[T]](selectStateMachine(name.result))).tree)
        }
      c.Expr[futureSystem.Fut[T]](tree)
    }

    AsyncUtils.vprintln(s"async state machine transform expands to:\n ${code.tree}")
    code
  }

  def logDiagnostics(c: Context)(anfTree: c.Tree, states: Seq[String]) {
    def location = try {
      c.macroApplication.pos.source.path
    } catch {
      case _: UnsupportedOperationException =>
        c.macroApplication.pos.toString
    }

    AsyncUtils.vprintln(s"In file '$location':")
    AsyncUtils.vprintln(s"${c.macroApplication}")
    AsyncUtils.vprintln(s"ANF transform expands to:\n $anfTree")
    states foreach (s => AsyncUtils.vprintln(s))
  }
}

/** Internal class used by the `async` macro; should not be manually extended by client code */
abstract class StateMachine[Result, EC] extends (scala.util.Try[Any] => Unit) with (() => Unit) {
  def result$async: Result

  def execContext$async: EC
}
