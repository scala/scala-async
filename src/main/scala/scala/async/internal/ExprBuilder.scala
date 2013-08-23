/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async.internal

import scala.reflect.macros.Context
import scala.collection.mutable.ListBuffer
import collection.mutable
import language.existentials
import scala.reflect.api.Universe
import scala.reflect.api

trait ExprBuilder {
  builder: AsyncMacro =>

  import global._
  import defn._

  val futureSystem: FutureSystem
  val futureSystemOps: futureSystem.Ops { val universe: global.type }

  val stateAssigner  = new StateAssigner
  val labelDefStates = collection.mutable.Map[Symbol, Int]()

  trait AsyncState {
    def state: Int

    def mkHandlerCaseForState: CaseDef

    def mkOnCompleteHandler[T: WeakTypeTag]: Option[CaseDef] = None

    def stats: List[Tree]

    final def allStats: List[Tree] = this match {
      case a: AsyncStateWithAwait => stats :+ a.awaitable.resultValDef
      case _ => stats
    }

    final def body: Tree = stats match {
      case stat :: Nil => stat
      case init :+ last => Block(init, last)
    }
  }

  /** A sequence of statements the concludes with a unconditional transition to `nextState` */
  final class SimpleAsyncState(val stats: List[Tree], val state: Int, nextState: Int, symLookup: SymLookup)
    extends AsyncState {

    def mkHandlerCaseForState: CaseDef =
      mkHandlerCase(state, stats :+ mkStateTree(nextState, symLookup) :+ mkResumeApply(symLookup))

    override val toString: String =
      s"AsyncState #$state, next = $nextState"
  }

  /** A sequence of statements with a conditional transition to the next state, which will represent
    * a branch of an `if` or a `match`.
    */
  final class AsyncStateWithoutAwait(val stats: List[Tree], val state: Int) extends AsyncState {
    override def mkHandlerCaseForState: CaseDef =
      mkHandlerCase(state, stats)

    override val toString: String =
      s"AsyncStateWithoutAwait #$state"
  }

  /** A sequence of statements that concludes with an `await` call. The `onComplete`
    * handler will unconditionally transition to `nestState`.``
    */
  final class AsyncStateWithAwait(val stats: List[Tree], val state: Int, nextState: Int,
                                  val awaitable: Awaitable, symLookup: SymLookup)
    extends AsyncState {

    override def mkHandlerCaseForState: CaseDef = {
      val callOnComplete = futureSystemOps.onComplete(Expr(awaitable.expr),
        Expr(This(tpnme.EMPTY)), Expr(Ident(name.execContext))).tree
      mkHandlerCase(state, stats :+ callOnComplete)
    }

    override def mkOnCompleteHandler[T: WeakTypeTag]: Option[CaseDef] = {
      val tryGetTree =
        Assign(
          Ident(awaitable.resultName),
          TypeApply(Select(Select(Ident(symLookup.applyTrParam), Try_get), newTermName("asInstanceOf")), List(TypeTree(awaitable.resultType)))
        )

      /* if (tr.isFailure)
       *   result.complete(tr.asInstanceOf[Try[T]])
       * else {
       *   <resultName> = tr.get.asInstanceOf[<resultType>]
       *   <nextState>
       *   <mkResumeApply>
       * }
       */
      val ifIsFailureTree =
        If(Select(Ident(symLookup.applyTrParam), Try_isFailure),
           futureSystemOps.completeProm[T](
             Expr[futureSystem.Prom[T]](symLookup.memberRef(name.result)),
             Expr[scala.util.Try[T]](
               TypeApply(Select(Ident(symLookup.applyTrParam), newTermName("asInstanceOf")),
                         List(TypeTree(weakTypeOf[scala.util.Try[T]]))))).tree,
           Block(List(tryGetTree, mkStateTree(nextState, symLookup)), mkResumeApply(symLookup))
         )

      Some(mkHandlerCase(state, List(ifIsFailureTree)))
    }

    override val toString: String =
      s"AsyncStateWithAwait #$state, next = $nextState"
  }

  /*
   * Builder for a single state of an async method.
   */
  final class AsyncStateBuilder(state: Int, private val symLookup: SymLookup) {
    /* Statements preceding an await call. */
    private val stats                      = ListBuffer[Tree]()
    /** The state of the target of a LabelDef application (while loop jump) */
    private var nextJumpState: Option[Int] = None

    def +=(stat: Tree): this.type = {
      assert(nextJumpState.isEmpty, s"statement appeared after a label jump: $stat")
      def addStat() = stats += stat
      stat match {
        case Apply(fun, Nil) =>
          labelDefStates get fun.symbol match {
            case Some(nextState) => nextJumpState = Some(nextState)
            case None            => addStat()
          }
        case _               => addStat()
      }
      this
    }

    def resultWithAwait(awaitable: Awaitable,
                        nextState: Int): AsyncState = {
      val effectiveNextState = nextJumpState.getOrElse(nextState)
      new AsyncStateWithAwait(stats.toList, state, effectiveNextState, awaitable, symLookup)
    }

    def resultSimple(nextState: Int): AsyncState = {
      val effectiveNextState = nextJumpState.getOrElse(nextState)
      new SimpleAsyncState(stats.toList, state, effectiveNextState, symLookup)
    }

    def resultWithIf(condTree: Tree, thenState: Int, elseState: Int): AsyncState = {
      def mkBranch(state: Int) = Block(mkStateTree(state, symLookup) :: Nil, mkResumeApply(symLookup))
      this += If(condTree, mkBranch(thenState), mkBranch(elseState))
      new AsyncStateWithoutAwait(stats.toList, state)
    }

    /**
     * Build `AsyncState` ending with a match expression.
     *
     * The cases of the match simply resume at the state of their corresponding right-hand side.
     *
     * @param scrutTree       tree of the scrutinee
     * @param cases           list of case definitions
     * @param caseStates      starting state of the right-hand side of the each case
     * @return                an `AsyncState` representing the match expression
     */
    def resultWithMatch(scrutTree: Tree, cases: List[CaseDef], caseStates: List[Int], symLookup: SymLookup): AsyncState = {
      // 1. build list of changed cases
      val newCases = for ((cas, num) <- cases.zipWithIndex) yield cas match {
        case CaseDef(pat, guard, rhs) =>
          val bindAssigns = rhs.children.takeWhile(isSyntheticBindVal)
          CaseDef(pat, guard, Block(bindAssigns :+ mkStateTree(caseStates(num), symLookup), mkResumeApply(symLookup)))
      }
      // 2. insert changed match tree at the end of the current state
      this += Match(scrutTree, newCases)
      new AsyncStateWithoutAwait(stats.toList, state)
    }

    def resultWithLabel(startLabelState: Int, symLookup: SymLookup): AsyncState = {
      this += Block(mkStateTree(startLabelState, symLookup) :: Nil, mkResumeApply(symLookup))
      new AsyncStateWithoutAwait(stats.toList, state)
    }

    override def toString: String = {
      val statsBeforeAwait = stats.mkString("\n")
      s"ASYNC STATE:\n$statsBeforeAwait"
    }
  }

  /**
   * An `AsyncBlockBuilder` builds a `ListBuffer[AsyncState]` based on the expressions of a `Block(stats, expr)` (see `Async.asyncImpl`).
   *
   * @param stats       a list of expressions
   * @param expr        the last expression of the block
   * @param startState  the start state
   * @param endState    the state to continue with
   */
  final private class AsyncBlockBuilder(stats: List[Tree], expr: Tree, startState: Int, endState: Int,
                                        private val symLookup: SymLookup) {
    val asyncStates = ListBuffer[AsyncState]()

    var stateBuilder = new AsyncStateBuilder(startState, symLookup)
    var currState    = startState

    def checkForUnsupportedAwait(tree: Tree) = if (tree exists {
      case Apply(fun, _) if isAwait(fun) => true
      case _                             => false
    }) abort(tree.pos, "await must not be used in this position")

    def nestedBlockBuilder(nestedTree: Tree, startState: Int, endState: Int) = {
      val (nestedStats, nestedExpr) = statsAndExpr(nestedTree)
      new AsyncBlockBuilder(nestedStats, nestedExpr, startState, endState, symLookup)
    }

    import stateAssigner.nextState

    // populate asyncStates
    for (stat <- stats) stat match {
      // the val name = await(..) pattern
      case vd @ ValDef(mods, name, tpt, Apply(fun, arg :: Nil)) if isAwait(fun) =>
        val afterAwaitState = nextState()
        val awaitable = Awaitable(arg, stat.symbol, tpt.tpe, vd)
        asyncStates += stateBuilder.resultWithAwait(awaitable, afterAwaitState) // complete with await
        currState = afterAwaitState
        stateBuilder = new AsyncStateBuilder(currState, symLookup)

      case If(cond, thenp, elsep) if stat exists isAwait =>
        checkForUnsupportedAwait(cond)

        val thenStartState = nextState()
        val elseStartState = nextState()
        val afterIfState = nextState()

        asyncStates +=
          // the two Int arguments are the start state of the then branch and the else branch, respectively
          stateBuilder.resultWithIf(cond, thenStartState, elseStartState)

        List((thenp, thenStartState), (elsep, elseStartState)) foreach {
          case (branchTree, state) =>
            val builder = nestedBlockBuilder(branchTree, state, afterIfState)
            asyncStates ++= builder.asyncStates
        }

        currState = afterIfState
        stateBuilder = new AsyncStateBuilder(currState, symLookup)

      case Match(scrutinee, cases) if stat exists isAwait =>
        checkForUnsupportedAwait(scrutinee)

        val caseStates = cases.map(_ => nextState())
        val afterMatchState = nextState()

        asyncStates +=
          stateBuilder.resultWithMatch(scrutinee, cases, caseStates, symLookup)

        for ((cas, num) <- cases.zipWithIndex) {
          val (stats, expr) = statsAndExpr(cas.body)
          val stats1 = stats.dropWhile(isSyntheticBindVal)
          val builder = nestedBlockBuilder(Block(stats1, expr), caseStates(num), afterMatchState)
          asyncStates ++= builder.asyncStates
        }

        currState = afterMatchState
        stateBuilder = new AsyncStateBuilder(currState, symLookup)

      case ld@LabelDef(name, params, rhs) if rhs exists isAwait =>
        val startLabelState = nextState()
        val afterLabelState = nextState()
        asyncStates += stateBuilder.resultWithLabel(startLabelState, symLookup)
        labelDefStates(ld.symbol) = startLabelState
        val builder = nestedBlockBuilder(rhs, startLabelState, afterLabelState)
        asyncStates ++= builder.asyncStates

        currState = afterLabelState
        stateBuilder = new AsyncStateBuilder(currState, symLookup)
      case _                                                    =>
        checkForUnsupportedAwait(stat)
        stateBuilder += stat
    }
    // complete last state builder (representing the expressions after the last await)
    stateBuilder += expr
    val lastState = stateBuilder.resultSimple(endState)
    asyncStates += lastState
  }

  trait AsyncBlock {
    def asyncStates: List[AsyncState]

    def onCompleteHandler[T: WeakTypeTag]: Tree

    def resumeFunTree[T: WeakTypeTag]: DefDef
  }

  case class SymLookup(stateMachineClass: Symbol, applyTrParam: Symbol) {
    def stateMachineMember(name: TermName): Symbol =
      stateMachineClass.info.member(name)
    def memberRef(name: TermName): Tree =
      gen.mkAttributedRef(stateMachineMember(name))
  }

  def buildAsyncBlock(block: Block, symLookup: SymLookup): AsyncBlock = {
    val Block(stats, expr) = block
    val startState = stateAssigner.nextState()
    val endState = Int.MaxValue

    val blockBuilder = new AsyncBlockBuilder(stats, expr, startState, endState, symLookup)

    new AsyncBlock {
      def asyncStates = blockBuilder.asyncStates.toList

      def mkCombinedHandlerCases[T: WeakTypeTag]: List[CaseDef] = {
        val caseForLastState: CaseDef = {
          val lastState = asyncStates.last
          val lastStateBody = Expr[T](lastState.body)
          val rhs = futureSystemOps.completeProm(
            Expr[futureSystem.Prom[T]](symLookup.memberRef(name.result)), reify(scala.util.Success[T](lastStateBody.splice)))
          mkHandlerCase(lastState.state, rhs.tree)
        }
        asyncStates.toList match {
          case s :: Nil =>
            List(caseForLastState)
          case _        =>
            val initCases = for (state <- asyncStates.toList.init) yield state.mkHandlerCaseForState
            initCases :+ caseForLastState
        }
      }

      val initStates = asyncStates.init

      /**
       * def resume(): Unit = {
       *   try {
       *     state match {
       *       case 0 => {
       *         f11 = exprReturningFuture
       *         f11.onComplete(onCompleteHandler)(context)
       *       }
       *       ...
       *     }
       *   } catch {
       *     case NonFatal(t) => result.failure(t)
       *   }
       * }
       */
      def resumeFunTree[T: WeakTypeTag]: DefDef =
        DefDef(Modifiers(), name.resume, Nil, List(Nil), Ident(definitions.UnitClass),
          Try(
            Match(symLookup.memberRef(name.state), mkCombinedHandlerCases[T]),
            List(
              CaseDef(
                Bind(name.t, Ident(nme.WILDCARD)),
                Apply(Ident(defn.NonFatalClass), List(Ident(name.t))),
                Block(List({
                  val t = Expr[Throwable](Ident(name.t))
                  futureSystemOps.completeProm[T](
                    Expr[futureSystem.Prom[T]](symLookup.memberRef(name.result)), reify(scala.util.Failure(t.splice))).tree
                }), literalUnit))), EmptyTree))

      /**
       * assumes tr: Try[Any] is in scope.
       *
       * state match {
       *   case 0 =>
       *     x11 = tr.get.asInstanceOf[Double]
       *     state = 1
       *     resume()
       * }
       */
      def onCompleteHandler[T: WeakTypeTag]: Tree =
        Match(symLookup.memberRef(name.state), initStates.flatMap(_.mkOnCompleteHandler[T]).toList)
    }
  }

  private def isSyntheticBindVal(tree: Tree) = tree match {
    case vd@ValDef(_, lname, _, Ident(rname)) => lname.toString.contains(name.bindSuffix)
    case _                                    => false
  }

  case class Awaitable(expr: Tree, resultName: Symbol, resultType: Type, resultValDef: ValDef)

  private def mkResumeApply(symLookup: SymLookup) = Apply(symLookup.memberRef(name.resume), Nil)

  private def mkStateTree(nextState: Int, symLookup: SymLookup): Tree =
    Assign(symLookup.memberRef(name.state), Literal(Constant(nextState)))

  private def mkHandlerCase(num: Int, rhs: List[Tree]): CaseDef =
    mkHandlerCase(num, Block(rhs, literalUnit))

  private def mkHandlerCase(num: Int, rhs: Tree): CaseDef =
    CaseDef(Literal(Constant(num)), EmptyTree, rhs)

  private def literalUnit = Literal(Constant(()))
}
