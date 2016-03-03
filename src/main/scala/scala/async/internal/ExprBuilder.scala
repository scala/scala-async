/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async.internal

import scala.collection.mutable.ListBuffer
import collection.mutable
import language.existentials
import scala.reflect.api.Universe
import scala.reflect.api

trait ExprBuilder {
  builder: AsyncMacro =>

  import c.universe._
  import defn._
  import c.internal._

  val futureSystem: FutureSystem
  val futureSystemOps: futureSystem.Ops { val c: builder.c.type }

  val stateAssigner  = new StateAssigner
  val labelDefStates = collection.mutable.Map[Symbol, Int]()

  trait AsyncState {
    def state: Int

    def nextStates: List[Int]

    def mkHandlerCaseForState[T: WeakTypeTag]: CaseDef

    def mkOnCompleteHandler[T: WeakTypeTag]: Option[CaseDef] = None

    var stats: List[Tree]

    def treesThenStats(trees: List[Tree]): List[Tree] = {
      (stats match {
        case init :+ last if tpeOf(last) =:= definitions.NothingTpe =>
          adaptToUnit((trees ::: init) :+ Typed(last, TypeTree(definitions.AnyTpe)))
        case _ =>
          adaptToUnit(trees ::: stats)
      }) :: Nil
    }

    final def allStats: List[Tree] = this match {
      case a: AsyncStateWithAwait => treesThenStats(a.awaitable.resultValDef :: Nil)
      case _ => stats
    }

    final def body: Tree = stats match {
      case stat :: Nil => stat
      case init :+ last => Block(init, last)
    }
  }

  /** A sequence of statements that concludes with a unconditional transition to `nextState` */
  final class SimpleAsyncState(var stats: List[Tree], val state: Int, nextState: Int, symLookup: SymLookup)
    extends AsyncState {

    def nextStates: List[Int] =
      List(nextState)

    def mkHandlerCaseForState[T: WeakTypeTag]: CaseDef = {
      mkHandlerCase(state, treesThenStats(mkStateTree(nextState, symLookup) :: Nil))
    }

    override val toString: String =
      s"AsyncState #$state, next = $nextState"
  }

  /** A sequence of statements with a conditional transition to the next state, which will represent
    * a branch of an `if` or a `match`.
    */
  final class AsyncStateWithoutAwait(var stats: List[Tree], val state: Int, val nextStates: List[Int]) extends AsyncState {
    override def mkHandlerCaseForState[T: WeakTypeTag]: CaseDef =
      mkHandlerCase(state, stats)

    override val toString: String =
      s"AsyncStateWithoutAwait #$state, nextStates = $nextStates"
  }

  /** A sequence of statements that concludes with an `await` call. The `onComplete`
    * handler will unconditionally transition to `nextState`.
    */
  final class AsyncStateWithAwait(var stats: List[Tree], val state: Int, onCompleteState: Int, nextState: Int,
                                  val awaitable: Awaitable, symLookup: SymLookup)
    extends AsyncState {

    def nextStates: List[Int] =
      List(nextState)

    override def mkHandlerCaseForState[T: WeakTypeTag]: CaseDef = {
      val fun = This(tpnme.EMPTY)
      val callOnComplete = futureSystemOps.onComplete[Any, Unit](c.Expr[futureSystem.Fut[Any]](awaitable.expr),
        c.Expr[futureSystem.Tryy[Any] => Unit](fun), c.Expr[futureSystem.ExecContext](Ident(name.execContext))).tree
      val tryGetOrCallOnComplete: List[Tree] =
        if (futureSystemOps.continueCompletedFutureOnSameThread) {
          val tempName = name.fresh(name.completed)
          val initTemp = ValDef(NoMods, tempName, TypeTree(futureSystemOps.tryType[Any]), futureSystemOps.getCompleted[Any](c.Expr[futureSystem.Fut[Any]](awaitable.expr)).tree)
          val ifTree = If(Apply(Select(Literal(Constant(null)), TermName("ne")), Ident(tempName) :: Nil),
            adaptToUnit(ifIsFailureTree[T](Ident(tempName)) :: Nil),
            Block(toList(callOnComplete), Return(literalUnit)))
          initTemp :: ifTree :: Nil
        } else
          toList(callOnComplete) ::: Return(literalUnit) :: Nil
      mkHandlerCase(state, stats ++ List(mkStateTree(onCompleteState, symLookup)) ++ tryGetOrCallOnComplete)
    }

    private def tryGetTree(tryReference: => Tree) =
      Assign(
        Ident(awaitable.resultName),
        TypeApply(Select(futureSystemOps.tryyGet[Any](c.Expr[futureSystem.Tryy[Any]](tryReference)).tree, newTermName("asInstanceOf")), List(TypeTree(awaitable.resultType)))
      )

    /* if (tr.isFailure)
     *   result.complete(tr.asInstanceOf[Try[T]])
     * else {
     *   <resultName> = tr.get.asInstanceOf[<resultType>]
     *   <nextState>
     *   <mkResumeApply>
     * }
     */
    def ifIsFailureTree[T: WeakTypeTag](tryReference: => Tree) =
      If(futureSystemOps.tryyIsFailure(c.Expr[futureSystem.Tryy[T]](tryReference)).tree,
        Block(toList(futureSystemOps.completeProm[T](
          c.Expr[futureSystem.Prom[T]](symLookup.memberRef(name.result)),
          c.Expr[futureSystem.Tryy[T]](
            TypeApply(Select(tryReference, newTermName("asInstanceOf")),
              List(TypeTree(futureSystemOps.tryType[T]))))).tree),
          Return(literalUnit)),
        Block(List(tryGetTree(tryReference)), mkStateTree(nextState, symLookup))
      )

    override def mkOnCompleteHandler[T: WeakTypeTag]: Option[CaseDef] = {
      Some(mkHandlerCase(onCompleteState, List(ifIsFailureTree[T](Ident(symLookup.applyTrParam)))))
    }

    override val toString: String =
      s"AsyncStateWithAwait #$state, next = $nextState"
  }

  /*
   * Builder for a single state of an async expression.
   */
  final class AsyncStateBuilder(state: Int, private val symLookup: SymLookup) {
    /* Statements preceding an await call. */
    private val stats                      = ListBuffer[Tree]()
    /** The state of the target of a LabelDef application (while loop jump) */
    private var nextJumpState: Option[Int] = None
    private var nextJumpSymbol: Symbol = NoSymbol
    def effectiveNextState(nextState: Int) = nextJumpState.orElse(if (nextJumpSymbol == NoSymbol) None else Some(stateIdForLabel(nextJumpSymbol))).getOrElse(nextState)

    def +=(stat: Tree): this.type = {
      stat match {
        case Literal(Constant(())) => // This case occurs in do/while
        case _ =>
          assert(nextJumpState.isEmpty, s"statement appeared after a label jump: $stat")
      }
      def addStat() = stats += stat
      stat match {
        case Apply(fun, args) if isLabel(fun.symbol) =>
          // labelDefStates belongs to the current ExprBuilder
          labelDefStates get fun.symbol match {
            case opt@Some(nextState) =>
              // A backward jump
              nextJumpState = opt // re-use object
              nextJumpSymbol = fun.symbol
            case None =>
              // We haven't the corresponding LabelDef, this is a forward jump
              nextJumpSymbol = fun.symbol
          }
        case _               => addStat()
      }
      this
    }

    def resultWithAwait(awaitable: Awaitable,
                        onCompleteState: Int,
                        nextState: Int): AsyncState = {
      new AsyncStateWithAwait(stats.toList, state, onCompleteState, effectiveNextState(nextState), awaitable, symLookup)
    }

    def resultSimple(nextState: Int): AsyncState = {
      new SimpleAsyncState(stats.toList, state, effectiveNextState(nextState), symLookup)
    }

    def resultWithIf(condTree: Tree, thenState: Int, elseState: Int): AsyncState = {
      def mkBranch(state: Int) = mkStateTree(state, symLookup)
      this += If(condTree, mkBranch(thenState), mkBranch(elseState))
      new AsyncStateWithoutAwait(stats.toList, state, List(thenState, elseState))
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
          CaseDef(pat, guard, Block(bindAssigns, mkStateTree(caseStates(num), symLookup)))
      }
      // 2. insert changed match tree at the end of the current state
      this += Match(scrutTree, newCases)
      new AsyncStateWithoutAwait(stats.toList, state, caseStates)
    }

    def resultWithLabel(startLabelState: Int, symLookup: SymLookup): AsyncState = {
      this += mkStateTree(startLabelState, symLookup)
      new AsyncStateWithoutAwait(stats.toList, state, List(startLabelState))
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

    def checkForUnsupportedAwait(tree: Tree) = if (containsAwait(tree))
      c.abort(tree.pos, "await must not be used in this position")

    def nestedBlockBuilder(nestedTree: Tree, startState: Int, endState: Int) = {
      val (nestedStats, nestedExpr) = statsAndExpr(nestedTree)
      new AsyncBlockBuilder(nestedStats, nestedExpr, startState, endState, symLookup)
    }

    import stateAssigner.nextState
    def directlyAdjacentLabelDefs(t: Tree): List[Tree] = {
      def isPatternCaseLabelDef(t: Tree) = t match {
        case LabelDef(name, _, _) => name.toString.startsWith("case")
        case _ => false
      }
      val span = (stats :+ expr).filterNot(isLiteralUnit).span(_ ne t)
      span match {
        case (before, _ :: after) =>
          before.reverse.takeWhile(isPatternCaseLabelDef) ::: after.takeWhile(isPatternCaseLabelDef)
        case _ =>
          stats :+ expr
      }
    }

    // populate asyncStates
    def add(stat: Tree): Unit = stat match {
      // the val name = await(..) pattern
      case vd @ ValDef(mods, name, tpt, Apply(fun, arg :: Nil)) if isAwait(fun) =>
        val onCompleteState = nextState()
        val afterAwaitState = nextState()
        val awaitable = Awaitable(arg, stat.symbol, tpt.tpe, vd)
        asyncStates += stateBuilder.resultWithAwait(awaitable, onCompleteState, afterAwaitState) // complete with await
        currState = afterAwaitState
        stateBuilder = new AsyncStateBuilder(currState, symLookup)

      case If(cond, thenp, elsep) if containsAwait(stat) || containsForiegnLabelJump(stat) =>
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

      case Match(scrutinee, cases) if containsAwait(stat) =>
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
      case ld @ LabelDef(name, params, rhs)
        if containsAwait(rhs) || directlyAdjacentLabelDefs(ld).exists(containsAwait) =>

        val startLabelState = stateIdForLabel(ld.symbol)
        val afterLabelState = nextState()
        asyncStates += stateBuilder.resultWithLabel(startLabelState, symLookup)
        labelDefStates(ld.symbol) = startLabelState
        val builder = nestedBlockBuilder(rhs, startLabelState, afterLabelState)
        asyncStates ++= builder.asyncStates
        currState = afterLabelState
        stateBuilder = new AsyncStateBuilder(currState, symLookup)
      case b @ Block(stats, expr) =>
        (stats :+ expr) foreach (add)
      case _ =>
        checkForUnsupportedAwait(stat)
        stateBuilder += stat
    }
    for (stat <- (stats :+ expr)) add(stat)
    val lastState = stateBuilder.resultSimple(endState)
    asyncStates += lastState
  }

  trait AsyncBlock {
    def asyncStates: List[AsyncState]

    def onCompleteHandler[T: WeakTypeTag]: Tree
  }

  case class SymLookup(stateMachineClass: Symbol, applyTrParam: Symbol) {
    def stateMachineMember(name: TermName): Symbol =
      stateMachineClass.info.member(name)
    def memberRef(name: TermName): Tree =
      gen.mkAttributedRef(stateMachineMember(name))
  }

  /**
   * Uses `AsyncBlockBuilder` to create an instance of `AsyncBlock`.
   *
   * @param  block      a `Block` tree in ANF
   * @param  symLookup  helper for looking up members of the state machine class
   * @return            an `AsyncBlock`
   */
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
          val lastStateBody = c.Expr[T](lastState.body)
          val rhs = futureSystemOps.completeWithSuccess(
            c.Expr[futureSystem.Prom[T]](symLookup.memberRef(name.result)), lastStateBody)
          mkHandlerCase(lastState.state, Block(rhs.tree, Return(literalUnit)))
        }
        asyncStates.toList match {
          case s :: Nil =>
            List(caseForLastState)
          case _        =>
            val initCases = for (state <- asyncStates.toList.init) yield state.mkHandlerCaseForState[T]
            initCases :+ caseForLastState
        }
      }

      val initStates = asyncStates.init

      /**
       * Builds the definition of the `resume` method.
       *
       * The resulting tree has the following shape:
       *
       *     def resume(): Unit = {
       *       try {
       *         state match {
       *           case 0 => {
       *             f11 = exprReturningFuture
       *             f11.onComplete(onCompleteHandler)(context)
       *           }
       *           ...
       *         }
       *       } catch {
       *         case NonFatal(t) => result.failure(t)
       *       }
       *     }
       */
      private def resumeFunTree[T: WeakTypeTag]: Tree = {
        val stateMemberSymbol = symLookup.stateMachineMember(name.state)
        val stateMemberRef = symLookup.memberRef(name.state)
        val body = Match(stateMemberRef, mkCombinedHandlerCases[T] ++ initStates.flatMap(_.mkOnCompleteHandler[T]) ++ List(CaseDef(Ident(nme.WILDCARD), EmptyTree, Throw(Apply(Select(New(Ident(defn.IllegalStateExceptionClass)), termNames.CONSTRUCTOR), List())))))

        Try(
          body,
          List(
            CaseDef(
            Bind(name.t, Typed(Ident(nme.WILDCARD), Ident(defn.ThrowableClass))),
            EmptyTree, {
              val then = {
                val t = c.Expr[Throwable](Ident(name.t))
                val complete = futureSystemOps.completeProm[T](
                    c.Expr[futureSystem.Prom[T]](symLookup.memberRef(name.result)), futureSystemOps.tryyFailure[T](t)).tree
                Block(toList(complete), Return(literalUnit))
              }
              If(Apply(Ident(defn.NonFatalClass), List(Ident(name.t))), then, Throw(Ident(name.t)))
              then
            })), EmptyTree)

        //body
      }

      def forever(t: Tree): Tree = {
        val labelName = name.fresh("while$")
        LabelDef(labelName, Nil, Block(toList(t), Apply(Ident(labelName), Nil)))
      }

      /**
       * Builds a `match` expression used as an onComplete handler.
       *
       * Assumes `tr: Try[Any]` is in scope. The resulting tree has the following shape:
       *
       *     state match {
       *       case 0 =>
       *         x11 = tr.get.asInstanceOf[Double]
       *         state = 1
       *         resume()
       *     }
       */
      def onCompleteHandler[T: WeakTypeTag]: Tree = {
        val onCompletes = initStates.flatMap(_.mkOnCompleteHandler[T]).toList
        forever {
          adaptToUnit(toList(resumeFunTree))
        }
      }
    }
  }

  private def isSyntheticBindVal(tree: Tree) = tree match {
    case vd@ValDef(_, lname, _, Ident(rname)) => lname.toString.contains(name.bindSuffix)
    case _                                    => false
  }

  case class Awaitable(expr: Tree, resultName: Symbol, resultType: Type, resultValDef: ValDef)

  private def mkStateTree(nextState: Int, symLookup: SymLookup): Tree =
    Assign(symLookup.memberRef(name.state), Literal(Constant(nextState)))

  private def mkHandlerCase(num: Int, rhs: List[Tree]): CaseDef =
    mkHandlerCase(num, adaptToUnit(rhs))

  // We use the convention that the state machine's ID for a state corresponding to
  // a labeldef will a negative number be based on the symbol ID. This allows us
  // to translate a forward jump to the label as a state transition to a known state
  // ID, even though the state machine transform hasn't yet processed the target label
  // def. Negative numbers are used so as as not to clash with regular state IDs, which
  // are allocated in ascending order from 0.
  private def stateIdForLabel(sym: Symbol): Int = -symId(sym)

  private def tpeOf(t: Tree): Type = t match {
    case _ if t.tpe != null => t.tpe
    case Try(body, Nil, _) => tpeOf(body)
    case Block(_, expr) => tpeOf(expr)
    case Literal(Constant(value)) if value == () => definitions.UnitTpe
    case Return(_) => definitions.NothingTpe
    case _ => NoType
  }

  private def adaptToUnit(rhs: List[Tree]): c.universe.Block = {
    rhs match {
      case (rhs: Block) :: Nil if tpeOf(rhs) <:< definitions.UnitTpe =>
        rhs
      case init :+ last if tpeOf(last) <:< definitions.UnitTpe =>
        Block(init, last)
      case init :+ (last @ Literal(Constant(()))) =>
        Block(init, last)
      case init :+ (last @ Block(_, Return(_) | Literal(Constant(())))) =>
        Block(init, last)
      case init :+ (Block(stats, expr)) =>
        Block(init, Block(stats :+ expr, literalUnit))
      case _ =>
        Block(rhs, literalUnit)
    }
  }

  private def mkHandlerCase(num: Int, rhs: Tree): CaseDef =
    CaseDef(Literal(Constant(num)), EmptyTree, rhs)

  def literalUnit = Literal(Constant(())) // a def to avoid sharing trees

  def toList(tree: Tree): List[Tree] = tree match {
    case Block(stats, Literal(Constant(value))) if value == () => stats
    case _ => tree :: Nil
  }

  def literalNull = Literal(Constant(null))
}
