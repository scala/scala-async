/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import scala.reflect.macros.Context
import scala.collection.mutable.ListBuffer
import collection.mutable

/*
 * @author Philipp Haller
 */
private[async] final case class ExprBuilder[C <: Context, FS <: FutureSystem](val c: C, val futureSystem: FS) {
  builder =>

  val utils = TransformUtils[c.type](c)

  import c.universe._
  import utils._
  import defn._

  lazy val futureSystemOps = futureSystem.mkOps(c)

  private def resetDuplicate(tree: Tree) = c.resetAllAttrs(tree.duplicate)

  private def mkResumeApply = Apply(Ident(name.resume), Nil)

  private def mkStateTree(nextState: Int): c.Tree =
    Assign(Ident(name.state), c.literal(nextState).tree)

  private def mkHandlerCase(num: Int, rhs: List[c.Tree]): CaseDef =
    mkHandlerCase(num, Block(rhs: _*))

  private def mkHandlerCase(num: Int, rhs: c.Tree): CaseDef =
    CaseDef(c.literal(num).tree, EmptyTree, rhs)

  val stateAssigner  = new StateAssigner
  val labelDefStates = collection.mutable.Map[Symbol, Int]()

  class AsyncState(stats: List[c.Tree], val state: Int, val nextState: Int) {
    val body: c.Tree = stats match {
      case stat :: Nil => stat
      case _           => Block(stats: _*)
    }

    def mkHandlerCaseForState(): CaseDef =
      mkHandlerCase(state, stats :+ mkStateTree(nextState) :+ mkResumeApply)

    def mkOnCompleteHandler(): Option[CaseDef] = {
      this match {
        case aw: AsyncStateWithAwait =>
          val tryGetTree =
            Assign(
              Ident(aw.resultName),
              TypeApply(Select(Select(Ident(name.tr), Try_get), newTermName("asInstanceOf")), List(TypeTree(aw.resultType)))
            )
          val updateState = mkStateTree(nextState)
          Some(mkHandlerCase(state, List(tryGetTree, updateState, mkResumeApply)))
        case _                       =>
          None
      }
    }

    override val toString: String =
      s"AsyncState #$state, next = $nextState"
  }

  final class AsyncStateWithoutAwait(stats: List[c.Tree], state: Int)
    extends AsyncState(stats, state, 0) {
    // nextState unused, since encoded in then and else branches

    override def mkHandlerCaseForState(): CaseDef =
      mkHandlerCase(state, stats)

    override val toString: String =
      s"AsyncStateWithIf #$state, next = $nextState"
  }

  final class AsyncStateWithAwait(stats: List[c.Tree], state: Int, nextState: Int,
                                  awaitable: c.Tree, val resultName: TermName, val resultType: Type)
    extends AsyncState(stats, state, nextState) {

    protected def tryType = appliedType(TryClass.toType, List(resultType))

    private def mkOnCompleteTree: c.Tree = {
      futureSystemOps.onComplete(c.Expr(awaitable), c.Expr(Ident(name.onCompleteHandler)), c.Expr(Ident(name.execContext))).tree
    }

    override def mkHandlerCaseForState(): CaseDef = {
      assert(awaitable != null)
      mkHandlerCase(state, stats :+ mkOnCompleteTree)
    }

    override val toString: String =
      s"AsyncStateWithAwait #$state, next = $nextState"
  }

  /*
   * Builder for a single state of an async method.
   */
  final class AsyncStateBuilder(state: Int, private val nameMap: Map[Symbol, c.Name]) {
    self =>

    /* Statements preceding an await call. */
    private val stats = ListBuffer[c.Tree]()

    /* Argument of an await call. */
    var awaitable: c.Tree = null

    /* Result name of an await call. */
    var resultName: TermName = null

    /* Result type of an await call. */
    var resultType: Type = null

    var nextState    : Int         = -1
    var nextJumpState: Option[Int] = None

    def rename(tree: Tree) = substituteNames(tree, nameMap)

    def +=(stat: c.Tree): this.type = {
      assert(nextJumpState.isEmpty, s"statement appeared after a label jump: $stat")
      def addStat() = stats += resetDuplicate(rename(stat))
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

    def result(): AsyncState = {
      val effectiveNextState = nextJumpState.getOrElse(nextState)
      if (awaitable == null)
        new AsyncState(stats.toList, state, effectiveNextState)
      else
        new AsyncStateWithAwait(stats.toList, state, effectiveNextState, awaitable, resultName, resultType)
    }

    /* Result needs to be created as a var at the beginning of the transformed method body, so that
     * it is visible in subsequent states of the state machine.
     *
     * @param awaitArg         the argument of await
     * @param awaitResultName  the name of the variable that the result of await is assigned to
     * @param awaitResultType  the type of the result of await
     */
    def complete(awaitArg: c.Tree, awaitResultName: TermName, awaitResultType: Tree,
                 nextState: Int): this.type = {
      awaitable = resetDuplicate(rename(awaitArg))
      resultName = awaitResultName
      resultType = awaitResultType.tpe
      this.nextState = nextState
      this
    }

    def complete(nextState: Int): this.type = {
      this.nextState = nextState
      this
    }

    def resultWithIf(condTree: c.Tree, thenState: Int, elseState: Int): AsyncState = {
      // 1. build changed if-else tree
      // 2. insert that tree at the end of the current state
      val cond = resetDuplicate(rename(condTree))
      def mkBranch(state: Int) = Block(mkStateTree(state), mkResumeApply)
      this += If(cond, mkBranch(thenState), mkBranch(elseState))
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
    def resultWithMatch(scrutTree: c.Tree, cases: List[CaseDef], caseStates: List[Int]): AsyncState = {
      // 1. build list of changed cases
      val newCases = for ((cas, num) <- cases.zipWithIndex) yield cas match {
        case CaseDef(pat, guard, rhs) => CaseDef(pat, guard, Block(mkStateTree(caseStates(num)), mkResumeApply))
      }
      // 2. insert changed match tree at the end of the current state
      this += Match(resetDuplicate(scrutTree), newCases)
      new AsyncStateWithoutAwait(stats.toList, state)
    }

    def resultWithLabel(startLabelState: Int): AsyncState = {
      this += Block(mkStateTree(startLabelState), mkResumeApply)
      new AsyncStateWithoutAwait(stats.toList, state)
    }

    override def toString: String = {
      val statsBeforeAwait = stats.mkString("\n")
      s"ASYNC STATE:\n$statsBeforeAwait \nawaitable: $awaitable \nresult name: $resultName"
    }
  }

  /**
   * An `AsyncBlockBuilder` builds a `ListBuffer[AsyncState]` based on the expressions of a `Block(stats, expr)` (see `Async.asyncImpl`).
   *
   * @param stats       a list of expressions
   * @param expr        the last expression of the block
   * @param startState  the start state
   * @param endState    the state to continue with
   * @param toRename    a `Map` for renaming the given key symbols to the mangled value names
   */
  final class AsyncBlockBuilder(stats: List[c.Tree], expr: c.Tree, startState: Int, endState: Int,
                          private val toRename: Map[Symbol, c.Name]) {
    val asyncStates = ListBuffer[builder.AsyncState]()

    private var stateBuilder = new builder.AsyncStateBuilder(startState, toRename)
    private var currState    = startState

    /* TODO Fall back to CPS plug-in if tree contains an `await` call. */
    def checkForUnsupportedAwait(tree: c.Tree) = if (tree exists {
      case Apply(fun, _) if isAwait(fun) => true
      case _                             => false
    }) c.abort(tree.pos, "await must not be used in this position") //throw new FallbackToCpsException

    def nestedBlockBuilder(nestedTree: Tree, startState: Int, endState: Int) = {
      val (nestedStats, nestedExpr) = statsAndExpr(nestedTree)
      new AsyncBlockBuilder(nestedStats, nestedExpr, startState, endState, toRename)
    }

    import stateAssigner.nextState

    // populate asyncStates
    for (stat <- stats) stat match {
      // the val name = await(..) pattern
      case ValDef(mods, name, tpt, Apply(fun, args)) if isAwait(fun) =>
        val afterAwaitState = nextState()
        asyncStates += stateBuilder.complete(args.head, toRename(stat.symbol).toTermName, tpt, afterAwaitState).result // complete with await
        currState = afterAwaitState
        stateBuilder = new builder.AsyncStateBuilder(currState, toRename)

      case ValDef(mods, name, tpt, rhs) if toRename contains stat.symbol =>
        checkForUnsupportedAwait(rhs)
        stateBuilder += Assign(Ident(toRename(stat.symbol).toTermName), rhs)

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
        stateBuilder = new builder.AsyncStateBuilder(currState, toRename)

      case Match(scrutinee, cases) if stat exists isAwait =>
        checkForUnsupportedAwait(scrutinee)

        val caseStates = cases.map(_ => nextState())
        val afterMatchState = nextState()

        asyncStates +=
          stateBuilder.resultWithMatch(scrutinee, cases, caseStates)

        for ((cas, num) <- cases.zipWithIndex) {
          val builder = nestedBlockBuilder(cas.body, caseStates(num), afterMatchState)
          asyncStates ++= builder.asyncStates
        }

        currState = afterMatchState
        stateBuilder = new AsyncStateBuilder(currState, toRename)

      case ld@LabelDef(name, params, rhs) if rhs exists isAwait =>
        val startLabelState = nextState()
        val afterLabelState = nextState()
        asyncStates += stateBuilder.resultWithLabel(startLabelState)
        labelDefStates(ld.symbol) = startLabelState
        val builder = nestedBlockBuilder(rhs, startLabelState, afterLabelState)
        asyncStates ++= builder.asyncStates

        currState = afterLabelState
        stateBuilder = new AsyncStateBuilder(currState, toRename)
      case _                                                    =>
        checkForUnsupportedAwait(stat)
        stateBuilder += stat
    }
    // complete last state builder (representing the expressions after the last await)
    stateBuilder += expr
    val lastState = stateBuilder.complete(endState).result()
    asyncStates += lastState

    def mkCombinedHandlerCases[T](): List[CaseDef] = {
      val caseForLastState: CaseDef = {
        val lastState = asyncStates.last
        val lastStateBody = c.Expr[T](lastState.body)
        val rhs = futureSystemOps.completeProm(c.Expr[futureSystem.Prom[T]](Ident(name.result)), reify(scala.util.Success(lastStateBody.splice)))
        mkHandlerCase(lastState.state, rhs.tree)
      }
      asyncStates.toList match {
        case s :: Nil =>
          List(caseForLastState)
        case _        =>
          val initCases = for (state <- asyncStates.toList.init) yield state.mkHandlerCaseForState()
          initCases :+ caseForLastState
      }
    }
  }
}
