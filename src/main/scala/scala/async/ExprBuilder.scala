/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import scala.reflect.macros.Context
import scala.collection.mutable.ListBuffer
import collection.mutable

/*
 * @author Philipp Haller
 */
final case class ExprBuilder[C <: Context, FS <: FutureSystem](override val c: C, val futureSystem: FS)
  extends TransformUtils(c) {
  builder =>

  import c.universe._
  import defn._

  private[async] object name {
    def suffix(string: String) = string + "$async"

    def suffixedName(prefix: String) = newTermName(suffix(prefix))

    val state       = suffixedName("state")
    val result      = suffixedName("result")
    val resume      = suffixedName("resume")
    val execContext = suffixedName("execContext")

    // TODO do we need to freshen any of these?
    val x1                = newTermName("x$1")
    val tr                = newTermName("tr")
    val onCompleteHandler = suffixedName("onCompleteHandler")

    def fresh(name: TermName) = if (name.toString.contains("$")) name else newTermName(c.fresh("" + name + "$"))
  }

  private[async] lazy val futureSystemOps = futureSystem.mkOps(c)

  private def resetDuplicate(tree: Tree) = c.resetAllAttrs(tree.duplicate)

  private def mkResumeApply = Apply(Ident(name.resume), Nil)

  private def mkStateTree(nextState: Int): c.Tree =
    mkStateTree(c.literal(nextState).tree)

  private def mkStateTree(nextState: Tree): c.Tree =
    Assign(Ident(name.state), nextState)

  private def mkVarDefTree(resultType: Type, resultName: TermName): c.Tree = {
    ValDef(Modifiers(Flag.MUTABLE), resultName, TypeTree(resultType), defaultValue(resultType))
  }

  private def mkHandlerCase(num: Int, rhs: List[c.Tree]): CaseDef =
    mkHandlerCase(num, Block(rhs: _*))

  private def mkHandlerCase(num: Int, rhs: c.Tree): CaseDef =
    CaseDef(c.literal(num).tree, EmptyTree, rhs)

  class AsyncState(stats: List[c.Tree], val state: Int, val nextState: Int) {
    val body: c.Tree = stats match {
      case stat :: Nil => stat
      case _           => Block(stats: _*)
    }

    val varDefs: List[(TermName, Type)] = Nil

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

    def varDefForResult: Option[c.Tree] =
      None

    def allVarDefs: List[c.Tree] =
      varDefForResult.toList ++ varDefs.map(p => mkVarDefTree(p._2, p._1))

    override val toString: String =
      s"AsyncState #$state, next = $nextState"
  }

  class AsyncStateWithoutAwait(stats: List[c.Tree], state: Int)
    extends AsyncState(stats, state, 0) {
    // nextState unused, since encoded in then and else branches

    override def mkHandlerCaseForState(): CaseDef =
      mkHandlerCase(state, stats)

    override val toString: String =
      s"AsyncStateWithIf #$state, next = $nextState"
  }

  abstract class AsyncStateWithAwait(stats: List[c.Tree], state: Int, nextState: Int)
    extends AsyncState(stats, state, nextState) {
    val awaitable : c.Tree
    val resultName: TermName
    val resultType: Type

    protected def tryType = appliedType(TryClass.toType, List(resultType))

    override val toString: String =
      s"AsyncStateWithAwait #$state, next = $nextState"

    private def mkOnCompleteTree: c.Tree = {
      futureSystemOps.onComplete(c.Expr(awaitable), c.Expr(Ident(name.onCompleteHandler)), c.Expr(Ident(name.execContext))).tree
    }

    override def mkHandlerCaseForState(): CaseDef = {
      assert(awaitable != null)
      mkHandlerCase(state, stats :+ mkOnCompleteTree)
    }

    override def varDefForResult: Option[c.Tree] =
      Some(mkVarDefTree(resultType, resultName))
  }

  /*
   * Builder for a single state of an async method.
   */
  class AsyncStateBuilder(state: Int, private val nameMap: Map[Symbol, c.Name]) {
    self =>

    /* Statements preceding an await call. */
    private val stats = ListBuffer[c.Tree]()

    /* Argument of an await call. */
    var awaitable: c.Tree = null

    /* Result name of an await call. */
    var resultName: TermName = null

    /* Result type of an await call. */
    var resultType: Type = null

    var nextState: Int = -1

    private val varDefs = ListBuffer[(TermName, Type)]()

    private val renamer = new Transformer {
      override def transform(tree: Tree) = tree match {
        case Ident(_) if nameMap.keySet contains tree.symbol =>
          Ident(nameMap(tree.symbol))
        case _                                               =>
          super.transform(tree)
      }
    }

    def +=(stat: c.Tree): this.type = {
      stats += resetDuplicate(renamer.transform(stat))
      this
    }

    //TODO do not ignore `mods`
    def addVarDef(mods: Any, name: TermName, tpt: c.Tree, rhs: c.Tree): this.type = {
      varDefs += (name -> tpt.tpe)
      this += Assign(Ident(name), rhs)
      this
    }

    def result(): AsyncState =
      if (awaitable == null)
        new AsyncState(stats.toList, state, nextState) {
          override val varDefs = self.varDefs.toList
        }
      else
        new AsyncStateWithAwait(stats.toList, state, nextState) {
          val awaitable  = self.awaitable
          val resultName = self.resultName
          val resultType = self.resultType
          override val varDefs = self.varDefs.toList
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
      val renamed = renamer.transform(awaitArg)
      awaitable = resetDuplicate(renamed)
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
      val cond = resetDuplicate(condTree)
      this += If(cond,
        Block(mkStateTree(thenState), mkResumeApply),
        Block(mkStateTree(elseState), mkResumeApply))
      new AsyncStateWithoutAwait(stats.toList, state) {
        override val varDefs = self.varDefs.toList
      }
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
      new AsyncStateWithoutAwait(stats.toList, state) {
        override val varDefs = self.varDefs.toList
      }
    }

    override def toString: String = {
      val statsBeforeAwait = stats.mkString("\n")
      s"ASYNC STATE:\n$statsBeforeAwait \nawaitable: $awaitable \nresult name: $resultName"
    }
  }

  val stateAssigner = new StateAssigner

  /**
   * An `AsyncBlockBuilder` builds a `ListBuffer[AsyncState]` based on the expressions of a `Block(stats, expr)` (see `Async.asyncImpl`).
   *
   * @param stats       a list of expressions
   * @param expr        the last expression of the block
   * @param startState  the start state
   * @param endState    the state to continue with
   * @param toRename    a `Map` for renaming the given key symbols to the mangled value names
   */
  class AsyncBlockBuilder(stats: List[c.Tree], expr: c.Tree, startState: Int, endState: Int,
                          private val toRename: Map[Symbol, c.Name]) {
    val asyncStates = ListBuffer[builder.AsyncState]()

    private var stateBuilder = new builder.AsyncStateBuilder(startState, toRename)
    // current state builder
    private var currState    = startState

    /* TODO Fall back to CPS plug-in if tree contains an `await` call. */
    def checkForUnsupportedAwait(tree: c.Tree) = if (tree exists {
      case Apply(fun, _) if isAwait(fun) => true
      case _                             => false
    }) c.abort(tree.pos, "await must not be used in this position") //throw new FallbackToCpsException

    def builderForBranch(tree: c.Tree, state: Int, nextState: Int): AsyncBlockBuilder = {
      val (branchStats, branchExpr) = tree match {
        case Block(s, e) => (s, e)
        case _           => (List(tree), c.literalUnit.tree)
      }
      new AsyncBlockBuilder(branchStats, branchExpr, state, nextState, toRename)
    }

    // populate asyncStates
    for (stat <- stats) stat match {
      // the val name = await(..) pattern
      case ValDef(mods, name, tpt, Apply(fun, args)) if isAwait(fun) =>
        val afterAwaitState = stateAssigner.nextState()
        asyncStates += stateBuilder.complete(args.head, toRename(stat.symbol).toTermName, tpt, afterAwaitState).result // complete with await
        currState = afterAwaitState
        stateBuilder = new builder.AsyncStateBuilder(currState, toRename)

      case ValDef(mods, name, tpt, rhs) if toRename contains stat.symbol =>
        checkForUnsupportedAwait(rhs)

        // when adding assignment need to take `toRename` into account
        stateBuilder.addVarDef(mods, toRename(stat.symbol).toTermName, tpt, rhs)

      case If(cond, thenp, elsep) if stat exists isAwait =>
        checkForUnsupportedAwait(cond)

        val thenStartState = stateAssigner.nextState()
        val elseStartState = stateAssigner.nextState()
        val afterIfState = stateAssigner.nextState()

        asyncStates +=
          // the two Int arguments are the start state of the then branch and the else branch, respectively
          stateBuilder.resultWithIf(cond, thenStartState, elseStartState)

        List((thenp, thenStartState), (elsep, elseStartState)) foreach {
          case (tree, state) =>
            val builder = builderForBranch(tree, state, afterIfState)
            asyncStates ++= builder.asyncStates
        }

        currState = afterIfState
        stateBuilder = new builder.AsyncStateBuilder(currState, toRename)

      case Match(scrutinee, cases) if stat exists isAwait =>
        checkForUnsupportedAwait(scrutinee)

        val caseStates = cases.map(_ => stateAssigner.nextState())
        val afterMatchState = stateAssigner.nextState()

        asyncStates +=
          stateBuilder.resultWithMatch(scrutinee, cases, caseStates)

        for ((cas, num) <- cases.zipWithIndex) {
          val (casStats, casExpr) = cas match {
            case CaseDef(_, _, Block(s, e)) => (s, e)
            case CaseDef(_, _, rhs)         => (List(rhs), c.literalUnit.tree)
          }
          val builder = new AsyncBlockBuilder(casStats, casExpr, caseStates(num), afterMatchState, toRename)
          asyncStates ++= builder.asyncStates
        }

        currState = afterMatchState
        stateBuilder = new builder.AsyncStateBuilder(currState, toRename)

      case _ =>
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
