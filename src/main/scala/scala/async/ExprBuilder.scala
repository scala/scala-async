/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import scala.reflect.macros.Context
import scala.collection.mutable.ListBuffer
import collection.mutable
import language.existentials

private[async] final case class ExprBuilder[C <: Context, FS <: FutureSystem](c: C, futureSystem: FS, origTree: C#Tree) {
  builder =>

  val utils = TransformUtils[c.type](c)

  import c.universe._
  import utils._
  import defn._

  lazy val futureSystemOps = futureSystem.mkOps(c)

  val stateAssigner  = new StateAssigner
  val labelDefStates = collection.mutable.Map[Symbol, Int]()

  trait AsyncState {
    def state: Int

    def mkHandlerCaseForState: CaseDef

    def mkOnCompleteHandler[T: c.WeakTypeTag]: Option[CaseDef] = None

    def stats: List[Tree]

    final def body: c.Tree = stats match {
      case stat :: Nil => stat
      case init :+ last => Block(init, last)
    }
  }

  /** A sequence of statements the concludes with a unconditional transition to `nextState` */
  final class SimpleAsyncState(val stats: List[Tree], val state: Int, nextState: Int, excState: Option[Int])
    extends AsyncState {

    def mkHandlerCaseForState: CaseDef =
      mkHandlerCase(state, stats :+ mkStateTree(nextState) :+ mkResumeApply, excState)

    override val toString: String =
      s"AsyncState #$state, next = $nextState"
  }

  /** A sequence of statements with a conditional transition to the next state, which will represent
    * a branch of an `if` or a `match`.
    */
  final class AsyncStateWithoutAwait(val stats: List[c.Tree], val state: Int, excState: Option[Int]) extends AsyncState {
    override def mkHandlerCaseForState: CaseDef =
      mkHandlerCase(state, stats, excState)

    override val toString: String =
      s"AsyncStateWithoutAwait #$state"
  }

  /** A sequence of statements that concludes with an `await` call. The `onComplete`
    * handler will unconditionally transition to `nestState`.``
    */
  final class AsyncStateWithAwait(val stats: List[c.Tree], val state: Int, nextState: Int,
                                  awaitable: Awaitable, excState: Option[Int])
    extends AsyncState {

    override def mkHandlerCaseForState: CaseDef = {
      val callOnComplete = futureSystemOps.onComplete(c.Expr(awaitable.expr),
        c.Expr(This(tpnme.EMPTY)), c.Expr(Ident(name.execContext))).tree
      mkHandlerCase(state, stats :+ callOnComplete, excState)
    }

    override def mkOnCompleteHandler[T: c.WeakTypeTag]: Option[CaseDef] = {
      val tryGetTree =
        Assign(
          Ident(awaitable.resultName),
          TypeApply(Select(Select(Ident(name.tr), Try_get), newTermName("asInstanceOf")), List(TypeTree(awaitable.resultType)))
        )

      /* if (tr.isFailure)
       *   result$async.complete(tr.asInstanceOf[Try[T]])
       * else {
       *   <resultName> = tr.get.asInstanceOf[<resultType>]
       *   <nextState>
       *   <mkResumeApply>
       * }
       */
      val ifIsFailureTree =
        If(Select(Ident(name.tr), Try_isFailure),
           futureSystemOps.completeProm[T](
             c.Expr[futureSystem.Prom[T]](Ident(name.result)),
             c.Expr[scala.util.Try[T]](
               TypeApply(Select(Ident(name.tr), newTermName("asInstanceOf")),
                         List(TypeTree(weakTypeOf[scala.util.Try[T]]))))).tree,
           Block(List(tryGetTree, mkStateTree(nextState)), mkResumeApply)
         )

      Some(mkHandlerCase(state, List(ifIsFailureTree), excState))
    }

    override val toString: String =
      s"AsyncStateWithAwait #$state, next = $nextState"
  }

  /*
   * Builder for a single state of an async method.
   *
   * The `excState` parameter is implicit, so that it is passed implicitly
   * when `AsyncBlockBuilder` creates new `AsyncStateBuilder`s.
   */
  final class AsyncStateBuilder(state: Int, private val nameMap: Map[Symbol, c.Name])(implicit excState: Option[Int]) {
    /* Statements preceding an await call. */
    private val stats                      = ListBuffer[c.Tree]()
    /** The state of the target of a LabelDef application (while loop jump) */
    private var nextJumpState: Option[Int] = None

    private def renameReset(tree: Tree) = resetInternalAttrs(substituteNames(tree, nameMap))

    def +=(stat: c.Tree): this.type = {
      assert(nextJumpState.isEmpty, s"statement appeared after a label jump: $stat")
      def addStat() = stats += renameReset(stat)
      stat match {
        case _: DefDef       => // these have been lifted.
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
      val sanitizedAwaitable = awaitable.copy(expr = renameReset(awaitable.expr))
      val effectiveNextState = nextJumpState.getOrElse(nextState)
      new AsyncStateWithAwait(stats.toList, state, effectiveNextState, sanitizedAwaitable, excState)
    }

    def resultWithoutAwait(): AsyncState = {
      this += mkResumeApply
      new AsyncStateWithoutAwait(stats.toList, state, excState)
    }

    def resultSimple(nextState: Int): AsyncState = {
      val effectiveNextState = nextJumpState.getOrElse(nextState)
      new SimpleAsyncState(stats.toList, state, effectiveNextState, excState)
    }

    def resultWithIf(condTree: c.Tree, thenState: Int, elseState: Int): AsyncState = {
      // 1. build changed if-else tree
      // 2. insert that tree at the end of the current state
      val cond = renameReset(condTree)
      def mkBranch(state: Int) = Block(mkStateTree(state) :: Nil, mkResumeApply)
      this += If(cond, mkBranch(thenState), mkBranch(elseState))
      new AsyncStateWithoutAwait(stats.toList, state, excState)
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
        case CaseDef(pat, guard, rhs) =>
          val bindAssigns = rhs.children.takeWhile(isSyntheticBindVal).map {
            case ValDef(_, name, _, rhs) => Assign(Ident(name), rhs)
            case t                       => sys.error(s"Unexpected tree. Expected ValDef, found: $t")
          }
          CaseDef(pat, guard, Block(bindAssigns :+ mkStateTree(caseStates(num)), mkResumeApply))
      }
      // 2. insert changed match tree at the end of the current state
      this += Match(renameReset(scrutTree), newCases)
      new AsyncStateWithoutAwait(stats.toList, state, excState)
    }

    def resultWithLabel(startLabelState: Int): AsyncState = {
      this += Block(mkStateTree(startLabelState) :: Nil, mkResumeApply)
      new AsyncStateWithoutAwait(stats.toList, state, excState)
    }

    override def toString: String = {
      val statsBeforeAwait = stats.mkString("\n")
      s"ASYNC STATE:\n$statsBeforeAwait"
    }
  }

  /**
   * An `AsyncBlockBuilder` builds a `ListBuffer[AsyncState]` based on the expressions of a `Block(stats, expr)` (see `Async.asyncImpl`).
   *
   * @param stats          a list of expressions
   * @param expr           the last expression of the block
   * @param startState     the start state
   * @param endState       the state to continue with
   * @param toRename       a `Map` for renaming the given key symbols to the mangled value names
   * @param excState       the state to continue with in case of an exception
   * @param parentExcState the state to continue with in case of an exception not handled by the current exception handler
   */
  final private class AsyncBlockBuilder(stats: List[c.Tree],   expr: c.Tree, startState: Int, endState: Int, private val toRename: Map[Symbol, c.Name],
                                        parentExcState: Option[Int] = None)(implicit excState: Option[Int]) {
    val asyncStates = ListBuffer[AsyncState]()

    var stateBuilder = new AsyncStateBuilder(startState, toRename)
    var currState    = startState

    /* TODO Fall back to CPS plug-in if tree contains an `await` call. */
    def checkForUnsupportedAwait(tree: c.Tree) = if (tree exists {
      case Apply(fun, _) if isAwait(fun) => true
      case _                             => false
    }) c.abort(tree.pos, "await must not be used in this position") //throw new FallbackToCpsException

    def nestedBlockBuilder(nestedTree: Tree, startState: Int, endState: Int,
                           excState: Option[Int] = None, parentExcState: Option[Int] = None) = {
      val (nestedStats, nestedExpr) = statsAndExpr(nestedTree)
      new AsyncBlockBuilder(nestedStats, nestedExpr, startState, endState, toRename, parentExcState)(excState)
    }

    import stateAssigner.nextState

    // populate asyncStates
    for (stat <- stats) stat match {
      // the val name = await(..) pattern
      case ValDef(mods, name, tpt, Apply(fun, arg :: Nil)) if isAwait(fun) =>
        val afterAwaitState = nextState()
        val awaitable = Awaitable(arg, toRename(stat.symbol).toTermName, tpt.tpe)
        asyncStates += stateBuilder.resultWithAwait(awaitable, afterAwaitState) // complete with await
        currState = afterAwaitState
        stateBuilder = new AsyncStateBuilder(currState, toRename)

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
        stateBuilder = new AsyncStateBuilder(currState, toRename)

      case Try(block, catches, finalizer) if stat exists isAwait =>
        val tryStartState   = nextState()
        val afterTryState   = nextState()
        val ehState         = nextState()
        val finalizerState  = if (!finalizer.isEmpty) Some(nextState()) else None

        // complete current state so that it continues with tryStartState
        asyncStates += stateBuilder.resultWithLabel(tryStartState)

        if (!finalizer.isEmpty) {
          val builder = nestedBlockBuilder(finalizer, finalizerState.get, afterTryState)
          asyncStates ++= builder.asyncStates
        }

        // create handler state
        def handlersDot(m: String) = Select(Ident(name.handlers), m)
        val exceptionExpr          = c.Expr[Throwable](Ident(name.exception))
        // handler state does not have active exception handler --> None
        val handlerStateBuilder    = new AsyncStateBuilder(ehState, toRename)(None)

        val parentExpr: c.Expr[Unit] =
          if (parentExcState.isEmpty) reify { throw exceptionExpr.splice }
          else c.Expr[Unit](mkStateTree(parentExcState.get))

        val handlerExpr  = reify {
          val h = c.Expr[PartialFunction[Throwable, Unit]](handlersDot("head")).splice
          c.Expr[Unit](Assign(Ident(name.handlers), handlersDot("tail"))).splice

          if (h isDefinedAt exceptionExpr.splice) {
            h(exceptionExpr.splice)
            c.Expr[Unit](mkStateTree(if (!finalizer.isEmpty) finalizerState.get else afterTryState)).splice
          } else {
            parentExpr.splice
          }
        }

        handlerStateBuilder += handlerExpr.tree
        asyncStates += handlerStateBuilder.resultWithoutAwait()

        val ehName         = newTermName("handlerPF$" + ehState)
        val partFunAssign  = ValDef(Modifiers(), ehName, TypeTree(typeOf[PartialFunction[Throwable, Unit]]), Match(EmptyTree, catches))
        val newHandler     = c.Expr[PartialFunction[Throwable, Unit]](Ident(ehName))
        val handlersIdent  = c.Expr[List[PartialFunction[Throwable, Unit]]](Ident(name.handlers))
        val pushedHandlers = reify { handlersIdent.splice.+:(newHandler.splice) }
        val pushAssign     = Assign(Ident(name.handlers), pushedHandlers.tree)

        val (tryStats, tryExpr) = statsAndExpr(block)
        val builder = nestedBlockBuilder(Block(partFunAssign :: pushAssign :: tryStats, tryExpr),
                                         tryStartState, if (!finalizer.isEmpty) finalizerState.get else afterTryState, Some(ehState), excState)
        asyncStates ++= builder.asyncStates

        currState = afterTryState
        stateBuilder = new AsyncStateBuilder(currState, toRename)

      case Match(scrutinee, cases) if stat exists isAwait =>
        checkForUnsupportedAwait(scrutinee)

        val caseStates = cases.map(_ => nextState())
        val afterMatchState = nextState()

        asyncStates +=
          stateBuilder.resultWithMatch(scrutinee, cases, caseStates)

        for ((cas, num) <- cases.zipWithIndex) {
          val (stats, expr) = statsAndExpr(cas.body)
          val stats1 = stats.dropWhile(isSyntheticBindVal)
          val builder = nestedBlockBuilder(Block(stats1, expr), caseStates(num), afterMatchState)
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
    val lastState = stateBuilder.resultSimple(endState)
    asyncStates += lastState
  }

  trait AsyncBlock {
    def asyncStates: List[AsyncState]

    def onCompleteHandler[T: c.WeakTypeTag]: Tree

    def resumeFunTree[T]: Tree
  }

  def build(block: Block, toRename: Map[Symbol, c.Name]): AsyncBlock = {
    val Block(stats, expr) = block
    val startState = stateAssigner.nextState()
    val endState = Int.MaxValue

    val blockBuilder = new AsyncBlockBuilder(stats, expr, startState, endState, toRename)(None)

    new AsyncBlock {
      def asyncStates = blockBuilder.asyncStates.toList

      def mkCombinedHandlerCases[T]: List[CaseDef] = {
        val caseForLastState: CaseDef = {
          val lastState = asyncStates.last
          val lastStateBody = c.Expr[T](lastState.body)
          val rhs = futureSystemOps.completeProm(
            c.Expr[futureSystem.Prom[T]](Ident(name.result)), reify(scala.util.Success(lastStateBody.splice)))
          mkHandlerCase(lastState.state, rhs.tree, None)
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
       * // assumes tr: Try[Any] is in scope.
       * //
       * state match {
       * case 0 => {
       * x11 = tr.get.asInstanceOf[Double];
       * state = 1;
       * resume()
       * }
       */
      def onCompleteHandler[T: c.WeakTypeTag]: Tree = Match(Ident(name.state), initStates.flatMap(_.mkOnCompleteHandler[T]).toList)

      /**
       * def resume(): Unit = {
       * try {
       * state match {
       * case 0 => {
       * f11 = exprReturningFuture
       * f11.onComplete(onCompleteHandler)(context)
       * }
       * ...
       * }
       * } catch {
       * case NonFatal(t) => result.failure(t)
       * }
       * }
       */
      def resumeFunTree[T]: Tree =
        DefDef(Modifiers(), name.resume, Nil, List(Nil), Ident(definitions.UnitClass),
          Try(
            Match(Ident(name.state), mkCombinedHandlerCases[T]),
            List(
              CaseDef(
                Apply(Ident(defn.NonFatalClass), List(Bind(name.tr, Ident(nme.WILDCARD)))),
                EmptyTree,
                Block(List({
                  val t = c.Expr[Throwable](Ident(name.tr))
                  futureSystemOps.completeProm[T](c.Expr[futureSystem.Prom[T]](Ident(name.result)), reify(scala.util.Failure(t.splice))).tree
                }), c.literalUnit.tree))), EmptyTree))
    }
  }

  private def isSyntheticBindVal(tree: Tree) = tree match {
    case vd@ValDef(_, lname, _, Ident(rname)) => lname.toString.contains(name.bindSuffix)
    case _                                    => false
  }

  private final case class Awaitable(expr: Tree, resultName: TermName, resultType: Type)

  private val internalSyms = origTree.collect {
    case dt: DefTree => dt.symbol
  }

  private def resetInternalAttrs(tree: Tree) = utils.resetInternalAttrs(tree, internalSyms)

  private def mkResumeApply = Apply(Ident(name.resume), Nil)

  private def mkStateTree(nextState: Int): c.Tree =
    Assign(Ident(name.state), c.literal(nextState).tree)

  private def mkHandlerCase(num: Int, rhs: List[c.Tree], excState: Option[Int]): CaseDef =
    mkHandlerCase(num, Block(rhs, c.literalUnit.tree), excState)

  /* Generates `case` clause with wrapping try-catch:
   *
   * case `num` =>
   *   try {
   *     rhs
   *   } catch {
   *     case NonFatal(t) =>
   *       exception$async = t
   *       state$async     = excState.get
   *       resume$async()
   *   }
   */
  private def mkHandlerCase(num: Int, rhs: c.Tree, excState: Option[Int]): CaseDef = {
    val rhsWithTry =
      if (excState.isEmpty) rhs
      else Try(rhs,
               List(
                 CaseDef(
                   Apply(Ident(defn.NonFatalClass), List(Bind(newTermName("t"), Ident(nme.WILDCARD)))),
                   EmptyTree,
                   Block(List(
                     Assign(Ident(name.exception), Ident(newTermName("t"))),
                     mkStateTree(excState.get),
                     mkResumeApply
                   ), c.literalUnit.tree))), EmptyTree
             )
    CaseDef(c.literal(num).tree, EmptyTree, rhsWithTry)
  }
}
