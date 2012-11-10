/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import scala.reflect.macros.Context
import scala.collection.mutable.{ListBuffer, Builder}
import concurrent.Future

/*
 * @author Philipp Haller
 */
final class ExprBuilder[C <: Context, FS <: FutureSystem](val c: C, val futureSystem: FS) extends AsyncUtils {
  builder =>

  lazy val futureSystemOps = futureSystem.mkOps(c)

  import c.universe._
  import Flag._
  import defn._

  object name {
    // TODO do we need to freshen any of these?
    def suffix(string: String) = string + "$async"
    def expandedTermName(prefix: String) = newTermName(suffix(prefix))
    def expandedTypeName(prefix: String) = newTypeName(suffix(prefix))
    val resume = expandedTermName("resume")
    val state = expandedTermName("state")
    val result = expandedTermName("result")
    val tr = newTermName("tr")
    val any = newTermName("any")
    val x1 = newTermName("x$1")
    val apply = newTermName("apply")
    val isDefinedAt = newTermName("isDefinedAt")

    val asyncHander = expandedTypeName("Handler")
  }

  private val execContext = futureSystemOps.execContext

  private def resetDuplicate(tree: Tree) = c.resetAllAttrs(tree.duplicate)

  private val awaitMethod = awaitSym(c)

  private def mkResumeApply = Apply(Ident(name.resume), List())

  def mkStateTree(nextState: Int): c.Tree =
    mkStateTree(c.literal(nextState).tree)

  def mkStateTree(nextState: Tree): c.Tree =
    Assign(
      Ident(name.state),
      nextState)

  def defaultValue(tpe: Type): Literal = {
    val defaultValue: Any =
      if (tpe <:< definitions.BooleanTpe) false
      else if (definitions.ScalaNumericValueClasses.exists(tpe <:< _.toType)) 0
      else null
    Literal(Constant(defaultValue))
  }

  def mkVarDefTree(resultType: Type, resultName: TermName): c.Tree = {
    ValDef(Modifiers(Flag.MUTABLE), resultName, TypeTree(resultType), defaultValue(resultType))
  }

  def mkHandlerCase(num: Int, rhs: List[c.Tree]): CaseDef =
    mkHandlerCase(num, Block(rhs: _*))

  def mkHandlerCase(num: Int, rhs: c.Tree): CaseDef =
    CaseDef(
      // pattern
      Bind(name.any, Typed(Ident(nme.WILDCARD), Ident(definitions.IntClass))),
      // guard
      mkAny_==(c.Expr(Ident(name.any)))(c.literal(num)).tree,
      rhs
    )

  private def paramValDef(name: TermName, sym: Symbol) = ValDef(Modifiers(PARAM), name, Ident(sym), EmptyTree)
  private def paramValDef(name: TermName, tpe: Type) = ValDef(Modifiers(PARAM), name, TypeTree(tpe), EmptyTree)

  def mkHandlerTreeFor(cases: List[(CaseDef, Int)]): c.Tree = {
    val partFunIdent = Ident(defn.PartialFunctionClass)
//    val partFunTpe = appliedType(defn.PartialFunctionClass.tpe, definitions.IntTpe, definitions.UnitTpe)
    val intIdent = Ident(definitions.IntClass)
    val unitIdent = Ident(definitions.UnitClass)

    val caseCheck =
      defn.mkList_contains(defn.mkList_apply(cases.map(p => c.literal(p._2))))(c.Expr(Ident(name.x1)))
    val handlerName = name.asyncHander

    Block(List(
      // anonymous subclass of PartialFunction[Int, Unit]
      // TODO subclass AbstractPartialFunction
      ClassDef(Modifiers(FINAL), handlerName, List(), Template(List(AppliedTypeTree(partFunIdent, List(intIdent, unitIdent))),
        emptyValDef, List(
          DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
            Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), c.literalUnit.tree)),

          DefDef(Modifiers(), name.isDefinedAt, List(), List(List(paramValDef(name.x1, definitions.IntClass))), TypeTree(),
            caseCheck.tree),

          DefDef(Modifiers(), name.apply, List(), List(List(paramValDef(name.x1, definitions.IntClass))), TypeTree(),
            Match(Ident(name.x1), cases.map(_._1)) // combine all cases into a single match
          )
        ))
      )),
      Apply(Select(New(Ident(handlerName)), nme.CONSTRUCTOR), List())
    )
  }

  class AsyncState(stats: List[c.Tree], val state: Int, val nextState: Int) {
    val body: c.Tree = stats match {
      case stat :: Nil => stat
      case _ => Block(stats: _*)
    }

    val varDefs: List[(TermName, Type)] = List()

    def mkHandlerCaseForState(): CaseDef =
      mkHandlerCase(state, stats :+ mkStateTree(nextState) :+ mkResumeApply)

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
    val awaitable: c.Tree
    val resultName: TermName
    val resultType: Type

    protected def tryType = appliedType(TryClass.toType, List(resultType))

    override val toString: String =
      s"AsyncStateWithAwait #$state, next = $nextState"

    /* Make an `onComplete` invocation which increments the state upon resuming:
     * 
     *     awaitable.onComplete {
     *       case tr =>
     *         resultName = tr.get
     *         state += 1
     *         resume()
     *     }
     */
    def mkOnCompleteIncrStateTree: c.Tree =
      mkOnCompleteTree(mkInt_+(c.Expr[Int](Ident(name.state)))(c.literal(1)).tree)

    /* Make an `onComplete` invocation which sets the state to `nextState` upon resuming:
     * 
     *     awaitable.onComplete {
     *       case tr =>
     *         resultName = tr.get
     *         state = `nextState`
     *         resume()
     *     }
     */
    def mkOnCompleteStateTree(nextState: Int): c.Tree =
      mkOnCompleteTree(c.literal(nextState).tree)

    private def mkOnCompleteTree(nextState: Tree): c.Tree = {
      val tryGetTree =
        Assign(
          Ident(resultName),
          Select(Ident(name.tr), Try_get)
        )

      val updateState = mkStateTree(nextState)

      val handlerTree =
        Function(List(paramValDef(name.tr, tryType)), Block(tryGetTree, updateState, mkResumeApply))

      futureSystemOps.onComplete(c.Expr(awaitable), c.Expr(handlerTree), execContext).tree
    }

    override def mkHandlerCaseForState(): CaseDef = {
      assert(awaitable != null)
      mkHandlerCase(state, stats :+ mkOnCompleteIncrStateTree)
    }

    override def varDefForResult: Option[c.Tree] =
      Some(mkVarDefTree(resultType, resultName))
  }

  /*
   * Builder for a single state of an async method.
   */
  class AsyncStateBuilder(state: Int, private var nameMap: Map[c.Symbol, c.Name]) {
    self =>

    /* Statements preceding an await call. */
    private val stats = ListBuffer[c.Tree]()

    /* Argument of an await call. */
    var awaitable: c.Tree = null

    /* Result name of an await call. */
    var resultName: TermName = null

    /* Result type of an await call. */
    var resultType: Type = null

    var nextState: Int = state + 1

    private val varDefs = ListBuffer[(TermName, Type)]()

    private val renamer = new Transformer {
      override def transform(tree: Tree) = tree match {
        case Ident(_) if nameMap.keySet contains tree.symbol =>
          Ident(nameMap(tree.symbol))
        case _ =>
          super.transform(tree)
      }
    }

    def +=(stat: c.Tree): this.type = {
      stats += resetDuplicate(renamer.transform(stat))
      this
    }

    //TODO do not ignore `mods`
    def addVarDef(mods: Any, name: TermName, tpt: c.Tree, rhs: c.Tree, extNameMap: Map[c.Symbol, c.Name]): this.type = {
      varDefs += (name -> tpt.tpe)
      nameMap ++= extNameMap // update name map
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
          val awaitable = self.awaitable
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
                 extNameMap: Map[c.Symbol, c.Name], nextState: Int = state + 1): this.type = {
      nameMap ++= extNameMap
      awaitable = resetDuplicate(renamer.transform(awaitArg))
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
     * @param stateFirstCase  state of the right-hand side of the first case
     * @param perCaseBudget   maximum number of states per case
     * @return                an `AsyncState` representing the match expression
     */
    def resultWithMatch(scrutTree: c.Tree, cases: List[CaseDef], stateFirstCase: Int, perCasebudget: Int): AsyncState = {
      // 1. build list of changed cases
      val newCases = for ((cas, num) <- cases.zipWithIndex) yield cas match {
        case CaseDef(pat, guard, rhs) => CaseDef(pat, guard, Block(mkStateTree(num * perCasebudget + stateFirstCase), mkResumeApply))
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

  /**
   * An `AsyncBlockBuilder` builds a `ListBuffer[AsyncState]` based on the expressions of a `Block(stats, expr)` (see `Async.asyncImpl`).
   *
   * @param stats       a list of expressions
   * @param expr        the last expression of the block
   * @param startState  the start state
   * @param endState    the state to continue with
   * @param budget      the maximum number of states in this block
   * @param toRename    a `Map` for renaming the given key symbols to the mangled value names
   */
  class AsyncBlockBuilder(stats: List[c.Tree], expr: c.Tree, startState: Int, endState: Int,
                          budget: Int, private var toRename: Map[c.Symbol, c.Name]) {
    val asyncStates = ListBuffer[builder.AsyncState]()

    private var stateBuilder = new builder.AsyncStateBuilder(startState, toRename)
    // current state builder
    private var currState = startState

    private var remainingBudget = budget

    /* TODO Fall back to CPS plug-in if tree contains an `await` call. */
    def checkForUnsupportedAwait(tree: c.Tree) = if (tree exists {
      case Apply(fun, _) if fun.symbol == awaitMethod => true
      case _ => false
    }) c.abort(tree.pos, "await unsupported in this position") //throw new FallbackToCpsException

    def builderForBranch(tree: c.Tree, state: Int, nextState: Int, budget: Int, nameMap: Map[c.Symbol, c.Name]): AsyncBlockBuilder = {
      val (branchStats, branchExpr) = tree match {
        case Block(s, e) => (s, e)
        case _ => (List(tree), c.literalUnit.tree)
      }
      new AsyncBlockBuilder(branchStats, branchExpr, state, nextState, budget, nameMap)
    }

    // populate asyncStates
    for (stat <- stats) stat match {
      // the val name = await(..) pattern
      case ValDef(mods, name, tpt, Apply(fun, args)) if fun.symbol == awaitMethod =>
        val newName = c.fresh(name)
        toRename += (stat.symbol -> newName)

        asyncStates += stateBuilder.complete(args.head, newName, tpt, toRename).result // complete with await
        if (remainingBudget > 0)
          remainingBudget -= 1
        else
          assert(false, "too many invocations of `await` in current method")
        currState += 1
        stateBuilder = new builder.AsyncStateBuilder(currState, toRename)

      case ValDef(mods, name, tpt, rhs) =>
        checkForUnsupportedAwait(rhs)

        val newName = c.fresh(name)
        toRename += (stat.symbol -> newName)
        // when adding assignment need to take `toRename` into account
        stateBuilder.addVarDef(mods, newName, tpt, rhs, toRename)

      case If(cond, thenp, elsep) =>
        checkForUnsupportedAwait(cond)

        val ifBudget: Int = remainingBudget / 2
        remainingBudget -= ifBudget //TODO test if budget > 0
        // state that we continue with after if-else: currState + ifBudget

        val thenBudget: Int = ifBudget / 2
        val elseBudget = ifBudget - thenBudget

        asyncStates +=
          // the two Int arguments are the start state of the then branch and the else branch, respectively
          stateBuilder.resultWithIf(cond, currState + 1, currState + thenBudget)

        List((thenp, currState + 1, thenBudget), (elsep, currState + thenBudget, elseBudget)) foreach {
          case (tree, state, branchBudget) =>
            val builder = builderForBranch(tree, state, currState + ifBudget, branchBudget, toRename)
            asyncStates ++= builder.asyncStates
            toRename ++= builder.toRename
        }

        // create new state builder for state `currState + ifBudget`
        currState = currState + ifBudget
        stateBuilder = new builder.AsyncStateBuilder(currState, toRename)

      case Match(scrutinee, cases) =>
        vprintln("transforming match expr: " + stat)
        checkForUnsupportedAwait(scrutinee)

        val matchBudget: Int = remainingBudget / 2
        remainingBudget -= matchBudget //TODO test if budget > 0
        // state that we continue with after match: currState + matchBudget

        val perCaseBudget: Int = matchBudget / cases.size
        asyncStates +=
          // the two Int arguments are the start state of the first case and the per-case state budget, respectively
          stateBuilder.resultWithMatch(scrutinee, cases, currState + 1, perCaseBudget)

        for ((cas, num) <- cases.zipWithIndex) {
          val (casStats, casExpr) = cas match {
            case CaseDef(_, _, Block(s, e)) => (s, e)
            case CaseDef(_, _, rhs) => (List(rhs), c.literalUnit.tree)
          }
          val builder = new AsyncBlockBuilder(casStats, casExpr, currState + (num * perCaseBudget) + 1, currState + matchBudget, perCaseBudget, toRename)
          asyncStates ++= builder.asyncStates
          toRename ++= builder.toRename
        }

        // create new state builder for state `currState + matchBudget`
        currState = currState + matchBudget
        stateBuilder = new builder.AsyncStateBuilder(currState, toRename)

      case _ =>
        checkForUnsupportedAwait(stat)
        stateBuilder += stat
    }
    // complete last state builder (representing the expressions after the last await)
    stateBuilder += expr
    val lastState = stateBuilder.complete(endState).result
    asyncStates += lastState

    def mkCombinedHandlerCases(): List[(CaseDef, Int)] = {
      assert(asyncStates.size > 1)

      val cases = for (state <- asyncStates.toList) yield state.mkHandlerCaseForState()
      cases zip asyncStates.init.map(_.state)
    }
  }

  /** `termSym( (_: Foo).bar(null: A, null: B)` will return the symbol of `bar`, after overload resolution. */
  def methodSym(apply: c.Expr[Any]): Symbol = {
    val tree2: Tree = c.typeCheck(apply.tree) // TODO why is this needed?
    tree2.collect {
      case s: SymTree if s.symbol.isMethod => s.symbol
    }.headOption.getOrElse(sys.error(s"Unable to find a method symbol in ${apply.tree}"))
  }

  object defn {
    def mkList_apply[A](args: List[Expr[A]]): Expr[List[A]] = {
      c.Expr(Apply(Ident(definitions.List_apply), args.map(_.tree)))
    }

    def mkList_contains[A](self: Expr[List[A]])(elem: Expr[Any]) = reify(self.splice.contains(elem.splice))

    def mkPartialFunction_orElse[A, B](self: Expr[PartialFunction[A, B]])(other: Expr[PartialFunction[A, B]]) = reify {
      self.splice.orElse(other.splice)
    }

    def mkFunction_apply[A, B](self: Expr[Function1[A, B]])(arg: Expr[A]) = reify {
      self.splice.apply(arg.splice)
    }

    def mkInt_+(self: Expr[Int])(other: Expr[Int]) = reify {
      self.splice + other.splice
    }

    def mkAny_==(self: Expr[Any])(other: Expr[Any]) = reify {
      self.splice == other.splice
    }

    def mkTry_get[A](self: Expr[util.Try[A]]) = reify {
      self.splice.get
    }

    val Try_get = methodSym(reify((null: scala.util.Try[Any]).get))

    val PartialFunctionClass = c.mirror.staticClass("scala.PartialFunction")
    val TryClass = c.mirror.staticClass("scala.util.Try")
    val NonFatalClass = c.mirror.staticModule("scala.util.control.NonFatal")
  }

}
