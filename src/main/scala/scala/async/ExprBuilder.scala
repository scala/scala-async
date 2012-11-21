/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import scala.reflect.macros.Context
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import AsyncUtils.vprintln

/*
 * @author Philipp Haller
 */
final class ExprBuilder[C <: Context, FS <: FutureSystem](override val c: C, val futureSystem: FS)
  extends TransformUtils(c) {
  builder =>

  import c.universe._
  import Flag._
  import defn._

  private[async] object name {
    def suffix(string: String) = string + "$async"

    def suffixedName(prefix: String) = newTermName(suffix(prefix))

    val state = suffixedName("state")
    val result = suffixedName("result")
    val resume = suffixedName("resume")
    val execContext = suffixedName("execContext")

    // TODO do we need to freshen any of these?
    val x1 = newTermName("x$1")
    val tr = newTermName("tr")
    val onCompleteHandler = suffixedName("onCompleteHandler")

    def fresh(name: TermName) = newTermName(c.fresh("" + name + "$"))
  }

  private[async] lazy val futureSystemOps = futureSystem.mkOps(c)

  private val execContext = futureSystemOps.execContext

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
      case _ => Block(stats: _*)
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
          val updateState = mkStateTree(nextState) // or increment?
          Some(mkHandlerCase(state, List(tryGetTree, updateState, mkResumeApply)))
        case _ =>
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
    val awaitable: c.Tree
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
                 nextState: Int = state + 1): this.type = {
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
                          budget: Int, private val toRename: Map[Symbol, c.Name]) {
    val asyncStates = ListBuffer[builder.AsyncState]()

    private var stateBuilder = new builder.AsyncStateBuilder(startState, toRename)
    // current state builder
    private var currState = startState

    private var remainingBudget = budget

    /* TODO Fall back to CPS plug-in if tree contains an `await` call. */
    def checkForUnsupportedAwait(tree: c.Tree) = if (tree exists {
      case Apply(fun, _) if fun.symbol == Async_await => true
      case _ => false
    }) c.abort(tree.pos, "await unsupported in this position") //throw new FallbackToCpsException

    def builderForBranch(tree: c.Tree, state: Int, nextState: Int, budget: Int): AsyncBlockBuilder = {
      val (branchStats, branchExpr) = tree match {
        case Block(s, e) => (s, e)
        case _ => (List(tree), c.literalUnit.tree)
      }
      new AsyncBlockBuilder(branchStats, branchExpr, state, nextState, budget, toRename)
    }

    // populate asyncStates
    for (stat <- stats) stat match {
      // the val name = await(..) pattern
      case ValDef(mods, name, tpt, Apply(fun, args)) if fun.symbol == Async_await =>
        asyncStates += stateBuilder.complete(args.head, toRename(stat.symbol).toTermName, tpt).result // complete with await
        if (remainingBudget > 0)
          remainingBudget -= 1
        else
          assert(false, "too many invocations of `await` in current method")
        currState += 1
        stateBuilder = new builder.AsyncStateBuilder(currState, toRename)

      case ValDef(mods, name, tpt, rhs) if toRename contains stat.symbol =>
        checkForUnsupportedAwait(rhs)

        // when adding assignment need to take `toRename` into account
        stateBuilder.addVarDef(mods, toRename(stat.symbol).toTermName, tpt, rhs)

      case If(cond, thenp, elsep) if stat exists isAwait =>
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
            val builder = builderForBranch(tree, state, currState + ifBudget, branchBudget)
            asyncStates ++= builder.asyncStates
        }

        // create new state builder for state `currState + ifBudget`
        currState = currState + ifBudget
        stateBuilder = new builder.AsyncStateBuilder(currState, toRename)

      case Match(scrutinee, cases) if stat exists isAwait =>
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
        }

        // create new state builder for state `currState + matchBudget`
        currState = currState + matchBudget
        stateBuilder = new builder.AsyncStateBuilder(currState, toRename)

      case ClassDef(_, name, _, _) =>
        // do not allow local class definitions, because of SI-5467 (specific to case classes, though)
        c.error(stat.pos, s"Local class ${name.decoded} illegal within `async` block")

      case ModuleDef(_, name, _) =>
        // local object definitions lead to spurious type errors (because of resetAllAttrs?)
        c.error(stat.pos, s"Local object ${name.decoded} illegal within `async` block")

      case _ =>
        checkForUnsupportedAwait(stat)
        stateBuilder += stat
    }
    // complete last state builder (representing the expressions after the last await)
    stateBuilder += expr
    val lastState = stateBuilder.complete(endState).result
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
        case _ =>
          val initCases = for (state <- asyncStates.toList.init) yield state.mkHandlerCaseForState()
          initCases :+ caseForLastState
      }
    }
  }

  private val Boolean_ShortCircuits: Set[Symbol] = {
    import definitions.BooleanClass
    def BooleanTermMember(name: String) = BooleanClass.typeSignature.member(newTermName(name).encodedName)
    val Boolean_&& = BooleanTermMember("&&")
    val Boolean_|| = BooleanTermMember("||")
    Set(Boolean_&&, Boolean_||)
  }

  def isByName(fun: Tree): (Int => Boolean) = {
    if (Boolean_ShortCircuits contains fun.symbol) i => true
    else fun.tpe match {
      case MethodType(params, _) =>
        val isByNameParams = params.map(_.asTerm.isByNameParam)
        (i: Int) => isByNameParams.applyOrElse(i, (_: Int) => false)
      case _ => Map()
    }
  }

  private def isAwait(fun: Tree) = {
    fun.symbol == defn.Async_await
  }

  private[async] class LiftableVarTraverser extends Traverser {
    var blockId = 0
    var valDefBlockId = Map[Symbol, (ValDef, Int)]()
    val liftable = collection.mutable.Set[ValDef]()


    def reportUnsupportedAwait(tree: Tree, whyUnsupported: String) {
      val badAwaits = tree collect {
        case rt: RefTree if rt.symbol == Async_await => rt
      }
      badAwaits foreach {
        tree =>
          c.error(tree.pos, s"await must not be used under a $whyUnsupported.")
      }
    }

    override def traverse(tree: Tree) = {
      tree match {
        case cd: ClassDef =>
          val kind = if (cd.symbol.asClass.isTrait) "trait" else "class"
          reportUnsupportedAwait(tree, s"nested ${kind}")
        case md: ModuleDef =>
          reportUnsupportedAwait(tree, "nested object")
        case _: Function =>
          reportUnsupportedAwait(tree, "nested anonymous function")
        case If(cond, thenp, elsep) if tree exists isAwait =>
          traverse(cond)
          blockId += 1
          traverse(thenp)
          blockId += 1
          traverse(elsep)
          blockId += 1
        case Match(selector, cases) if tree exists isAwait =>
          traverse(selector)
          blockId += 1
          cases foreach {
            c => traverse(c); blockId += 1
          }
        case Apply(fun, args) if isAwait(fun) =>
          traverseTrees(args)
          traverse(fun)
          blockId += 1
        case Apply(fun, args) =>
          val isInByName = isByName(fun)
          for ((arg, index) <- args.zipWithIndex) {
            if (!isInByName(index)) traverse(arg)
            else reportUnsupportedAwait(arg, "by-name argument")
          }
          traverse(fun)
        case vd: ValDef =>
          super.traverse(tree)
          valDefBlockId += (vd.symbol ->(vd, blockId))
          if (vd.rhs.symbol == Async_await) liftable += vd
        case as: Assign =>
          if (as.rhs.symbol == Async_await) liftable += valDefBlockId.getOrElse(as.symbol, c.abort(as.pos, "await may only be assigned to a var/val defined in the async block."))._1

          super.traverse(tree)
        case rt: RefTree =>
          valDefBlockId.get(rt.symbol) match {
            case Some((vd, defBlockId)) if defBlockId != blockId =>
              liftable += vd
            case _ =>
          }
          super.traverse(tree)
        case _ => super.traverse(tree)
      }
    }
  }


  /** `termSym( (_: Foo).bar(null: A, null: B)` will return the symbol of `bar`, after overload resolution. */
  private def methodSym(apply: c.Expr[Any]): Symbol = {
    val tree2: Tree = c.typeCheck(apply.tree) // TODO why is this needed?
    tree2.collect {
      case s: SymTree if s.symbol.isMethod => s.symbol
    }.headOption.getOrElse(sys.error(s"Unable to find a method symbol in ${apply.tree}"))
  }

  private[async] object defn {
    def mkList_apply[A](args: List[Expr[A]]): Expr[List[A]] = {
      c.Expr(Apply(Ident(definitions.List_apply), args.map(_.tree)))
    }

    def mkList_contains[A](self: Expr[List[A]])(elem: Expr[Any]) = reify(self.splice.contains(elem.splice))

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

    val TryClass = c.mirror.staticClass("scala.util.Try")
    val TryAnyType = appliedType(TryClass.toType, List(definitions.AnyTpe))
    val NonFatalClass = c.mirror.staticModule("scala.util.control.NonFatal")

    val Async_await = {
      val asyncMod = c.mirror.staticModule("scala.async.Async")
      val tpe = asyncMod.moduleClass.asType.toType
      tpe.member(c.universe.newTermName("await"))
    }
  }

}
