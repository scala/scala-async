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

  val execContextType = c.weakTypeOf[futureSystem.ExecContext]
  val execContext = futureSystemOps.execContext

  private val awaitMethod = awaitSym(c)

  /* Make a partial function literal handling case #num:
   * 
   *     {
   *       case any if any == num => rhs
   *     }
   */
  def mkHandler(num: Int, rhs: c.Expr[Any]): c.Expr[PartialFunction[Int, Unit]] = {
    /*
        val numLiteral = c.Expr[Int](Literal(Constant(num)))

        reify(new PartialFunction[Int, Unit] {
          def isDefinedAt(`x$1`: Int) =
            `x$1` == numLiteral.splice
          def apply(`x$1`: Int) = `x$1` match {
            case any: Int if any == numLiteral.splice =>
              rhs.splice
          }
        })
    */
    val rhsTree = c.resetAllAttrs(rhs.tree.duplicate)
    val handlerTree = mkHandlerTree(num, rhsTree)
    c.Expr(handlerTree).asInstanceOf[c.Expr[PartialFunction[Int, Unit]]]
  }

  def mkIncrStateTree(): c.Tree = {
    Assign(
      Ident(newTermName("state")),
      mkInt_+(c.Expr[Int](Ident(newTermName("state"))))(c.literal(1)).tree
    )
  }

  def mkStateTree(nextState: Int): c.Tree =
    Assign(
      Ident(newTermName("state")),
      Literal(Constant(nextState)))

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

  def mkHandlerCase(num: Int, rhs: c.Tree): CaseDef =
    CaseDef(
      // pattern
      Bind(newTermName("any"), Typed(Ident(nme.WILDCARD), Ident(definitions.IntClass))),
      // guard
      mkAny_==(c.Expr(Ident(newTermName("any"))))(c.literal(num)).tree,
      rhs
    )

  def mkHandlerTreeFor(cases: List[(CaseDef, Int)]): c.Tree = {
    val partFunIdent = Ident(c.mirror.staticClass("scala.PartialFunction"))
    val intIdent = Ident(definitions.IntClass)
    val unitIdent = Ident(definitions.UnitClass)

    val caseCheck =
      defn.mkList_contains(defn.mkList_apply(cases.map(p => c.literal(p._2))))(c.Expr(Ident(newTermName("x$1"))))

    Block(List(
      // anonymous subclass of PartialFunction[Int, Unit]
      // TODO subclass AbstractPartialFunction
      ClassDef(Modifiers(FINAL), newTypeName("$anon"), List(), Template(List(AppliedTypeTree(partFunIdent, List(intIdent, unitIdent))),
        emptyValDef, List(
          DefDef(Modifiers(), nme.CONSTRUCTOR, List(), List(List()), TypeTree(),
            Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), Literal(Constant(())))),

          DefDef(Modifiers(), newTermName("isDefinedAt"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("x$1"), intIdent, EmptyTree))), TypeTree(),
            caseCheck.tree),

          DefDef(Modifiers(), newTermName("apply"), List(), List(List(ValDef(Modifiers(PARAM), newTermName("x$1"), intIdent, EmptyTree))), TypeTree(),
            Match(Ident(newTermName("x$1")), cases.map(_._1)) // combine all cases into a single match
          )
        ))
      )),
      Apply(Select(New(Ident(newTypeName("$anon"))), nme.CONSTRUCTOR), List())
    )
  }

  def mkHandlerTree(num: Int, rhs: c.Tree): c.Tree =
    mkHandlerTreeFor(List(mkHandlerCase(num, rhs) -> num))

  class AsyncState(stats: List[c.Tree], val state: Int, val nextState: Int) {
    val body: c.Tree =
      if (stats.size == 1) stats.head
      else Block(stats: _*)

    val varDefs: List[(TermName, Type)] = List()

    def mkHandlerCaseForState(): CaseDef =
      mkHandlerCase(state, Block((stats :+ mkStateTree(nextState) :+ Apply(Ident("resume"), List())): _*))

    def mkHandlerTreeForState(): c.Tree =
      mkHandlerTree(state, Block((stats :+ mkStateTree(nextState) :+ Apply(Ident("resume"), List())): _*))

    def mkHandlerTreeForState(nextState: Int): c.Tree =
      mkHandlerTree(state, Block((stats :+ mkStateTree(nextState) :+ Apply(Ident("resume"), List())): _*))

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

    override def mkHandlerTreeForState(): c.Tree =
      mkHandlerTree(state, Block(stats: _*))

    //TODO mkHandlerTreeForState(nextState: Int)

    override def mkHandlerCaseForState(): CaseDef =
      mkHandlerCase(state, Block(stats: _*))

    override val toString: String =
      s"AsyncStateWithIf #$state, next = $nextState"
  }

  abstract class AsyncStateWithAwait(stats: List[c.Tree], state: Int, nextState: Int)
    extends AsyncState(stats, state, nextState) {
    val awaitable: c.Tree
    val resultName: TermName
    val resultType: Type

    override val toString: String =
      s"AsyncStateWithAwait #$state, next = $nextState"

    /* Make an `onComplete` invocation:
     * 
     *     awaitable.onComplete {
     *       case tr =>
     *         resultName = tr.get
     *         resume()
     *     }
     */
    def mkOnCompleteTree: c.Tree = {
      val assignTree =
        Assign(
          Ident(resultName.toString),
          mkTry_get(c.Expr(Ident("tr"))).tree
        )
      val handlerTree =
        Match(
          EmptyTree,
          List(
            CaseDef(Bind(newTermName("tr"), Ident("_")), EmptyTree,
              Block(assignTree, Apply(Ident("resume"), List())) // rhs of case
            )
          )
        )
      futureSystemOps.onComplete(c.Expr(awaitable), c.Expr(handlerTree), execContext).tree
    }

    /* Make an `onComplete` invocation which increments the state upon resuming:
     * 
     *     awaitable.onComplete {
     *       case tr =>
     *         resultName = tr.get
     *         state += 1
     *         resume()
     *     }
     */
    def mkOnCompleteIncrStateTree: c.Tree = {
      val tryGetTree =
        Assign(
          Ident(resultName.toString),
          Select(Ident("tr"), Try_get)
        )

      val handlerTree =
        Function(List(ValDef(Modifiers(PARAM), newTermName("tr"), TypeTree(tryType), EmptyTree)), Block(tryGetTree, mkIncrStateTree(), Apply(Ident("resume"), List())))

      futureSystemOps.onComplete(c.Expr(awaitable), c.Expr(handlerTree), execContext).tree
    }

    def tryType = appliedType(c.mirror.staticClass("scala.util.Try").toType, List(resultType))

    /* Make an `onComplete` invocation which sets the state to `nextState` upon resuming:
     * 
     *     awaitable.onComplete {
     *       case tr =>
     *         resultName = tr.get
     *         state = `nextState`
     *         resume()
     *     }
     */
    def mkOnCompleteStateTree(nextState: Int): c.Tree = {
      val tryGetTree =
        Assign(
          Ident(resultName.toString),
          Select(Ident("tr"), Try_get)
        )
      val handlerTree =
        Function(List(ValDef(Modifiers(PARAM), newTermName("tr"), TypeTree(tryType), EmptyTree)), Block(tryGetTree, mkStateTree(nextState), Apply(Ident("resume"), List())))

      futureSystemOps.onComplete(c.Expr(awaitable), c.Expr(handlerTree), execContext).tree
    }

    /* Make a partial function literal handling case #num:
     * 
     *     {
     *       case any if any == num =>
     *         stats
     *         awaitable.onComplete {
     *           case tr =>
     *             resultName = tr.get
     *             resume()
     *         }
     *     }
     */
    def mkHandlerForState(num: Int): c.Expr[PartialFunction[Int, Unit]] = {
      assert(awaitable != null)
      builder.mkHandler(num, c.Expr[Unit](Block((stats :+ mkOnCompleteTree): _*)))
    }

    /* Make a partial function literal handling case #num:
     * 
     *     {
     *       case any if any == num =>
     *         stats
     *         awaitable.onComplete {
     *           case tr =>
     *             resultName = tr.get
     *             state += 1
     *             resume()
     *         }
     *     }
     */
    override def mkHandlerTreeForState(): c.Tree = {
      assert(awaitable != null)
      mkHandlerTree(state, Block((stats :+ mkOnCompleteIncrStateTree): _*))
    }

    override def mkHandlerTreeForState(nextState: Int): c.Tree = {
      assert(awaitable != null)
      mkHandlerTree(state, Block((stats :+ mkOnCompleteStateTree(nextState)): _*))
    }

    override def mkHandlerCaseForState(): CaseDef = {
      assert(awaitable != null)
      mkHandlerCase(state, Block((stats :+ mkOnCompleteIncrStateTree): _*))
    }

    override def varDefForResult: Option[c.Tree] =
      Some(mkVarDefTree(resultType, resultName))
  }

  /*
   * Builder for a single state of an async method.
   */
  class AsyncStateBuilder(state: Int, private var nameMap: Map[c.Symbol, c.Name]) extends Builder[c.Tree, AsyncState] {
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
      stats += c.resetAllAttrs(renamer.transform(stat).duplicate)
      this
    }

    //TODO do not ignore `mods`
    def addVarDef(mods: Any, name: TermName, tpt: c.Tree, rhs: c.Tree, extNameMap: Map[c.Symbol, c.Name]): this.type = {
      varDefs += (name -> tpt.tpe)
      nameMap ++= extNameMap // update name map
      this += Assign(Ident(name), c.resetAllAttrs(renamer.transform(rhs).duplicate))
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

    def clear(): Unit = {
      stats.clear()
      awaitable = null
      resultName = null
      resultType = null
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
      awaitable = c.resetAllAttrs(renamer.transform(awaitArg).duplicate)
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
      val cond = c.resetAllAttrs(condTree.duplicate)
      this += If(cond,
        Block(mkStateTree(thenState), Apply(Ident("resume"), List())),
        Block(mkStateTree(elseState), Apply(Ident("resume"), List())))
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
        case CaseDef(pat, guard, rhs) => CaseDef(pat, guard, Block(mkStateTree(num * perCasebudget + stateFirstCase), Apply(Ident("resume"), List())))
      }
      // 2. insert changed match tree at the end of the current state
      this += Match(c.resetAllAttrs(scrutTree.duplicate), newCases)
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
        case _ => (List(tree), Literal(Constant(())))
      }
      new AsyncBlockBuilder(branchStats, branchExpr, state, nextState, budget, nameMap)
    }

    // populate asyncStates
    for (stat <- stats) stat match {
      // the val name = await(..) pattern
      case ValDef(mods, name, tpt, Apply(fun, args)) if fun.symbol == awaitMethod =>
        val newName = c.fresh(name)
        toRename += (stat.symbol -> newName)

        asyncStates += stateBuilder.complete(args(0), newName, tpt, toRename).result // complete with await
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
            case CaseDef(_, _, rhs) => (List(rhs), Literal(Constant(())))
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

    def mkCombinedHandlerExpr(): c.Expr[PartialFunction[Int, Unit]] = {
      assert(asyncStates.size > 1)

      val cases = for (state <- asyncStates.toList) yield state.mkHandlerCaseForState()
      reify {
        c.Expr[PartialFunction[Int, Unit]](mkHandlerTreeFor(cases zip asyncStates.init.map(_.state))).splice: PartialFunction[Int, Unit]
      }
    }

    /* Builds the handler expression for a sequence of async states.
     */
    def mkHandlerExpr(): c.Expr[PartialFunction[Int, Unit]] = {
      assert(asyncStates.size > 1)

      var handlerExpr =
        c.Expr(asyncStates(0).mkHandlerTreeForState()).asInstanceOf[c.Expr[PartialFunction[Int, Unit]]]

      if (asyncStates.size == 2)
        handlerExpr
      else {
        for (asyncState <- asyncStates.tail.init) {
          // do not traverse first or last state
          val handlerTreeForNextState = asyncState.mkHandlerTreeForState()
          val currentHandlerTreeNaked = c.resetAllAttrs(handlerExpr.tree.duplicate)
          handlerExpr = mkPartialFunction_orElse(c.Expr(currentHandlerTreeNaked))(c.Expr(handlerTreeForNextState))
        }
        handlerExpr
      }
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

    val Try_get = methodSym(reify((null.asInstanceOf[scala.util.Try[Any]]).get))
  }

}
