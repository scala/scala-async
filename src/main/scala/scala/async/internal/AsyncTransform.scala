package scala.async.internal

trait AsyncTransform {
  self: AsyncMacro =>

  import global._

  val asyncBase: AsyncBase

  def asyncTransform[T](body: Tree, execContext: Tree)
                       (resultType: WeakTypeTag[T]): Tree = {

    // We annotate the type of the whole expression as `T @uncheckedBounds` so as not to introduce
    // warnings about non-conformant LUBs. See SI-7694
    // This implicit propagates the annotated type in the type tag.
    implicit val uncheckedBoundsResultTag: WeakTypeTag[T] = WeakTypeTag[T](rootMirror, FixedMirrorTypeCreator(rootMirror, uncheckedBounds(resultType.tpe)))

    reportUnsupportedAwaits(body)

    // Transform to A-normal form:
    //  - no await calls in qualifiers or arguments,
    //  - if/match only used in statement position.
    val anfTree0: Block = anfTransform(body)

    val anfTree = futureSystemOps.postAnfTransform(anfTree0)

    val applyDefDefDummyBody: DefDef = {
      val applyVParamss = List(List(ValDef(Modifiers(Flag.PARAM), name.tr, TypeTree(futureSystemOps.tryType[Any]), EmptyTree)))
      DefDef(NoMods, name.apply, Nil, applyVParamss, TypeTree(definitions.UnitTpe), literalUnit)
    }

    // Create `ClassDef` of state machine with empty method bodies for `resume` and `apply`.
    val stateMachine: ClassDef = {
      val body: List[Tree] = {
        val stateVar = ValDef(Modifiers(Flag.MUTABLE | Flag.PRIVATE | Flag.LOCAL), name.state, TypeTree(definitions.IntTpe), Literal(Constant(StateAssigner.Initial)))
        val result = ValDef(NoMods, name.result, TypeTree(futureSystemOps.promType[T](uncheckedBoundsResultTag)), futureSystemOps.createProm[T](uncheckedBoundsResultTag).tree)
        val execContextValDef = ValDef(NoMods, name.execContext, TypeTree(), execContext)

        val apply0DefDef: DefDef = {
          // We extend () => Unit so we can pass this class as the by-name argument to `Future.apply`.
          // See SI-1247 for the the optimization that avoids creation.
          DefDef(NoMods, name.apply, Nil, Nil, TypeTree(definitions.UnitTpe), Apply(Ident(name.apply), literalNull :: Nil))
        }
        List(emptyConstructor, stateVar, result, execContextValDef) ++ List(applyDefDefDummyBody, apply0DefDef)
      }

      val tryToUnit = appliedType(definitions.FunctionClass(1), futureSystemOps.tryType[Any], typeOf[Unit])
      val template = Template(List(tryToUnit, typeOf[() => Unit]).map(TypeTree(_)), emptyValDef, body)

      val t = ClassDef(NoMods, name.stateMachineT, Nil, template)
      typecheckClassDef(t)
    }

    val stateMachineClass = stateMachine.symbol
    val asyncBlock: AsyncBlock = {
      val symLookup = new SymLookup(stateMachineClass, applyDefDefDummyBody.vparamss.head.head.symbol)
      buildAsyncBlock(anfTree, symLookup)
    }

    logDiagnostics(anfTree, asyncBlock.asyncStates.map(_.toString))

    val liftedFields: List[Tree] = liftables(asyncBlock.asyncStates)

    // live variables analysis
    // the result map indicates in which states a given field should be nulled out
    val assignsOf = fieldsToNullOut(asyncBlock.asyncStates, liftedFields)

    for ((state, flds) <- assignsOf) {
      val assigns = flds.map { fld =>
        val fieldSym = fld.symbol
        Block(
          List(
            asyncBase.nullOut(global)(Expr[String](Literal(Constant(fieldSym.name.toString))), Expr[Any](Ident(fieldSym))).tree
          ),
          Assign(gen.mkAttributedStableRef(fieldSym.owner.thisType, fieldSym), mkZero(fieldSym.info))
        )
      }
      val asyncState = asyncBlock.asyncStates.find(_.state == state).get
      asyncState.stats = assigns ++ asyncState.stats
    }

    def startStateMachine: Tree = {
      val stateMachineSpliced: Tree = spliceMethodBodies(
        liftedFields,
        stateMachine,
        atMacroPos(asyncBlock.onCompleteHandler[T])
      )

      def selectStateMachine(selection: TermName) = Select(Ident(name.stateMachine), selection)

      Block(List[Tree](
        stateMachineSpliced,
        ValDef(NoMods, name.stateMachine, TypeTree(), Apply(Select(New(Ident(stateMachine.symbol)), nme.CONSTRUCTOR), Nil)),
        futureSystemOps.spawn(Apply(selectStateMachine(name.apply), Nil), selectStateMachine(name.execContext))
      ),
      futureSystemOps.promiseToFuture(Expr[futureSystem.Prom[T]](selectStateMachine(name.result))).tree)
    }

    val isSimple = asyncBlock.asyncStates.size == 1
    if (isSimple)
      futureSystemOps.spawn(body, execContext) // generate lean code for the simple case of `async { 1 + 1 }`
    else
      startStateMachine
  }

  def logDiagnostics(anfTree: Tree, states: Seq[String]) {
    def location = try {
      macroPos.source.path
    } catch {
      case _: UnsupportedOperationException =>
        macroPos.toString
    }

    AsyncUtils.vprintln(s"In file '$location':")
    AsyncUtils.vprintln(s"${macroApplication}")
    AsyncUtils.vprintln(s"ANF transform expands to:\n $anfTree")
    states foreach (s => AsyncUtils.vprintln(s))
  }

  /**
   *  Build final `ClassDef` tree of state machine class.
   *
   *  @param  liftables  trees of definitions that are lifted to fields of the state machine class
   *  @param  tree       `ClassDef` tree of the state machine class
   *  @param  applyBody  tree of onComplete handler (`apply` method)
   *  @return            transformed `ClassDef` tree of the state machine class
   */
  def spliceMethodBodies(liftables: List[Tree], tree: ClassDef, applyBody: Tree): Tree = {
    val liftedSyms = liftables.map(_.symbol).toSet
    val stateMachineClass = tree.symbol
    liftedSyms.foreach {
      sym =>
        if (sym != null) {
          sym.owner = stateMachineClass
          if (sym.isModule)
            sym.moduleClass.owner = stateMachineClass
        }
    }
    // Replace the ValDefs in the splicee with Assigns to the corresponding lifted
    // fields. Similarly, replace references to them with references to the field.
    //
    // This transform will only be run on the RHS of `def foo`.
    class UseFields extends MacroTypingTransformer {
      override def transform(tree: Tree): Tree = tree match {
        case _ if currentOwner == stateMachineClass          =>
          super.transform(tree)
        case ValDef(_, _, _, rhs) if liftedSyms(tree.symbol) =>
          atOwner(currentOwner) {
            val fieldSym = tree.symbol
            val lhs = atPos(tree.pos) {
              gen.mkAttributedStableRef(fieldSym.owner.thisType, fieldSym)
            }
            val assign = treeCopy.Assign(tree, lhs, transform(rhs)).setType(definitions.UnitTpe)
            changeOwner(assign, tree.symbol, currentOwner)
            assign
          }
        case _: DefTree if liftedSyms(tree.symbol)           =>
          EmptyTree
        case Ident(name) if liftedSyms(tree.symbol)          =>
          val fieldSym = tree.symbol
          atPos(tree.pos) {
            gen.mkAttributedStableRef(fieldSym.owner.thisType, fieldSym).setType(tree.tpe)
          }
        case _                                               =>
          super.transform(tree)
      }
    }

    val liftablesUseFields = liftables.map {
      case vd: ValDef => vd
      case x          =>
        val useField = new UseFields()
        //.substituteSymbols(fromSyms, toSyms)
        useField.atOwner(stateMachineClass)(useField.transform(x))
    }

    tree.children.foreach {
      t =>
        new ChangeOwnerAndModuleClassTraverser(callSiteTyper.context.owner, tree.symbol).traverse(t)
    }
    val treeSubst = tree

    /* Fixes up DefDef: use lifted fields in `body` */
    def fixup(dd: DefDef, body: Tree, ctx: analyzer.Context): Tree = {
      val spliceeAnfFixedOwnerSyms = body
      val useField = new UseFields()
      val newRhs = useField.atOwner(dd.symbol)(useField.transform(spliceeAnfFixedOwnerSyms))
      val typer = global.analyzer.newTyper(ctx.make(dd, dd.symbol))
      treeCopy.DefDef(dd, dd.mods, dd.name, dd.tparams, dd.vparamss, dd.tpt, typer.typed(newRhs))
    }

    liftablesUseFields.foreach(t => if (t.symbol != null) stateMachineClass.info.decls.enter(t.symbol))

    val result0 = transformAt(treeSubst) {
      case t@Template(parents, self, stats) =>
        (ctx: analyzer.Context) => {
          treeCopy.Template(t, parents, self, liftablesUseFields ++ stats)
        }
    }
    val result = transformAt(result0) {
      case dd@DefDef(_, name.apply, _, List(List(_)), _, _) if dd.symbol.owner == stateMachineClass =>
        (ctx: analyzer.Context) =>
          val typedTree = fixup(dd, changeOwner(applyBody, callSiteTyper.context.owner, dd.symbol), ctx)
          typedTree
    }
    result
  }

  def typecheckClassDef(cd: ClassDef): ClassDef = {
    val Block(cd1 :: Nil, _) = callSiteTyper.typedPos(macroPos)(Block(cd :: Nil, Literal(Constant(()))))
    cd1.asInstanceOf[ClassDef]
  }
}
