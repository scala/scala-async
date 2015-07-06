package scala.async.internal

trait LiveVariables {
  self: AsyncMacro =>
  import c.universe._
  import Flag._

  /**
   *  Returns for a given state a list of fields (as trees) that should be nulled out
   *  upon resuming that state (at the beginning of `resume`).
   *
   *  @param   asyncStates the states of an `async` block
   *  @param   liftables   the lifted fields
   *  @return  a map mapping a state to the fields that should be nulled out
   *           upon resuming that state
   */
  def fieldsToNullOut(asyncStates: List[AsyncState], liftables: List[Tree]): Map[Int, List[Tree]] = {
    // live variables analysis:
    // the result map indicates in which states a given field should be nulled out
    val liveVarsMap: Map[Tree, Set[Int]] = liveVars(asyncStates, liftables)

    var assignsOf = Map[Int, List[Tree]]()

    for ((fld, where) <- liveVarsMap; state <- where)
      assignsOf get state match {
        case None =>
          assignsOf += (state -> List(fld))
        case Some(trees) if !trees.exists(_.symbol == fld.symbol) =>
          assignsOf += (state -> (fld +: trees))
        case _ =>
          /* do nothing */
      }

    assignsOf
  }

  /**
   *  Live variables data-flow analysis.
   *
   *  The goal is to find, for each lifted field, the last state where the field is used.
   *  In all direct successor states which are not (indirect) predecessors of that last state
   *  (possible through loops), the corresponding field should be nulled out (at the beginning of
   *  `resume`).
   *
   *  @param   asyncStates the states of an `async` block
   *  @param   liftables   the lifted fields
   *  @return              a map which indicates for a given field (the key) the states in which it should be nulled out
   */
  def liveVars(asyncStates: List[AsyncState], liftables: List[Tree]): Map[Tree, Set[Int]] = {
    val liftedSyms: Set[Symbol] = // include only vars
      liftables.filter {
        case ValDef(mods, _, _, _) => mods.hasFlag(MUTABLE)
        case _ => false
      }.map(_.symbol).toSet

    // determine which fields should be live also at the end (will not be nulled out)
    val noNull: Set[Symbol] = liftedSyms.filter { sym =>
      val typeSym = tpe(sym).typeSymbol
      (typeSym.isClass && (typeSym.asClass.isPrimitive || typeSym == definitions.NothingClass)) || liftables.exists { tree =>
        !liftedSyms.contains(tree.symbol) && tree.exists(_.symbol == sym)
      }
    }
    AsyncUtils.vprintln(s"fields never zero-ed out: ${noNull.mkString(", ")}")

    /**
     *  Traverse statements of an `AsyncState`, collect `Ident`-s refering to lifted fields.
     *
     *  @param  as  a state of an `async` expression
     *  @return     a set of lifted fields that are used within state `as`
     */
    def fieldsUsedIn(as: AsyncState): ReferencedFields = {
      class FindUseTraverser extends AsyncTraverser {
        var usedFields = Set[Symbol]()
        var capturedFields = Set[Symbol]()
        private def capturing[A](body: => A): A = {
          val saved = capturing
          try {
            capturing = true
            body
          } finally capturing = saved
        }
        private def capturingCheck(tree: Tree) = capturing(tree foreach check)
        private var capturing: Boolean = false
        private def check(tree: Tree) {
          tree match {
            case Ident(_) if liftedSyms(tree.symbol) =>
              if (capturing)
                capturedFields += tree.symbol
              else
                usedFields += tree.symbol
            case _ =>
          }
        }
        override def traverse(tree: Tree) = {
          check(tree)
          super.traverse(tree)
        }

        override def nestedClass(classDef: ClassDef): Unit = capturingCheck(classDef)

        override def nestedModule(module: ModuleDef): Unit = capturingCheck(module)

        override def nestedMethod(defdef: DefDef): Unit = capturingCheck(defdef)

        override def byNameArgument(arg: Tree): Unit = capturingCheck(arg)

        override def function(function: Function): Unit = capturingCheck(function)

        override def patMatFunction(tree: Match): Unit = capturingCheck(tree)
      }

      val findUses = new FindUseTraverser
      findUses.traverse(Block(as.stats: _*))
      ReferencedFields(findUses.usedFields, findUses.capturedFields)
    }
    case class ReferencedFields(used: Set[Symbol], captured: Set[Symbol]) {
      override def toString = s"used: ${used.mkString(",")}\ncaptured: ${captured.mkString(",")}"
    }

    /* Build the control-flow graph.
     *
     * A state `i` is contained in the list that is the value to which
     * key `j` maps iff control can flow from state `j` to state `i`.
     */
    val cfg: Map[Int, List[Int]] = asyncStates.map(as => (as.state -> as.nextStates)).toMap

    /** Tests if `state1` is a predecessor of `state2`.
     */
    def isPred(state1: Int, state2: Int): Boolean = {
      val seen = scala.collection.mutable.HashSet[Int]()

      def isPred0(state1: Int, state2: Int): Boolean = 
        if(state1 == state2) false
        else if (seen(state1)) false  // breaks cycles in the CFG
        else cfg get state1 match {
          case Some(nextStates) =>
            seen += state1
            nextStates.contains(state2) || nextStates.exists(isPred0(_, state2))
          case None =>
            false
        }

      isPred0(state1, state2)
    }

    val finalState = asyncStates.find(as => !asyncStates.exists(other => isPred(as.state, other.state))).get

    for (as <- asyncStates)
      AsyncUtils.vprintln(s"fields used in state #${as.state}: ${fieldsUsedIn(as)}")

    /* Backwards data-flow analysis. Computes live variables information at entry and exit
     * of each async state.
     *
     * Compute using a simple fixed point iteration:
     *
     * 1. currStates = List(finalState)
     * 2. for each cs \in currStates, compute LVentry(cs) from LVexit(cs) and used fields information for cs
     * 3. record if LVentry(cs) has changed for some cs.
     * 4. obtain predecessors pred of each cs \in currStates
     * 5. for each p \in pred, compute LVexit(p) as union of the LVentry of its successors
     * 6. currStates = pred
     * 7. repeat if something has changed
     */

    var LVentry = Map[Int, Set[Symbol]]() withDefaultValue Set[Symbol]()
    var LVexit  = Map[Int, Set[Symbol]]() withDefaultValue Set[Symbol]()

    // All fields are declared to be dead at the exit of the final async state, except for the ones
    // that cannot be nulled out at all (those in noNull), because they have been captured by a nested def.
    LVexit = LVexit + (finalState.state -> noNull)

    var currStates = List(finalState)    // start at final state
    var captured: Set[Symbol] = Set()

    while (!currStates.isEmpty) {
      var entryChanged: List[AsyncState] = Nil

      for (cs <- currStates) {
        val LVentryOld = LVentry(cs.state)
        val referenced = fieldsUsedIn(cs)
        captured ++= referenced.captured
        val LVentryNew = LVexit(cs.state) ++ referenced.used
        if (!LVentryNew.sameElements(LVentryOld)) {
          LVentry = LVentry + (cs.state -> LVentryNew)
          entryChanged ::= cs
        }
      }

      val pred = entryChanged.flatMap(cs => asyncStates.filter(_.nextStates.contains(cs.state)))
      var exitChanged: List[AsyncState] = Nil

      for (p <- pred) {
        val LVexitOld = LVexit(p.state)
        val LVexitNew = p.nextStates.flatMap(succ => LVentry(succ)).toSet
        if (!LVexitNew.sameElements(LVexitOld)) {
          LVexit = LVexit + (p.state -> LVexitNew)
          exitChanged ::= p
        }
      }

      currStates = exitChanged
    }

    for (as <- asyncStates) {
      AsyncUtils.vprintln(s"LVentry at state #${as.state}: ${LVentry(as.state).mkString(", ")}")
      AsyncUtils.vprintln(s"LVexit  at state #${as.state}: ${LVexit(as.state).mkString(", ")}")
    }

    def lastUsagesOf(field: Tree, at: AsyncState): Set[Int] = {
      val avoid = scala.collection.mutable.HashSet[AsyncState]()

      def lastUsagesOf0(field: Tree, at: AsyncState): Set[Int] = {
        if (avoid(at)) Set()
        else if (captured(field.symbol)) {
          Set()
        }
        else LVentry get at.state match {
          case Some(fields) if fields.exists(_ == field.symbol) =>
            Set(at.state)
          case _ =>
            avoid += at
            val preds = asyncStates.filter(_.nextStates.contains(at.state)).toSet
            preds.flatMap(p => lastUsagesOf0(field, p))
        }
      }

      lastUsagesOf0(field, at)
    }

    val lastUsages: Map[Tree, Set[Int]] =
      liftables.map(fld => (fld -> lastUsagesOf(fld, finalState))).toMap

    for ((fld, lastStates) <- lastUsages)
      AsyncUtils.vprintln(s"field ${fld.symbol.name} is last used in states ${lastStates.mkString(", ")}")

    val nullOutAt: Map[Tree, Set[Int]] =
      for ((fld, lastStates) <- lastUsages) yield {
        val killAt = lastStates.flatMap { s =>
          if (s == finalState.state) Set()
          else {
            val lastAsyncState = asyncStates.find(_.state == s).get
            val succNums       = lastAsyncState.nextStates
            // all successor states that are not indirect predecessors
            // filter out successor states where the field is live at the entry
            succNums.filter(num => !isPred(num, s)).filterNot(num => LVentry(num).exists(_ == fld.symbol))
          }
        }
        (fld, killAt)
      }

    for ((fld, killAt) <- nullOutAt)
      AsyncUtils.vprintln(s"field ${fld.symbol.name} should be nulled out in states ${killAt.mkString(", ")}")

    nullOutAt
  }
}
