package scala.async.internal

import reflect.internal.Flags._

trait LiveVariables {
  self: AsyncMacro =>
  import global._

  /**
   *  Returns for a given state a list of fields (as trees) that should be nulled out
   *  upon resuming that state (at the beginning of `resume`).
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
   *  @return  a map which indicates for a given field (the key) the states in which it should be nulled out
   */
  def liveVars(asyncStates: List[AsyncState], liftables: List[Tree]): Map[Tree, Set[Int]] = {
    val liftedSyms: Set[Symbol] = // include only vars
      liftables.filter {
        case ValDef(mods, _, _, _) => mods.hasFlag(MUTABLE)
        case _ => false
      }.map(_.symbol).toSet

    // determine which fields should be live also at the end (will not be nulled out)
    val noNull: Set[Symbol] = liftedSyms.filter { sym =>
      liftables.exists { tree =>
        !liftedSyms.contains(tree.symbol) && tree.exists(_.symbol == sym)
      }
    }

    /**
     *  Traverse statements of an `AsyncState`, collect `Ident`-s refering to lifted fields.
     *
     *  @param  as  a state of an `async` expression
     *  @return     a set of lifted fields that are used within state `as`
     */
    def fieldsUsedIn(as: AsyncState): Set[Symbol] = {
      class FindUseTraverser extends Traverser {
        var usedFields = Set[Symbol]()
        override def traverse(tree: Tree) = tree match {
          case Ident(_) if liftedSyms(tree.symbol) =>
            usedFields += tree.symbol
          case _ =>
            super.traverse(tree)
        }
      }
      val findUses = new FindUseTraverser
      findUses.traverse(Block(as.stats: _*))
      findUses.usedFields
    }

    /* Build the control-flow graph.
     *
     * A state `i` is contained in the list that is the value to which
     * key `j` maps iff control can flow from state `j` to state `i`.
     */
    val cfg: Map[Int, List[Int]] = asyncStates.map(as => (as.state -> as.nextStates)).toMap

    /** Tests if `state1` is a predecessor of `state2`.
     */
    def isPred(state1: Int, state2: Int, seen: Set[Int] = Set()): Boolean =
      if (seen(state1)) false  // breaks cycles in the CFG
      else cfg get state1 match {
        case Some(nextStates) =>
          nextStates.contains(state2) || nextStates.exists(isPred(_, state2, seen + state1))
        case None =>
          false
      }

    val finalState = asyncStates.find(as => !asyncStates.exists(other => isPred(as.state, other.state))).get

    for (as <- asyncStates)
      AsyncUtils.vprintln(s"fields used in state #${as.state}: ${fieldsUsedIn(as).mkString(", ")}")

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
    var pred       = List[AsyncState]()  // current predecessor states
    var hasChanged = true                // if something has changed we need to continue iterating

    while (hasChanged) {
      hasChanged = false

      for (cs <- currStates) {
        val LVentryOld = LVentry(cs.state)
        val LVentryNew = LVexit(cs.state) ++ fieldsUsedIn(cs)
        if (!LVentryNew.sameElements(LVentryOld)) {
          LVentry = LVentry + (cs.state -> LVentryNew)
          hasChanged = true
        }
      }

      pred = currStates.flatMap(cs => asyncStates.filter(_.nextStates.contains(cs.state)))

      for (p <- pred) {
        val LVexitOld = LVexit(p.state)
        val LVexitNew = p.nextStates.flatMap(succ => LVentry(succ)).toSet
        if (!LVexitNew.sameElements(LVexitOld)) {
          LVexit = LVexit + (p.state -> LVexitNew)
          hasChanged = true
        }
      }

      currStates = pred
    }

    for (as <- asyncStates) {
      AsyncUtils.vprintln(s"LVentry at state #${as.state}: ${LVentry(as.state).mkString(", ")}")
      AsyncUtils.vprintln(s"LVexit  at state #${as.state}: ${LVexit(as.state).mkString(", ")}")
    }

    def lastUsagesOf(field: Tree, at: AsyncState, avoid: Set[AsyncState]): Set[Int] =
      if (avoid(at)) Set()
      else LVentry get at.state match {
        case Some(fields) if fields.exists(_ == field.symbol) =>
          Set(at.state)
        case _ =>
          val preds = asyncStates.filter(_.nextStates.contains(at.state)).toSet
          preds.flatMap(p => lastUsagesOf(field, p, avoid + at))
      }

    val lastUsages: Map[Tree, Set[Int]] =
      liftables.map(fld => (fld -> lastUsagesOf(fld, finalState, Set()))).toMap

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
