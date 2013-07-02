package scala.async

trait Lifter {
  self: AsyncMacro =>
  import global._

  /**
   * Identify which DefTrees are used (including transitively) which are declared
   * in some state but used (including transitively) in another state.
   *
   * These will need to be lifted to class members of the state machine.
   */
  def liftables(asyncStates: List[AsyncState]): List[Tree] = {
    object companionship {
      private val companions = collection.mutable.Map[Symbol, Symbol]()
      private val companionsInverse = collection.mutable.Map[Symbol, Symbol]()
      private def record(sym1: Symbol, sym2: Symbol) {
        companions(sym1) = sym2
        companions(sym2) = sym1
      }

      def record(defs: List[Tree]) {
        // Keep note of local companions so we rename them consistently
        // when lifting.
        val comps = for {
          cd@ClassDef(_, _, _, _) <- defs
          md@ModuleDef(_, _, _) <- defs
          if (cd.name.toTermName == md.name)
        } record(cd.symbol, md.symbol)
      }
      def companionOf(sym: Symbol): Symbol = {
        companions.get(sym).orElse(companionsInverse.get(sym)).getOrElse(NoSymbol)
      }
    }


    val defs: Map[Tree, Int] = {
      /** Collect the DefTrees directly enclosed within `t` that have the same owner */
      def collectDirectlyEnclosedDefs(t: Tree): List[DefTree] = t match {
        case dt: DefTree => dt :: Nil
        case _: Function => Nil
        case t           =>
          val childDefs = t.children.flatMap(collectDirectlyEnclosedDefs(_))
          companionship.record(childDefs)
          childDefs
      }
      asyncStates.flatMap {
        asyncState =>
          val defs = collectDirectlyEnclosedDefs(Block(asyncState.allStats: _*))
          defs.map((_, asyncState.state))
      }.toMap
    }

    // In which block are these symbols defined?
    val symToDefiningState: Map[Symbol, Int] = defs.map {
      case (k, v) => (k.symbol, v)
    }

    // The definitions trees
    val symToTree: Map[Symbol, Tree] = defs.map {
      case (k, v) => (k.symbol, k)
    }

    // The direct references of each definition tree
    val defSymToReferenced: Map[Symbol, List[Symbol]] = defs.keys.map {
      case tree => (tree.symbol, tree.collect {
        case rt: RefTree if symToDefiningState.contains(rt.symbol) => rt.symbol
      })
    }.toMap

    // The direct references of each block, excluding references of `DefTree`-s which
    // are already accounted for.
    val stateIdToDirectlyReferenced: Map[Int, List[Symbol]] = {
      val refs: List[(Int, Symbol)] = asyncStates.flatMap(
        asyncState => asyncState.stats.filterNot(_.isDef).flatMap(_.collect {
          case rt: RefTree if symToDefiningState.contains(rt.symbol) => (asyncState.state, rt.symbol)
        })
      )
      toMultiMap(refs)
    }

    def liftableSyms: Set[Symbol] = {
      val liftableMutableSet = collection.mutable.Set[Symbol]()
      def markForLift(sym: Symbol) {
        if (!liftableMutableSet(sym)) {
          liftableMutableSet += sym

          // Only mark transitive references of defs, modules and classes. The RHS of lifted vals/vars
          // stays in its original location, so things that it refers to need not be lifted.
          if (!(sym.isVal || sym.isVar))
            defSymToReferenced(sym).foreach(sym2 => markForLift(sym2))
        }
      }
      // Start things with DefTrees directly referenced from statements from other states...
      val liftableStatementRefs: List[Symbol] = stateIdToDirectlyReferenced.toList.flatMap {
        case (i, syms) => syms.filter(sym => symToDefiningState(sym) != i)
      }
      // .. and likewise for DefTrees directly referenced by other DefTrees from other states
      val liftableRefsOfDefTrees = defSymToReferenced.toList.flatMap {
        case (referee, referents) => referents.filter(sym => symToDefiningState(sym) != symToDefiningState(referee))
      }
      // Mark these for lifting, which will follow transitive references.
      (liftableStatementRefs ++ liftableRefsOfDefTrees).foreach(markForLift)
      liftableMutableSet.toSet
    }

    val lifted = liftableSyms.map(symToTree).toList.map {
      case vd@ValDef(_, _, tpt, rhs)                          =>
        import reflect.internal.Flags._
        val sym = vd.symbol
        sym.setFlag(MUTABLE | STABLE | PRIVATE | LOCAL)
        sym.name = name.fresh(sym.name.toTermName)
        sym.modifyInfo(_.deconst)
        ValDef(vd.symbol, gen.mkZero(vd.symbol.info)).setPos(vd.pos)
      case dd@DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        import reflect.internal.Flags._
        val sym = dd.symbol
        sym.name = this.name.fresh(sym.name.toTermName)
        sym.setFlag(PRIVATE | LOCAL)
        DefDef(dd.symbol, rhs).setPos(dd.pos)
      case cd@ClassDef(_, _, _, impl)                         =>
        import reflect.internal.Flags._
        val sym = cd.symbol
        sym.name = newTypeName(name.fresh(sym.name.toString).toString)
        companionship.companionOf(cd.symbol) match {
          case NoSymbol =>
          case moduleSymbol =>
            moduleSymbol.name = sym.name.toTermName
            moduleSymbol.moduleClass.name = moduleSymbol.name.toTypeName
        }
        ClassDef(cd.symbol, impl).setPos(cd.pos)
      case md@ModuleDef(_, _, impl)                           =>
        import reflect.internal.Flags._
        val sym = md.symbol
        companionship.companionOf(md.symbol) match {
          case NoSymbol =>
            sym.name = name.fresh(sym.name.toTermName)
            sym.moduleClass.name = sym.name.toTypeName
          case classSymbol => // will be renamed by `case ClassDef` above.
        }
        ModuleDef(md.symbol, impl).setPos(md.pos)
      case td@TypeDef(_, _, _, rhs)                           =>
        import reflect.internal.Flags._
        val sym = td.symbol
        sym.name = newTypeName(name.fresh(sym.name.toString).toString)
        TypeDef(td.symbol, rhs).setPos(td.pos)
    }
    lifted
  }
}
