/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async

import scala.reflect.macros.Context
import scala.collection.mutable

private[async] final case class AsyncAnalysis[C <: Context](c: C, asyncBase: AsyncBase) {
  import c.universe._

  val utils = TransformUtils[c.type](c)

  import utils._

  /**
   * Analyze the contents of an `async` block in order to:
   * - Report unsupported `await` calls under nested templates, functions, by-name arguments.
   *
   * Must be called on the original tree, not on the ANF transformed tree.
   */
  def reportUnsupportedAwaits(tree: Tree): Boolean = {
    val analyzer = new UnsupportedAwaitAnalyzer
    analyzer.traverse(tree)
    analyzer.hasUnsupportedAwaits
  }

  /**
   * Analyze the contents of an `async` block in order to:
   * - Find which local `ValDef`-s need to be lifted to fields of the state machine, based
   * on whether or not they are accessed only from a single state.
   *
   * Must be called on the ANF transformed tree.
   */
  def defTreesUsedInSubsequentStates(tree: Tree): List[DefTree] = {
    val analyzer = new AsyncDefinitionUseAnalyzer
    analyzer.traverse(tree)
    val liftable: List[DefTree] = (analyzer.valDefsToLift ++ analyzer.nestedMethodsToLift).toList.distinct
    liftable
  }

  private class UnsupportedAwaitAnalyzer extends AsyncTraverser {
    var hasUnsupportedAwaits = false

    override def nestedClass(classDef: ClassDef) {
      val kind = if (classDef.symbol.asClass.isTrait) "trait" else "class"
      if (!reportUnsupportedAwait(classDef, s"nested $kind")) {
        // do not allow local class definitions, because of SI-5467 (specific to case classes, though)
        if (classDef.symbol.asClass.isCaseClass)
          c.error(classDef.pos, s"Local case class ${classDef.name.decoded} illegal within `async` block")
      }
    }

    override def nestedModule(module: ModuleDef) {
      if (!reportUnsupportedAwait(module, "nested object")) {
        // local object definitions lead to spurious type errors (because of resetAllAttrs?)
        c.error(module.pos, s"Local object ${module.name.decoded} illegal within `async` block")
      }
    }

    override def nestedMethod(module: DefDef) {
      reportUnsupportedAwait(module, "nested method")
    }

    override def byNameArgument(arg: Tree) {
      reportUnsupportedAwait(arg, "by-name argument")
    }

    override def function(function: Function) {
      reportUnsupportedAwait(function, "nested function")
    }

    override def patMatFunction(tree: Match) {
      reportUnsupportedAwait(tree, "nested function")
    }

    override def traverse(tree: Tree) {
      def containsAwait(t: Tree) = t exists isAwait
      tree match {
        case Try(_, catches, _) if catches exists containsAwait =>
          reportUnsupportedAwait(tree, "catch")
          super.traverse(tree)
        case Return(_)                                          =>
          c.abort(tree.pos, "return is illegal within a async block")
        case ValDef(mods, _, _, _) if mods.hasFlag(Flag.LAZY)   =>
          c.abort(tree.pos, "lazy vals are illegal within an async block")
        case _                                                  =>
          super.traverse(tree)
      }
    }

    /**
     * @return true, if the tree contained an unsupported await.
     */
    private def reportUnsupportedAwait(tree: Tree, whyUnsupported: String): Boolean = {
      val badAwaits: List[RefTree] = tree collect {
        case rt: RefTree if isAwait(rt) => rt
      }
      badAwaits foreach {
        tree =>
          reportError(tree.pos, s"await must not be used under a $whyUnsupported.")
      }
      badAwaits.nonEmpty
    }

    private def reportError(pos: Position, msg: String) {
      hasUnsupportedAwaits = true
      if (!asyncBase.fallbackEnabled)
        c.error(pos, msg)
    }
  }

  private class AsyncDefinitionUseAnalyzer extends AsyncTraverser {
    private var chunkId = 0

    private def nextChunk() = chunkId += 1

    private var valDefChunkId = Map[Symbol, (ValDef, Int)]()

    val valDefsToLift      : mutable.Set[ValDef] = collection.mutable.Set()
    val nestedMethodsToLift: mutable.Set[DefDef] = collection.mutable.Set()

    override def nestedMethod(defDef: DefDef) {
      nestedMethodsToLift += defDef
      markReferencedVals(defDef)
    }

    override def function(function: Function) {
      markReferencedVals(function)
    }

    override def patMatFunction(tree: Match) {
      markReferencedVals(tree)
    }

    private def markReferencedVals(tree: Tree) {
      tree foreach {
        case rt: RefTree =>
          valDefChunkId.get(rt.symbol) match {
            case Some((vd, defChunkId)) =>
              valDefsToLift += vd // lift all vals referred to by nested functions.
            case _                      =>
          }
        case _           =>
      }
    }

    override def traverse(tree: Tree) = {
      tree match {
        case If(cond, thenp, elsep) if tree exists isAwait     =>
          traverseChunks(List(cond, thenp, elsep))
        case Match(selector, cases) if tree exists isAwait     =>
          traverseChunks(selector :: cases)
        case LabelDef(name, params, rhs) if rhs exists isAwait =>
          traverseChunks(rhs :: Nil)
        case Apply(fun, args) if isAwait(fun)                  =>
          super.traverse(tree)
          nextChunk()
        case vd: ValDef                                        =>
          super.traverse(tree)
          valDefChunkId += (vd.symbol -> (vd -> chunkId))
          val isPatternBinder = vd.name.toString.contains(name.bindSuffix)
          if (isAwait(vd.rhs) || isPatternBinder) valDefsToLift += vd
        case as: Assign                                        =>
          if (isAwait(as.rhs)) {
            assert(as.lhs.symbol != null, "internal error: null symbol for Assign tree:" + as + " " + as.lhs.symbol)

            // TODO test the orElse case, try to remove the restriction.
            val (vd, defBlockId) = valDefChunkId.getOrElse(as.lhs.symbol, c.abort(as.pos, s"await may only be assigned to a var/val defined in the async block. ${as.lhs} ${as.lhs.symbol}"))
            valDefsToLift += vd
          }
          super.traverse(tree)
        case rt: RefTree                                       =>
          valDefChunkId.get(rt.symbol) match {
            case Some((vd, defChunkId)) if defChunkId != chunkId =>
              valDefsToLift += vd
            case _                                               =>
          }
          super.traverse(tree)
        case _                                                 => super.traverse(tree)
      }
    }

    private def traverseChunks(trees: List[Tree]) {
      trees.foreach {
        t => traverse(t); nextChunk()
      }
    }
  }

}
