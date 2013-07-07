/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async.internal

import scala.reflect.macros.Context
import scala.collection.mutable

trait AsyncAnalysis {
  self: AsyncMacro =>

  import global._

  /**
   * Analyze the contents of an `async` block in order to:
   * - Report unsupported `await` calls under nested templates, functions, by-name arguments.
   *
   * Must be called on the original tree, not on the ANF transformed tree.
   */
  def reportUnsupportedAwaits(tree: Tree, report: Boolean): Boolean = {
    val analyzer = new UnsupportedAwaitAnalyzer(report)
    analyzer.traverse(tree)
    analyzer.hasUnsupportedAwaits
  }

  private class UnsupportedAwaitAnalyzer(report: Boolean) extends AsyncTraverser {
    var hasUnsupportedAwaits = false

    override def nestedClass(classDef: ClassDef) {
      val kind = if (classDef.symbol.isTrait) "trait" else "class"
      reportUnsupportedAwait(classDef, s"nested ${kind}")
    }

    override def nestedModule(module: ModuleDef) {
      reportUnsupportedAwait(module, "nested object")
    }

    override def nestedMethod(defDef: DefDef) {
      reportUnsupportedAwait(defDef, "nested method")
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
      def containsAwait = tree exists isAwait
      tree match {
        case Try(_, _, _) if containsAwait                    =>
          reportUnsupportedAwait(tree, "try/catch")
          super.traverse(tree)
        case Return(_)                                        =>
          abort(tree.pos, "return is illegal within a async block")
        case ValDef(mods, _, _, _) if mods.hasFlag(Flag.LAZY) =>
          // TODO lift this restriction
          abort(tree.pos, "lazy vals are illegal within an async block")
        case _                                                =>
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
      if (report)
        abort(pos, msg)
    }
  }
}
