/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async.internal

import scala.collection.mutable.ListBuffer
import scala.reflect.macros.Context
import scala.collection.mutable

trait AsyncAnalysis {
  self: AsyncMacro =>

  import c.universe._

  /**
   * Analyze the contents of an `async` block in order to:
   * - Report unsupported `await` calls under nested templates, functions, by-name arguments.
   *
   * Must be called on the original tree, not on the ANF transformed tree.
   */
  def reportUnsupportedAwaits(tree: Tree): Unit = {
    val analyzer = new UnsupportedAwaitAnalyzer
    analyzer.traverse(tree)
    // analyzer.hasUnsupportedAwaits // XB: not used?!
  }

  private class UnsupportedAwaitAnalyzer extends AsyncTraverser {
    var hasUnsupportedAwaits = false

    override def nestedClass(classDef: ClassDef) {
      val kind = if (classDef.symbol.asClass.isTrait) "trait" else "class"
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
      tree match {
        case Try(_, _, _) if containsAwait(tree)              =>
          reportUnsupportedAwait(tree, "try/catch")
          super.traverse(tree)
        case Return(_)                                        =>
          c.abort(tree.pos, "return is illegal within a async block")
        case DefDef(mods, _, _, _, _, _) if mods.hasFlag(Flag.LAZY) && containsAwait(tree) =>
          reportUnsupportedAwait(tree, "lazy val initializer")
        case ValDef(mods, _, _, _) if mods.hasFlag(Flag.LAZY) && containsAwait(tree) =>
          reportUnsupportedAwait(tree, "lazy val initializer")
        case CaseDef(_, guard, _) if guard exists isAwait     =>
          // TODO lift this restriction
          reportUnsupportedAwait(tree, "pattern guard")
        case _                                                =>
          super.traverse(tree)
      }
    }

    /**
     * @return true, if the tree contained an unsupported await.
     */
    private def reportUnsupportedAwait(tree: Tree, whyUnsupported: String): Boolean = {
      val badAwaits = ListBuffer[Tree]()
      object traverser extends Traverser {
        override def traverse(tree: Tree): Unit = {
          if (!isAsync(tree))
            super.traverse(tree)
          tree match {
            case rt: RefTree if isAwait(rt) =>
              badAwaits += rt
            case _ =>
          }
        }
      }
      traverser(tree)
      badAwaits foreach {
        tree =>
          reportError(tree.pos, s"await must not be used under a $whyUnsupported.")
      }
      badAwaits.nonEmpty
    }

    private def reportError(pos: Position, msg: String) {
      hasUnsupportedAwaits = true
      c.abort(pos, msg)
    }
  }
}
