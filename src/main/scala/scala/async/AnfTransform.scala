
package scala.async

import scala.reflect.macros.Context

private[async] final case class AnfTransform[C <: Context](override val c: C) extends TransformUtils(c) {

  import c.universe._

  def apply(tree: Tree): List[Tree] = {
    val unique = uniqueNames(tree)
    // Must prepend the () for issue #31.
    anf.transformToList(Block(List(c.literalUnit.tree), unique))
  }

  private def uniqueNames(tree: Tree): Tree = {
    new UniqueNames(tree).transform(tree)
  }

  /** Assigns unique names to all definitions in a tree, and adjusts references to use the new name.
    * Only modifies names that appear more than once in the tree.
    *
    * This step is needed to allow us to safely merge blocks during the `inline` transform below.
    */
  private final class UniqueNames(tree: Tree) extends Transformer {
    val repeatedNames: Set[Name] = tree.collect {
      case dt: DefTree => dt.symbol.name
    }.groupBy(x => x).filter(_._2.size > 1).keySet

    /** Stepping outside of the public Macro API to call [[scala.reflect.internal.Symbols.Symbol.name_=]] */
    val symtab = c.universe.asInstanceOf[reflect.internal.SymbolTable]

    val renamed = collection.mutable.Set[Symbol]()

    override def transform(tree: Tree): Tree = {
      tree match {
        case defTree: DefTree if repeatedNames(defTree.symbol.name) =>
          val trans = super.transform(defTree)
          val origName = defTree.symbol.name
          val sym = defTree.symbol.asInstanceOf[symtab.Symbol]
          val fresh = c.fresh("" + sym.name + "$")
          sym.name = defTree.symbol.name match {
            case _: TermName => symtab.newTermName(fresh)
            case _: TypeName => symtab.newTypeName(fresh)
          }
          renamed += trans.symbol
          val newName = trans.symbol.name
          trans match {
            case ValDef(mods, name, tpt, rhs)                    =>
              treeCopy.ValDef(trans, mods, newName, tpt, rhs)
            case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
              treeCopy.DefDef(trans, mods, newName, tparams, vparamss, tpt, rhs)
            case TypeDef(mods, name, tparams, rhs)               =>
              treeCopy.TypeDef(tree, mods, newName, tparams, transform(rhs))
            // If we were to allow local classes / objects, we would need to rename here.
            case ClassDef(mods, name, tparams, impl) =>
              treeCopy.ClassDef(tree, mods, newName, tparams, transform(impl).asInstanceOf[Template])
            case ModuleDef(mods, name, impl)         =>
              treeCopy.ModuleDef(tree, mods, newName, transform(impl).asInstanceOf[Template])
            case x                                   => super.transform(x)
          }
        case Ident(name)                                            =>
          if (renamed(tree.symbol)) treeCopy.Ident(tree, tree.symbol.name)
          else tree
        case Select(fun, name)                                      =>
          if (renamed(tree.symbol)) {
            treeCopy.Select(tree, transform(fun), tree.symbol.name)
          } else super.transform(tree)
        case _                                                      => super.transform(tree)
      }
    }
  }

  private object trace {
    private var indent = -1
    def indentString = "  " * indent
    def apply[T](prefix: String, args: Any)(t: => T): T = {
      indent += 1
      try {
        AsyncUtils.trace(s"${indentString}$prefix($args)")
        val result = t
        AsyncUtils.trace(s"${indentString}= $result")
        result
      } finally {
        indent -= 1
      }
    }
  }

  private object inline {
    def transformToList(tree: Tree): List[Tree] = trace("inline", tree) {
      val stats :+ expr = anf.transformToList(tree)
      expr match {
        case Apply(fun, args) if isAwait(fun) =>
          val valDef = defineVal("await", expr, tree.pos)
          stats :+ valDef :+ Ident(valDef.name)

        case If(cond, thenp, elsep) =>
          // if type of if-else is Unit don't introduce assignment,
          // but add Unit value to bring it into form expected by async transform
          if (expr.tpe =:= definitions.UnitTpe) {
            stats :+ expr :+ Literal(Constant(()))
          } else {
            val varDef = defineVar("ifres", expr.tpe, tree.pos)
            def branchWithAssign(orig: Tree) = orig match {
              case Block(thenStats, thenExpr) => Block(thenStats, Assign(Ident(varDef.name), thenExpr))
              case _                          => Assign(Ident(varDef.name), orig)
            }
            val ifWithAssign = If(cond, branchWithAssign(thenp), branchWithAssign(elsep))
            stats :+ varDef :+ ifWithAssign :+ Ident(varDef.name)
          }

        case Match(scrut, cases) =>
          // if type of match is Unit don't introduce assignment,
          // but add Unit value to bring it into form expected by async transform
          if (expr.tpe =:= definitions.UnitTpe) {
            stats :+ expr :+ Literal(Constant(()))
          }
          else {
            val varDef = defineVar("matchres", expr.tpe, tree.pos)
            val casesWithAssign = cases map {
              case cd@CaseDef(pat, guard, Block(caseStats, caseExpr)) =>
                attachCopy.CaseDef(cd)(pat, guard, Block(caseStats, Assign(Ident(varDef.name), caseExpr)))
              case cd@CaseDef(pat, guard, body)                       =>
                attachCopy.CaseDef(cd)(pat, guard, Assign(Ident(varDef.name), body))
            }
            val matchWithAssign = attachCopy.Match(tree)(scrut, casesWithAssign)
            stats :+ varDef :+ matchWithAssign :+ Ident(varDef.name)
          }
        case _                   =>
          stats :+ expr
      }
    }

    def transformToList(trees: List[Tree]): List[Tree] = trees match {
      case fst :: rest => transformToList(fst) ++ transformToList(rest)
      case Nil         => Nil
    }

    def transformToBlock(tree: Tree): Block = transformToList(tree) match {
      case stats :+ expr => Block(stats, expr)
    }

    def liftedName(prefix: String) = c.fresh(prefix + "$")

    private def defineVar(prefix: String, tp: Type, pos: Position): ValDef = {
      val vd = ValDef(Modifiers(Flag.MUTABLE), liftedName(prefix), TypeTree(tp), defaultValue(tp))
      vd.setPos(pos)
      vd
    }

    private def defineVal(prefix: String, lhs: Tree, pos: Position): ValDef = {
      val vd = ValDef(NoMods, liftedName(prefix), TypeTree(), lhs)
      vd.setPos(pos)
      vd
    }
  }

  private object anf {

    private[AnfTransform] def transformToList(tree: Tree): List[Tree] = trace("anf", tree) {
      def containsAwait = tree exists isAwait
      tree match {
        case Select(qual, sel) if containsAwait =>
          val stats :+ expr = inline.transformToList(qual)
          stats :+ attachCopy.Select(tree)(expr, sel).setSymbol(tree.symbol)

        case Apply(fun, args) if containsAwait =>
          // we an assume that no await call appears in a by-name argument position,
          // this has already been checked.

          val funStats :+ simpleFun = inline.transformToList(fun)
          val argLists = args map inline.transformToList
          val allArgStats = argLists flatMap (_.init)
          val simpleArgs = argLists map (_.last)
          funStats ++ allArgStats :+ attachCopy.Apply(tree)(simpleFun, simpleArgs).setSymbol(tree.symbol)

        case Block(stats, expr) if containsAwait =>
          inline.transformToList(stats :+ expr)

        case ValDef(mods, name, tpt, rhs) if containsAwait =>
          if (rhs exists isAwait) {
            val stats :+ expr = inline.transformToList(rhs)
            stats :+ attachCopy.ValDef(tree)(mods, name, tpt, expr).setSymbol(tree.symbol)
          } else List(tree)

        case Assign(lhs, rhs) if containsAwait =>
          val stats :+ expr = inline.transformToList(rhs)
          stats :+ attachCopy.Assign(tree)(lhs, expr)

        case If(cond, thenp, elsep) if containsAwait =>
          val stats :+ expr = inline.transformToList(cond)
          val thenBlock = inline.transformToBlock(thenp)
          val elseBlock = inline.transformToBlock(elsep)
          stats :+
            c.typeCheck(attachCopy.If(tree)(expr, thenBlock, elseBlock))

        case Match(scrut, cases) if containsAwait =>
          val scrutStats :+ scrutExpr = inline.transformToList(scrut)
          val caseDefs = cases map {
            case CaseDef(pat, guard, body) =>
              val block = inline.transformToBlock(body)
              attachCopy.CaseDef(tree)(pat, guard, block)
          }
          scrutStats :+ c.typeCheck(attachCopy.Match(tree)(scrutExpr, caseDefs))

        case LabelDef(name, params, rhs) if containsAwait =>
          List(LabelDef(name, params, Block(inline.transformToList(rhs), Literal(Constant(())))).setSymbol(tree.symbol))

        case TypeApply(fun, targs) if containsAwait =>
          val funStats :+ simpleFun = inline.transformToList(fun)
          funStats :+ attachCopy.TypeApply(tree)(simpleFun, targs).setSymbol(tree.symbol)

        case _ =>
          List(tree)
      }
    }
  }
}
