
/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async

import scala.reflect.macros.Context

private[async] final case class AnfTransform[C <: Context](c: C) {

  import c.universe._

  val utils = TransformUtils[c.type](c)

  import utils._

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
    val repeatedNames: Set[Symbol] = {
      class DuplicateNameTraverser extends AsyncTraverser {
        val result = collection.mutable.Buffer[Symbol]()

        override def traverse(tree: Tree) {
          tree match {
            case dt: DefTree => result += dt.symbol
            case _           => super.traverse(tree)
          }
        }
      }
      val dupNameTraverser = new DuplicateNameTraverser
      dupNameTraverser.traverse(tree)
      dupNameTraverser.result.groupBy(x => x.name).filter(_._2.size > 1).values.flatten.toSet[Symbol]
    }

    /** Stepping outside of the public Macro API to call [[scala.reflect.internal.Symbols.Symbol.name_=]] */
    val symtab = c.universe.asInstanceOf[reflect.internal.SymbolTable]

    val renamed = collection.mutable.Set[Symbol]()

    override def transform(tree: Tree): Tree = {
      tree match {
        case defTree: DefTree if repeatedNames(defTree.symbol) =>
          val trans = super.transform(defTree)
          val origName = defTree.symbol.name
          val sym = defTree.symbol.asInstanceOf[symtab.Symbol]
          val fresh = name.fresh(sym.name.toString)
          sym.name = defTree.symbol.name match {
            case _: TermName => symtab.newTermName(fresh)
            case _: TypeName => symtab.newTypeName(fresh)
          }
          renamed += trans.symbol
          val newName = trans.symbol.name
          trans match {
            case ValDef(mods, name, tpt, rhs)                    =>
              treeCopy.ValDef(trans, mods, newName, tpt, rhs)
            case Bind(name, body)                                =>
              treeCopy.Bind(trans, newName, body)
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
        case Ident(name)                                       =>
          if (renamed(tree.symbol)) treeCopy.Ident(tree, tree.symbol.name)
          else tree
        case Select(fun, name)                                 =>
          if (renamed(tree.symbol)) {
            treeCopy.Select(tree, transform(fun), tree.symbol.name)
          } else super.transform(tree)
        case _                                                 => super.transform(tree)
      }
    }
  }

  private object trace {
    private var indent = -1

    def indentString = "  " * indent

    def apply[T](prefix: String, args: Any)(t: => T): T = {
      indent += 1
      def oneLine(s: Any) = s.toString.replaceAll( """\n""", "\\\\n").take(127)
      try {
        AsyncUtils.trace(s"${
          indentString
        }$prefix(${oneLine(args)})")
        val result = t
        AsyncUtils.trace(s"${indentString}= ${oneLine(result)}")
        result
      } finally {
        indent -= 1
      }
    }
  }

  private object inline {
    def transformToList(tree: Tree): List[Tree] = trace("inline", tree) {
      def branchWithAssign(orig: Tree, varDef: ValDef) = orig match {
        case Block(stats, expr) => Block(stats, Assign(Ident(varDef.name), expr))
        case _                  => Assign(Ident(varDef.name), orig)
      }

      def casesWithAssign(cases: List[CaseDef], varDef: ValDef) = cases map {
        case cd @ CaseDef(pat, guard, orig) =>
          attachCopy(cd)(CaseDef(pat, guard, branchWithAssign(orig, varDef)))
      }

      val stats :+ expr = anf.transformToList(tree)
      expr match {
        // if type of if-else/try/match is Unit don't introduce assignment,
        // but add Unit value to bring it into form expected by async transform
        case If(_, _, _) | Try(_, _, _) | Match(_, _) if expr.tpe =:= definitions.UnitTpe =>
          stats :+ expr :+ Literal(Constant(()))

        case Apply(fun, args) if isAwait(fun) =>
          val valDef = defineVal(name.await, expr, tree.pos)
          stats :+ valDef :+ Ident(valDef.name)

        case If(cond, thenp, elsep) =>
          val varDef = defineVar(name.ifRes, expr.tpe, tree.pos)
          val ifWithAssign = If(cond, branchWithAssign(thenp, varDef), branchWithAssign(elsep, varDef))
          stats :+ varDef :+ ifWithAssign :+ Ident(varDef.name)

        case Try(body, catches, finalizer) =>
          val varDef = defineVar(name.tryRes, expr.tpe, tree.pos)
          val tryWithAssign = Try(branchWithAssign(body, varDef), casesWithAssign(catches, varDef), finalizer)
          stats :+ varDef :+ tryWithAssign :+ Ident(varDef.name)

        case Match(scrut, cases) =>
          val varDef = defineVar(name.matchRes, expr.tpe, tree.pos)
          val matchWithAssign = attachCopy(tree)(Match(scrut, casesWithAssign(cases, varDef)))
          stats :+ varDef :+ matchWithAssign :+ Ident(varDef.name)

        case _                   =>
          stats :+ expr
      }
    }

    def transformToList(trees: List[Tree]): List[Tree] = trees flatMap transformToList

    def transformToBlock(tree: Tree): Block = transformToList(tree) match {
      case stats :+ expr => Block(stats, expr)
    }

    private def defineVar(prefix: String, tp: Type, pos: Position): ValDef = {
      val vd = ValDef(Modifiers(Flag.MUTABLE), name.fresh(prefix), TypeTree(tp), defaultValue(tp))
      vd.setPos(pos)
      vd
    }
  }

  private def defineVal(prefix: String, lhs: Tree, pos: Position): ValDef = {
    val vd = ValDef(NoMods, name.fresh(prefix), TypeTree(), lhs)
    vd.setPos(pos)
    vd
  }

  private object anf {

    private[AnfTransform] def transformToList(tree: Tree): List[Tree] = trace("anf", tree) {
      def containsAwait = tree exists isAwait
      tree match {
        case Select(qual, sel) if containsAwait =>
          val stats :+ expr = inline.transformToList(qual)
          stats :+ attachCopy(tree)(Select(expr, sel).setSymbol(tree.symbol))

        case Apply(fun, args) if containsAwait   =>
          checkForAwaitInNonPrimaryParamSection(fun, args)

          // we an assume that no await call appears in a by-name argument position,
          // this has already been checked.
          val isByName: (Int) => Boolean = utils.isByName(fun)
          val funStats :+ simpleFun = inline.transformToList(fun)
          def isAwaitRef(name: Name) = name.toString.startsWith(utils.name.await + "$")
          val (argStats, argExprs): (List[List[Tree]], List[Tree]) =
            mapArguments[List[Tree]](args) {
              case (arg, i) if isByName(i) || isSafeToInline(arg) => (Nil, arg)
              case (arg@Ident(name), _) if isAwaitRef(name)       => (Nil, arg) // not typed, so it eludes the check in `isSafeToInline`
              case (arg, i)                                       =>
                inline.transformToList(arg) match {
                  case stats :+ expr =>
                    val valDef = defineVal(name.arg(i), expr, arg.pos)
                    (stats :+ valDef, Ident(valDef.name))
                }
            }
          funStats ++ argStats.flatten :+ attachCopy(tree)(Apply(simpleFun, argExprs).setSymbol(tree.symbol))
        case Block(stats, expr) if containsAwait =>
          inline.transformToList(stats :+ expr)

        case ValDef(mods, name, tpt, rhs) if containsAwait =>
          if (rhs exists isAwait) {
            val stats :+ expr = inline.transformToList(rhs)
            stats :+ attachCopy(tree)(ValDef(mods, name, tpt, expr).setSymbol(tree.symbol))
          } else List(tree)

        case Assign(lhs, rhs) if containsAwait =>
          val stats :+ expr = inline.transformToList(rhs)
          stats :+ attachCopy(tree)(Assign(lhs, expr))

        case Try(body, catches, finalizer) if containsAwait =>
          val stats :+ expr = inline.transformToList(body)
          val tryType = c.typeCheck(Try(Block(stats, expr), catches, finalizer)).tpe
          List(attachCopy(tree)(Try(Block(stats, expr), catches, finalizer)).setType(tryType))

        case If(cond, thenp, elsep) if containsAwait =>
          val condStats :+ condExpr = inline.transformToList(cond)
          val thenBlock = inline.transformToBlock(thenp)
          val elseBlock = inline.transformToBlock(elsep)
          // Typechecking with `condExpr` as the condition fails if the condition
          // contains an await. `ifTree.setType(tree.tpe)` also fails; it seems
          // we rely on this call to `typeCheck` descending into the branches.
          // But, we can get away with typechecking a throwaway `If` tree with the
          // original scrutinee and the new branches, and setting that type on
          // the real `If` tree.
          val ifType = c.typeCheck(If(cond, thenBlock, elseBlock)).tpe
          condStats :+
            attachCopy(tree)(If(condExpr, thenBlock, elseBlock)).setType(ifType)

        case Match(scrut, cases) if containsAwait =>
          val scrutStats :+ scrutExpr = inline.transformToList(scrut)
          val caseDefs = cases map {
            case CaseDef(pat, guard, body) =>
              // extract local variables for all names bound in `pat`, and rewrite `body`
              // to refer to these.
              // TODO we can move this into ExprBuilder once we get rid of `AsyncDefinitionUseAnalyzer`.
              val block = inline.transformToBlock(body)
              val (valDefs, mappings) = (pat collect {
                case b@Bind(name, _) =>
                  val newName = newTermName(utils.name.fresh(name.toTermName + utils.name.bindSuffix))
                  val vd = ValDef(NoMods, newName, TypeTree(), Ident(b.symbol))
                  (vd, (b.symbol, newName))
              }).unzip
              val Block(stats1, expr1) = utils.substituteNames(block, mappings.toMap).asInstanceOf[Block]
              attachCopy(tree)(CaseDef(pat, guard, Block(valDefs ++ stats1, expr1)))
          }
          // Refer to comments the translation of `If` above.
          val matchType = c.typeCheck(Match(scrut, caseDefs)).tpe
          val typedMatch = attachCopy(tree)(Match(scrutExpr, caseDefs)).setType(tree.tpe)
          scrutStats :+ typedMatch

        case LabelDef(name, params, rhs) if containsAwait =>
          List(LabelDef(name, params, Block(inline.transformToList(rhs), Literal(Constant(())))).setSymbol(tree.symbol))

        case TypeApply(fun, targs) if containsAwait =>
          val funStats :+ simpleFun = inline.transformToList(fun)
          funStats :+ attachCopy(tree)(TypeApply(simpleFun, targs).setSymbol(tree.symbol))

        case _ =>
          List(tree)
      }
    }
  }

  def checkForAwaitInNonPrimaryParamSection(fun: Tree, args: List[Tree]) {
    // TODO treat the Apply(Apply(.., argsN), ...), args0) holistically, and rewrite
    // *all* argument lists in the correct order to preserve semantics.
    fun match {
      case Apply(fun1, _) =>
        fun1.tpe match {
          case MethodType(_, resultType: MethodType) if resultType =:= fun.tpe =>
            c.error(fun.pos, "implementation restriction: await may only be used in the first parameter list.")
          case _                                                               =>
        }
      case _              =>
    }

  }
}
