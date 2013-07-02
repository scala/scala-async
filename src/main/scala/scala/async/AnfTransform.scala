
/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async

import scala.tools.nsc.Global

private[async] trait AnfTransform {
  self: AsyncMacro =>

  import global._
  import reflect.internal.Flags._

  def anfTransform(tree: Tree): Block = {
    // Must prepend the () for issue #31.
    val block = callSiteTyper.typedPos(tree.pos)(Block(List(Literal(Constant(()))), tree)).setType(tree.tpe)

    new SelectiveAnfTransform().transform(block)
  }

  sealed abstract class AnfMode

  case object Anf extends AnfMode

  case object Linearizing extends AnfMode

  final class SelectiveAnfTransform extends MacroTypingTransformer {
    var mode: AnfMode = Anf

    def blockToList(tree: Tree): List[Tree] = tree match {
      case Block(stats, expr) => stats :+ expr
      case t                  => t :: Nil
    }

    def listToBlock(trees: List[Tree]): Block = trees match {
      case trees @ (init :+ last) =>
        val pos = trees.map(_.pos).reduceLeft(_ union _)
        Block(init, last).setType(last.tpe).setPos(pos)
    }

    override def transform(tree: Tree): Block = {
      def anfLinearize: Block = {
        val trees: List[Tree] = mode match {
          case Anf         => anf._transformToList(tree)
          case Linearizing => linearize._transformToList(tree)
        }
        listToBlock(trees)
      }
      tree match {
        case _: ValDef | _: DefDef | _: Function | _: ClassDef | _: TypeDef =>
          atOwner(tree.symbol)(anfLinearize)
        case _: ModuleDef                                                   =>
          atOwner(tree.symbol.moduleClass orElse tree.symbol)(anfLinearize)
        case _                                                              =>
          anfLinearize
      }
    }

    private object linearize {
      def transformToList(tree: Tree): List[Tree] = {
        mode = Linearizing; blockToList(transform(tree))
      }

      def transformToBlock(tree: Tree): Block = listToBlock(transformToList(tree))

      def _transformToList(tree: Tree): List[Tree] = trace(tree) {
        val stats :+ expr = anf.transformToList(tree)
        expr match {
          case Apply(fun, args) if isAwait(fun) =>
            val valDef = defineVal(name.await, expr, tree.pos)
            stats :+ valDef :+ gen.mkAttributedStableRef(valDef.symbol)

          case If(cond, thenp, elsep) =>
            // if type of if-else is Unit don't introduce assignment,
            // but add Unit value to bring it into form expected by async transform
            if (expr.tpe =:= definitions.UnitTpe) {
              stats :+ expr :+ localTyper.typedPos(expr.pos)(Literal(Constant(())))
            } else {
              val varDef = defineVar(name.ifRes, expr.tpe, tree.pos)
              def branchWithAssign(orig: Tree) = localTyper.typedPos(orig.pos)(
                orig match {
                  case Block(thenStats, thenExpr) => Block(thenStats, Assign(Ident(varDef.symbol), thenExpr))
                  case _                          => Assign(Ident(varDef.symbol), orig)
                }
              ).setType(orig.tpe)
              val ifWithAssign = treeCopy.If(tree, cond, branchWithAssign(thenp), branchWithAssign(elsep))
              stats :+ varDef :+ ifWithAssign :+ gen.mkAttributedStableRef(varDef.symbol)
            }

          case Match(scrut, cases) =>
            // if type of match is Unit don't introduce assignment,
            // but add Unit value to bring it into form expected by async transform
            if (expr.tpe =:= definitions.UnitTpe) {
              stats :+ expr :+ localTyper.typedPos(expr.pos)(Literal(Constant(())))
            }
            else {
              val varDef = defineVar(name.matchRes, expr.tpe, tree.pos)
              def typedAssign(lhs: Tree) =
                localTyper.typedPos(lhs.pos)(Assign(Ident(varDef.symbol), lhs))
              val casesWithAssign = cases map {
                case cd@CaseDef(pat, guard, body) =>
                  val newBody = body match {
                    case b@Block(caseStats, caseExpr) => treeCopy.Block(b, caseStats, typedAssign(caseExpr))
                    case _                            => typedAssign(body)
                  }
                  treeCopy.CaseDef(cd, pat, guard, newBody)
              }
              val matchWithAssign = treeCopy.Match(tree, scrut, casesWithAssign)
              require(matchWithAssign.tpe != null, matchWithAssign)
              stats :+ varDef :+ matchWithAssign :+ gen.mkAttributedStableRef(varDef.symbol)
            }
          case _                   =>
            stats :+ expr
        }
      }

      private def defineVar(prefix: String, tp: Type, pos: Position): ValDef = {
        val sym = currOwner.newTermSymbol(name.fresh(prefix), pos, MUTABLE | SYNTHETIC).setInfo(tp)
        ValDef(sym, gen.mkZero(tp)).setType(NoType).setPos(pos)
      }
    }

    private object trace {
      private var indent = -1

      def indentString = "  " * indent

      def apply[T](args: Any)(t: => T): T = {
        def prefix = mode.toString.toLowerCase
        indent += 1
        def oneLine(s: Any) = s.toString.replaceAll( """\n""", "\\\\n").take(127)
        try {
          AsyncUtils.trace(s"${indentString}$prefix(${oneLine(args)})")
          val result = t
          AsyncUtils.trace(s"${indentString}= ${oneLine(result)}")
          result
        } finally {
          indent -= 1
        }
      }
    }

    private def defineVal(prefix: String, lhs: Tree, pos: Position): ValDef = {
      val sym = currOwner.newTermSymbol(name.fresh(prefix), pos, SYNTHETIC).setInfo(lhs.tpe)
      changeOwner(lhs, currentOwner, sym)
      ValDef(sym, changeOwner(lhs, currentOwner, sym)).setType(NoType).setPos(pos)
    }

    private object anf {
      def transformToList(tree: Tree): List[Tree] = {
        mode = Anf; blockToList(transform(tree))
      }

      def _transformToList(tree: Tree): List[Tree] = trace(tree) {
        val containsAwait = tree exists isAwait
        if (!containsAwait) {
          List(tree)
        } else tree match {
          case Select(qual, sel) =>
            val stats :+ expr = linearize.transformToList(qual)
            stats :+ treeCopy.Select(tree, expr, sel)

          case treeInfo.Applied(fun, targs, argss) if argss.nonEmpty =>
            // we an assume that no await call appears in a by-name argument position,
            // this has already been checked.
            val funStats :+ simpleFun = linearize.transformToList(fun)
            def isAwaitRef(name: Name) = name.toString.startsWith(AnfTransform.this.name.await + "$")
            val (argStatss, argExprss): (List[List[List[Tree]]], List[List[Tree]]) =
              mapArgumentss[List[Tree]](fun, argss) {
                case Arg(expr, byName, _) if byName /*|| isPure(expr) TODO */ => (Nil, expr)
                case Arg(expr@Ident(name), _, _) if isAwaitRef(name)          => (Nil, expr) // TODO needed? // not typed, so it eludes the check in `isSafeToInline`
                case Arg(expr, _, argName)                                    =>
                  linearize.transformToList(expr) match {
                    case stats :+ expr1 =>
                      val valDef = defineVal(argName, expr1, expr1.pos)
                      require(valDef.tpe != null, valDef)
                      val stats1 = stats :+ valDef
                      //stats1.foreach(changeOwner(_, currentOwner, currentOwner.owner))
                      (stats1, gen.stabilize(gen.mkAttributedIdent(valDef.symbol)))
                  }
              }
            val applied = treeInfo.dissectApplied(tree)
            val core = if (targs.isEmpty) simpleFun else treeCopy.TypeApply(applied.callee, simpleFun, targs)
            val newApply = argExprss.foldLeft(core)(Apply(_, _)).setSymbol(tree.symbol)
            val typedNewApply = localTyper.typedPos(tree.pos)(newApply).setType(tree.tpe)
            funStats ++ argStatss.flatten.flatten :+ typedNewApply
          case Block(stats, expr)                                    =>
            (stats :+ expr).flatMap(linearize.transformToList)

          case ValDef(mods, name, tpt, rhs) =>
            if (rhs exists isAwait) {
              val stats :+ expr = atOwner(currOwner.owner)(linearize.transformToList(rhs))
              stats.foreach(changeOwner(_, currOwner, currOwner.owner))
              stats :+ treeCopy.ValDef(tree, mods, name, tpt, expr)
            } else List(tree)

          case Assign(lhs, rhs) =>
            val stats :+ expr = linearize.transformToList(rhs)
            stats :+ treeCopy.Assign(tree, lhs, expr)

          case If(cond, thenp, elsep) =>
            val condStats :+ condExpr = linearize.transformToList(cond)
            val thenBlock = linearize.transformToBlock(thenp)
            val elseBlock = linearize.transformToBlock(elsep)
            // Typechecking with `condExpr` as the condition fails if the condition
            // contains an await. `ifTree.setType(tree.tpe)` also fails; it seems
            // we rely on this call to `typeCheck` descending into the branches.
            // But, we can get away with typechecking a throwaway `If` tree with the
            // original scrutinee and the new branches, and setting that type on
            // the real `If` tree.
            val iff = treeCopy.If(tree, condExpr, thenBlock, elseBlock)
            condStats :+ iff

          case Match(scrut, cases) =>
            val scrutStats :+ scrutExpr = linearize.transformToList(scrut)
            val caseDefs = cases map {
              case CaseDef(pat, guard, body) =>
                // extract local variables for all names bound in `pat`, and rewrite `body`
                // to refer to these.
                // TODO we can move this into ExprBuilder once we get rid of `AsyncDefinitionUseAnalyzer`.
                val block = linearize.transformToBlock(body)
                val (valDefs, mappings) = (pat collect {
                  case b@Bind(name, _) =>
                    val vd = defineVal(name.toTermName + AnfTransform.this.name.bindSuffix, gen.mkAttributedStableRef(b.symbol), b.pos)
                    (vd, (b.symbol, vd.symbol))
                }).unzip
                val (from, to) = mappings.unzip
                val b@Block(stats1, expr1) = block.substituteSymbols(from, to).asInstanceOf[Block]
                val newBlock = treeCopy.Block(b, valDefs ++ stats1, expr1)
                treeCopy.CaseDef(tree, pat, guard, newBlock)
            }
            // Refer to comments the translation of `If` above.
            val typedMatch = treeCopy.Match(tree, scrutExpr, caseDefs)
            scrutStats :+ typedMatch

          case LabelDef(name, params, rhs) =>
            List(LabelDef(name, params, Block(linearize.transformToList(rhs), Literal(Constant(())))).setSymbol(tree.symbol))

          case TypeApply(fun, targs) =>
            val funStats :+ simpleFun = linearize.transformToList(fun)
            funStats :+ treeCopy.TypeApply(tree, simpleFun, targs)

          case _ =>
            List(tree)
        }
      }
    }
  }
}
