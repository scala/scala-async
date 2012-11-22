
package scala.async

import scala.reflect.macros.Context

class AnfTransform[C <: Context](override val c: C) extends TransformUtils(c) {

  import c.universe._

  object inline {
    def transformToList(tree: Tree): List[Tree] = {
      val stats :+ expr = anf.transformToList(tree)
      expr match {
        case Apply(fun, args) if isAwait(fun) =>
          val valDef = defineVal("await", expr)
          stats :+ valDef :+ Ident(valDef.name)

        case If(cond, thenp, elsep) =>
          // if type of if-else is Unit don't introduce assignment,
          // but add Unit value to bring it into form expected by async transform
          if (expr.tpe =:= definitions.UnitTpe) {
            stats :+ expr :+ Literal(Constant(()))
          } else {
            val varDef = defineVar("ifres", expr.tpe)
            def branchWithAssign(orig: Tree) = orig match {
              case Block(thenStats, thenExpr) => Block(thenStats, Assign(Ident(varDef.name), thenExpr))
              case _ => Assign(Ident(varDef.name), orig)
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
            val varDef = defineVar("matchres", expr.tpe)
            val casesWithAssign = cases map {
              case CaseDef(pat, guard, Block(caseStats, caseExpr)) => CaseDef(pat, guard, Block(caseStats, Assign(Ident(varDef.name), caseExpr)))
              case CaseDef(pat, guard, body) => CaseDef(pat, guard, Assign(Ident(varDef.name), body))
            }
            val matchWithAssign = Match(scrut, casesWithAssign)
            stats :+ varDef :+ matchWithAssign :+ Ident(varDef.name)
          }
        case _ =>
          stats :+ expr
      }
    }

    def transformToList(trees: List[Tree]): List[Tree] = trees match {
      case fst :: rest => transformToList(fst) ++ transformToList(rest)
      case Nil => Nil
    }

    def transformToBlock(tree: Tree): Block = transformToList(tree) match {
      case stats :+ expr => Block(stats, expr)
    }

    def liftedName(prefix: String) = c.fresh(prefix + "$")

    private def defineVar(prefix: String, tp: Type): ValDef =
      ValDef(Modifiers(Flag.MUTABLE), liftedName(prefix), TypeTree(tp), defaultValue(tp))

    private def defineVal(prefix: String, lhs: Tree): ValDef =
      ValDef(NoMods, liftedName(prefix), TypeTree(), lhs)
  }

  object anf {
    def transformToList(tree: Tree): List[Tree] = {
      def containsAwait = tree exists isAwait
      tree match {
        case Select(qual, sel) if containsAwait =>
          val stats :+ expr = inline.transformToList(qual)
          stats :+ Select(expr, sel).setSymbol(tree.symbol)

        case Apply(fun, args) if containsAwait =>
          // we an assume that no await call appears in a by-name argument position,
          // this has already been checked.

          val funStats :+ simpleFun = inline.transformToList(fun)
          val argLists = args map inline.transformToList
          val allArgStats = argLists flatMap (_.init)
          val simpleArgs = argLists map (_.last)
          funStats ++ allArgStats :+ Apply(simpleFun, simpleArgs).setSymbol(tree.symbol)

        case Block(stats, expr) => // TODO figure out why adding a guard `if containsAwait` breaks LocalClasses0Spec.
          inline.transformToList(stats :+ expr)

        case ValDef(mods, name, tpt, rhs) if containsAwait =>
          if (rhs exists isAwait) {
            val stats :+ expr = inline.transformToList(rhs)
            stats :+ ValDef(mods, name, tpt, expr).setSymbol(tree.symbol)
          } else List(tree)
        case Assign(lhs, rhs) if containsAwait =>
          val stats :+ expr = inline.transformToList(rhs)
          stats :+ Assign(lhs, expr)

        case If(cond, thenp, elsep) if containsAwait =>
          val stats :+ expr = inline.transformToList(cond)
          val thenBlock = inline.transformToBlock(thenp)
          val elseBlock = inline.transformToBlock(elsep)
          stats :+
            c.typeCheck(If(expr, thenBlock, elseBlock))

        case Match(scrut, cases) if containsAwait =>
          val scrutStats :+ scrutExpr = inline.transformToList(scrut)
          val caseDefs = cases map {
            case CaseDef(pat, guard, body) =>
              val block = inline.transformToBlock(body)
              CaseDef(pat, guard, block)
          }
          scrutStats :+ c.typeCheck(Match(scrutExpr, caseDefs))

        case TypeApply(fun, targs) if containsAwait =>
          val funStats :+ simpleFun = inline.transformToList(fun)
          funStats :+ TypeApply(simpleFun, targs).setSymbol(tree.symbol)

        case _ =>
          List(tree)
      }
    }
  }

}
