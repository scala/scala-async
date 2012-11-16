package scala.async

import scala.reflect.macros.Context

class AnfTransform[C <: Context](override val c: C) extends TransformUtils(c) {
  import c.universe._
  import AsyncUtils._

  object inline {
    def transformToList(tree: Tree): List[Tree] = {
      val stats :+ expr = anf.transformToList(tree)
      expr match {

        case Apply(fun, args) if fun.toString.startsWith("scala.async.Async.await") =>
          val liftedName = c.fresh("await$")
          stats :+ ValDef(NoMods, liftedName, TypeTree(), expr) :+ Ident(liftedName)

        case If(cond, thenp, elsep) =>
          // if type of if-else is Unit don't introduce assignment,
          // but add Unit value to bring it into form expected by async transform
          if (expr.tpe =:= definitions.UnitTpe) {
            stats :+ expr :+ Literal(Constant(()))
          }
          else {
            val liftedName = c.fresh("ifres$")
            val varDef =
              ValDef(Modifiers(Flag.MUTABLE), liftedName, TypeTree(expr.tpe), defaultValue(expr.tpe))
            val thenWithAssign = thenp match {
              case Block(thenStats, thenExpr) => Block(thenStats, Assign(Ident(liftedName), thenExpr))
              case _ => Assign(Ident(liftedName), thenp)
            }
            val elseWithAssign = elsep match {
              case Block(elseStats, elseExpr) => Block(elseStats, Assign(Ident(liftedName), elseExpr))
              case _ => Assign(Ident(liftedName), elsep)
            }
            val ifWithAssign =
              If(cond, thenWithAssign, elseWithAssign)
            stats :+ varDef :+ ifWithAssign :+ Ident(liftedName)
          }
        case _ =>
          stats :+ expr
      }
    }

    def transformToList(trees: List[Tree]): List[Tree] = trees match {
      case fst :: rest => transformToList(fst) ++ transformToList(rest)
      case Nil         => Nil
    }
  }

  object anf {
    def transformToList(tree: Tree): List[Tree] = tree match {
      case Select(qual, sel) =>
        val stats :+ expr = inline.transformToList(qual)
        stats :+ Select(expr, sel)

      case Apply(fun, args) =>
        val funStats :+ simpleFun = inline.transformToList(fun)
        val argLists = args map inline.transformToList
        val allArgStats = argLists flatMap (_.init)
        val simpleArgs = argLists map (_.last)
        funStats ++ allArgStats :+ Apply(simpleFun, simpleArgs)

      case Block(stats, expr) =>
        inline.transformToList(stats) ++ inline.transformToList(expr)

      case ValDef(mods, name, tpt, rhs) =>
        val stats :+ expr = inline.transformToList(rhs)
        stats :+ ValDef(mods, name, tpt, expr)

      case Assign(name, rhs) =>
        val stats :+ expr = inline.transformToList(rhs)
        stats :+ Assign(name, expr)

      case If(cond, thenp, elsep) =>
        val stats :+ expr = inline.transformToList(cond)
        val thenStats :+ thenExpr = inline.transformToList(thenp)
        val elseStats :+ elseExpr = inline.transformToList(elsep)
        stats :+
          c.typeCheck(If(expr, Block(thenStats, thenExpr), Block(elseStats, elseExpr)),
                      lub(List(thenp.tpe, elsep.tpe)))

      //TODO
      case Literal(_) | Ident(_) | This(_) | Match(_, _) | New(_) | Function(_, _) => List(tree)

      case TypeApply(fun, targs) =>
        val funStats :+ simpleFun = inline.transformToList(fun)
        funStats :+ TypeApply(simpleFun, targs)

      //TODO
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) => List(tree)

      case ClassDef(mods, name, tparams, impl) => List(tree)

      case ModuleDef(mods, name, impl) => List(tree)

      case _ =>
        c.error(tree.pos, "Internal error while compiling `async` block")
        ???
    }
  }
}
