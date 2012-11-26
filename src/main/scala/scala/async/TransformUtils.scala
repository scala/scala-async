/**
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import scala.reflect.macros.Context
import reflect.ClassTag

/**
 * Utilities used in both `ExprBuilder` and `AnfTransform`.
 */
class TransformUtils[C <: Context](val c: C) {

  import c.universe._

  protected def defaultValue(tpe: Type): Literal = {
    val defaultValue: Any =
      if (tpe <:< definitions.BooleanTpe) false
      else if (definitions.ScalaNumericValueClasses.exists(tpe <:< _.toType)) 0
      else if (tpe <:< definitions.AnyValTpe) 0
      else null
    Literal(Constant(defaultValue))
  }

  protected def isAwait(fun: Tree) =
    fun.symbol == defn.Async_await

  /** Descends into the regions of the tree that are subject to the
    * translation to a state machine by `async`. When a nested template,
    * function, or by-name argument is encountered, the descend stops,
    * and `nestedClass` etc are invoked.
    */
  trait AsyncTraverser extends Traverser {
    def nestedClass(classDef: ClassDef) {
    }

    def nestedModule(module: ModuleDef) {
    }

    def byNameArgument(arg: Tree) {
    }

    def function(function: Function) {
    }

    override def traverse(tree: Tree) {
      tree match {
        case cd: ClassDef     => nestedClass(cd)
        case md: ModuleDef    => nestedModule(md)
        case fun: Function    => function(fun)
        case Apply(fun, args) =>
          val isInByName = isByName(fun)
          for ((arg, index) <- args.zipWithIndex) {
            if (!isInByName(index)) traverse(arg)
            else byNameArgument(arg)
          }
          traverse(fun)
        case _                => super.traverse(tree)
      }
    }
  }

  private lazy val Boolean_ShortCircuits: Set[Symbol] = {
    import definitions.BooleanClass
    def BooleanTermMember(name: String) = BooleanClass.typeSignature.member(newTermName(name).encodedName)
    val Boolean_&& = BooleanTermMember("&&")
    val Boolean_|| = BooleanTermMember("||")
    Set(Boolean_&&, Boolean_||)
  }

  protected def isByName(fun: Tree): (Int => Boolean) = {
    if (Boolean_ShortCircuits contains fun.symbol) i => true
    else fun.tpe match {
      case MethodType(params, _) =>
        val isByNameParams = params.map(_.asTerm.isByNameParam)
        (i: Int) => isByNameParams.applyOrElse(i, (_: Int) => false)
      case _                     => Map()
    }
  }

  private[async] object defn {
    def mkList_apply[A](args: List[Expr[A]]): Expr[List[A]] = {
      c.Expr(Apply(Ident(definitions.List_apply), args.map(_.tree)))
    }

    def mkList_contains[A](self: Expr[List[A]])(elem: Expr[Any]) = reify(self.splice.contains(elem.splice))

    def mkFunction_apply[A, B](self: Expr[Function1[A, B]])(arg: Expr[A]) = reify {
      self.splice.apply(arg.splice)
    }

    def mkAny_==(self: Expr[Any])(other: Expr[Any]) = reify {
      self.splice == other.splice
    }

    def mkTry_get[A](self: Expr[util.Try[A]]) = reify {
      self.splice.get
    }

    val Try_get = methodSym(reify((null: scala.util.Try[Any]).get))

    val TryClass      = c.mirror.staticClass("scala.util.Try")
    val TryAnyType    = appliedType(TryClass.toType, List(definitions.AnyTpe))
    val NonFatalClass = c.mirror.staticModule("scala.util.control.NonFatal")

    val Async_await = {
      val asyncMod = c.mirror.staticClass("scala.async.AsyncBase")
      val tpe = asyncMod.asType.toType
      tpe.member(c.universe.newTermName("await")).ensuring(_ != NoSymbol)
    }
  }


  /** `termSym( (_: Foo).bar(null: A, null: B)` will return the symbol of `bar`, after overload resolution. */
  private def methodSym(apply: c.Expr[Any]): Symbol = {
    val tree2: Tree = c.typeCheck(apply.tree)
    tree2.collect {
      case s: SymTree if s.symbol.isMethod => s.symbol
    }.headOption.getOrElse(sys.error(s"Unable to find a method symbol in ${apply.tree}"))
  }

  /** Using [[scala.reflect.api.Trees.TreeCopier]] copies more than we would like:
    * we don't want to copy types and symbols to the new trees in some cases.
    *
    * Instead, we just copy positions and attachments.
    */
  object attachCopy {
    def copyAttach[T <: Tree](orig: Tree, tree: T): tree.type = {
      tree.setPos(orig.pos)
      for (att <- orig.attachments.all)
        tree.updateAttachment[Any](att)(ClassTag.apply[Any](att.getClass))
      tree
    }

    def Apply(tree: Tree)(fun: Tree, args: List[Tree]): Apply =
      copyAttach(tree, c.universe.Apply(fun, args))

    def Assign(tree: Tree)(lhs: Tree, rhs: Tree): Assign =
      copyAttach(tree, c.universe.Assign(lhs, rhs))

    def CaseDef(tree: Tree)(pat: Tree, guard: Tree, block: Tree): CaseDef =
      copyAttach(tree, c.universe.CaseDef(pat, guard, block))

    def If(tree: Tree)(cond: Tree, thenp: Tree, elsep: Tree): If =
      copyAttach(tree, c.universe.If(cond, thenp, elsep))

    def Match(tree: Tree)(selector: Tree, cases: List[CaseDef]): Match =
      copyAttach(tree, c.universe.Match(selector, cases))

    def Select(tree: Tree)(qual: Tree, name: Name): Select =
      copyAttach(tree, c.universe.Select(qual, name))

    def TypeApply(tree: Tree)(fun: Tree, args: List[Tree]): TypeApply = {
      copyAttach(tree, c.universe.TypeApply(fun, args))
    }

    def ValDef(tree: Tree)(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree): ValDef =
      copyAttach(tree, c.universe.ValDef(mods, name, tpt, rhs))
  }
}
