/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async

import scala.reflect.macros.Context
import reflect.ClassTag

/**
 * Utilities used in both `ExprBuilder` and `AnfTransform`.
 */
private[async] final case class TransformUtils[C <: Context](c: C) {

  import c.universe._

  object name {
    def suffix(string: String) = string + "$async"

    def suffixedName(prefix: String) = newTermName(suffix(prefix))

    val state         = suffixedName("state")
    val result        = suffixedName("result")
    val resume        = suffixedName("resume")
    val execContext   = suffixedName("execContext")
    val stateMachine  = newTermName(fresh("stateMachine"))
    val stateMachineT = stateMachine.toTypeName
    val apply         = newTermName("apply")
    val applyOrElse   = newTermName("applyOrElse")
    val tr            = newTermName("tr")
    val matchRes      = "matchres"
    val ifRes         = "ifres"
    val await         = "await"
    val bindSuffix    = "$bind"

    def arg(i: Int) = "arg" + i

    def fresh(name: TermName): TermName = newTermName(fresh(name.toString))

    def fresh(name: String): String = if (name.toString.contains("$")) name else c.fresh("" + name + "$")
  }

  def defaultValue(tpe: Type): Literal = {
    val defaultValue: Any =
      if (tpe <:< definitions.BooleanTpe) false
      else if (definitions.ScalaNumericValueClasses.exists(tpe <:< _.toType)) 0
      else if (tpe <:< definitions.AnyValTpe) 0
      else null
    Literal(Constant(defaultValue))
  }

  def isAwait(fun: Tree) =
    fun.symbol == defn.Async_await

  /** Replace all `Ident` nodes referring to one of the keys n `renameMap` with a node
    * referring to the corresponding new name
    */
  def substituteNames(tree: Tree, renameMap: Map[Symbol, Name]): Tree = {
    val renamer = new Transformer {
      override def transform(tree: Tree) = tree match {
        case Ident(_) => (renameMap get tree.symbol).fold(tree)(Ident(_))
        case tt: TypeTree if tt.original != EmptyTree && tt.original != null =>
          // We also have to apply our renaming transform on originals of TypeTrees.
          // TODO 2.10.1 Can we find a cleaner way?
          val symTab = c.universe.asInstanceOf[reflect.internal.SymbolTable]
          val tt1 = tt.asInstanceOf[symTab.TypeTree]
          tt1.setOriginal(transform(tt.original).asInstanceOf[symTab.Tree])
          super.transform(tree)
        case _        => super.transform(tree)
      }
    }
    renamer.transform(tree)
  }

  /** Descends into the regions of the tree that are subject to the
    * translation to a state machine by `async`. When a nested template,
    * function, or by-name argument is encountered, the descent stops,
    * and `nestedClass` etc are invoked.
    */
  trait AsyncTraverser extends Traverser {
    def nestedClass(classDef: ClassDef) {
    }

    def nestedModule(module: ModuleDef) {
    }

    def nestedMethod(module: DefDef) {
    }

    def byNameArgument(arg: Tree) {
    }

    def function(function: Function) {
    }

    def patMatFunction(tree: Match) {
    }

    override def traverse(tree: Tree) {
      tree match {
        case cd: ClassDef          => nestedClass(cd)
        case md: ModuleDef         => nestedModule(md)
        case dd: DefDef            => nestedMethod(dd)
        case fun: Function         => function(fun)
        case m@Match(EmptyTree, _) => patMatFunction(m) // Pattern matching anonymous function under -Xoldpatmat of after `restorePatternMatchingFunctions`
        case Apply(fun, args)      =>
          val isInByName = isByName(fun)
          for ((arg, index) <- args.zipWithIndex) {
            if (!isInByName(index)) traverse(arg)
            else byNameArgument(arg)
          }
          traverse(fun)
        case _                     => super.traverse(tree)
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

  def isByName(fun: Tree): (Int => Boolean) = {
    if (Boolean_ShortCircuits contains fun.symbol) i => true
    else fun.tpe match {
      case MethodType(params, _) =>
        val isByNameParams = params.map(_.asTerm.isByNameParam)
        (i: Int) => isByNameParams.applyOrElse(i, (_: Int) => false)
      case _                     => Map()
    }
  }

  def statsAndExpr(tree: Tree): (List[Tree], Tree) = tree match {
    case Block(stats, expr) => (stats, expr)
    case _                  => (List(tree), Literal(Constant(())))
  }

  def mkVarDefTree(resultType: Type, resultName: TermName): c.Tree = {
    ValDef(Modifiers(Flag.MUTABLE), resultName, TypeTree(resultType), defaultValue(resultType))
  }

  def emptyConstructor: DefDef = {
    val emptySuperCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), Nil)
    DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(emptySuperCall), c.literalUnit.tree))
  }

  def applied(className: String, types: List[Type]): AppliedTypeTree =
    AppliedTypeTree(Ident(c.mirror.staticClass(className)), types.map(TypeTree(_)))

  object defn {
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

    val Try_get       = methodSym(reify((null: scala.util.Try[Any]).get))
    val Try_isFailure = methodSym(reify((null: scala.util.Try[Any]).isFailure))

    val TryClass      = c.mirror.staticClass("scala.util.Try")
    val TryAnyType    = appliedType(TryClass.toType, List(definitions.AnyTpe))
    val NonFatalClass = c.mirror.staticModule("scala.util.control.NonFatal")

    private def asyncMember(name: String) = {
      val asyncMod = c.mirror.staticClass("scala.async.AsyncBase")
      val tpe = asyncMod.asType.toType
      tpe.member(newTermName(name)).ensuring(_ != NoSymbol)
    }

    val Async_await = asyncMember("await")
  }

  /** `termSym( (_: Foo).bar(null: A, null: B)` will return the symbol of `bar`, after overload resolution. */
  private def methodSym(apply: c.Expr[Any]): Symbol = {
    val tree2: Tree = c.typeCheck(apply.tree)
    tree2.collect {
      case s: SymTree if s.symbol.isMethod => s.symbol
    }.headOption.getOrElse(sys.error(s"Unable to find a method symbol in ${apply.tree}"))
  }

  /**
   * Using [[scala.reflect.api.Trees.TreeCopier]] copies more than we would like:
   * we don't want to copy types and symbols to the new trees in some cases.
   *
   * Instead, we just copy positions and attachments.
   */
  def attachCopy[T <: Tree](orig: Tree)(tree: T): tree.type = {
    tree.setPos(orig.pos)
    for (att <- orig.attachments.all)
      tree.updateAttachment[Any](att)(ClassTag.apply[Any](att.getClass))
    tree
  }

  def resetInternalAttrs(tree: Tree, internalSyms: List[Symbol]) =
    new ResetInternalAttrs(internalSyms.toSet).transform(tree)

  /**
   * Adaptation of [[scala.reflect.internal.Trees.ResetAttrs]]
   *
   * A transformer which resets symbol and tpe fields of all nodes in a given tree,
   * with special treatment of:
   * `TypeTree` nodes: are replaced by their original if it exists, otherwise tpe field is reset
   * to empty if it started out empty or refers to local symbols (which are erased).
   * `TypeApply` nodes: are deleted if type arguments end up reverted to empty
   *
   * `This` and `Ident` nodes referring to an external symbol are ''not'' reset.
   */
  private final class ResetInternalAttrs(internalSyms: Set[Symbol]) extends Transformer {

    import language.existentials

    override def transform(tree: Tree): Tree = super.transform {
      def isExternal = tree.symbol != NoSymbol && !internalSyms(tree.symbol)

      tree match {
        case tpt: TypeTree                         => resetTypeTree(tpt)
        case TypeApply(fn, args)
          if args map transform exists (_.isEmpty) => transform(fn)
        case EmptyTree                             => tree
        case (_: Ident | _: This) if isExternal    => tree // #35 Don't reset the symbol of Ident/This bound outside of the async block
        case _                                     => resetTree(tree)
      }
    }

    private def resetTypeTree(tpt: TypeTree): Tree = {
      if (tpt.original != null)
        transform(tpt.original)
      else if (tpt.tpe != null && tpt.asInstanceOf[symtab.TypeTree forSome {val symtab: reflect.internal.SymbolTable}].wasEmpty) {
        val dupl = tpt.duplicate
        dupl.tpe = null
        dupl
      }
      else tpt
    }

    private def resetTree(tree: Tree): Tree = {
      val hasSymbol: Boolean = {
        val reflectInternalTree = tree.asInstanceOf[symtab.Tree forSome {val symtab: reflect.internal.SymbolTable}]
        reflectInternalTree.hasSymbol
      }
      val dupl = tree.duplicate
      if (hasSymbol)
        dupl.symbol = NoSymbol
      dupl.tpe = null
      dupl
    }
  }

  /**
   * Replaces expressions of the form `{ new $anon extends PartialFunction[A, B] { ... ; def applyOrElse[..](...) = ... match <cases> }`
   * with `Match(EmptyTree, cases`.
   *
   * This reverses the transformation performed in `Typers`, and works around non-idempotency of typechecking such trees.
   */
  // TODO Reference JIRA issue.
  final def restorePatternMatchingFunctions(tree: Tree) =
    RestorePatternMatchingFunctions transform tree

  private object RestorePatternMatchingFunctions extends Transformer {

    import language.existentials
    val DefaultCaseName: TermName = "defaultCase$"

    override def transform(tree: Tree): Tree = {
      val SYNTHETIC = (1 << 21).toLong.asInstanceOf[FlagSet]
      def isSynthetic(cd: ClassDef) = cd.mods hasFlag SYNTHETIC

      /** Is this pattern node a synthetic catch-all case, added during PartialFuction synthesis before we know
        * whether the user provided cases are exhaustive. */
      def isSyntheticDefaultCase(cdef: CaseDef) = cdef match {
        case CaseDef(Bind(DefaultCaseName, _), EmptyTree, _) => true
        case _                                                => false
      }
      tree match {
        case Block(
        (cd@ClassDef(_, _, _, Template(_, _, body))) :: Nil,
        Apply(Select(New(a), nme.CONSTRUCTOR), Nil)) if isSynthetic(cd) =>
          val restored = (body collectFirst {
            case DefDef(_, /*name.apply | */ name.applyOrElse, _, _, _, Match(_, cases)) =>
              val nonSyntheticCases = cases.takeWhile(cdef => !isSyntheticDefaultCase(cdef))
              val transformedCases = super.transformStats(nonSyntheticCases, currentOwner).asInstanceOf[List[CaseDef]]
              Match(EmptyTree, transformedCases)
          }).getOrElse(c.abort(tree.pos, s"Internal Error: Unable to find original pattern matching cases in: $body"))
          restored
        case t                                                          => super.transform(t)
      }
    }
  }


  def isSafeToInline(tree: Tree) = {
    val symtab = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
    object treeInfo extends {
      val global: symtab.type = symtab
    } with reflect.internal.TreeInfo
    val castTree = tree.asInstanceOf[symtab.Tree]
    treeInfo.isExprSafeToInline(castTree)
  }

  /** Map a list of arguments to:
    * - A list of argument Trees
    * - A list of auxillary results.
    *
    * The function unwraps and rewraps the `arg :_*` construct.
    *
    * @param args The original argument trees
    * @param f  A function from argument (with '_*' unwrapped) and argument index to argument.
    * @tparam A The type of the auxillary result
    */
  def mapArguments[A](args: List[Tree])(f: (Tree, Int) => (A, Tree)): (List[A], List[Tree]) = {
    args match {
      case args :+ Typed(tree, Ident(tpnme.WILDCARD_STAR)) =>
        val (a, argExprs :+ lastArgExpr) = (args :+ tree).zipWithIndex.map(f.tupled).unzip
        val exprs = argExprs :+ Typed(lastArgExpr, Ident(tpnme.WILDCARD_STAR)).setPos(lastArgExpr.pos)
        (a, exprs)
      case args                                            =>
        args.zipWithIndex.map(f.tupled).unzip
    }
  }
}
