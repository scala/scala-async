/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */
package scala.async.internal

import scala.reflect.macros.Context
import reflect.ClassTag
import scala.collection.immutable.ListMap

/**
 * Utilities used in both `ExprBuilder` and `AnfTransform`.
 */
private[async] trait TransformUtils {
  self: AsyncMacro =>

  import c.universe._
  import c.internal._
  import decorators._

  private object baseNames {

    val matchRes = "matchres"
    val ifRes = "ifres"
    val bindSuffix = "$bind"
    val completed = newTermName("completed")

    val state = newTermName("state")
    val result = newTermName(self.futureSystem.resultFieldName)
    val execContext = newTermName("execContext")
    val tr = newTermName("tr")
    val t = newTermName("throwable")
  }
  
  object name {
    def matchRes      = maybeFresh(baseNames.matchRes)
    def ifRes         = maybeFresh(baseNames.ifRes) 
    def bindSuffix    = maybeFresh(baseNames.bindSuffix)
    def completed     = maybeFresh(baseNames.completed) 

    val state         = maybeFresh(baseNames.state)
    val result        = baseNames.result
    val execContext   = maybeFresh(baseNames.execContext)
    val tr            = maybeFresh(baseNames.tr)
    val t             = maybeFresh(baseNames.t)

    val await = "await"
    val resume = newTermName("resume")
    val apply = newTermName("apply")
    val stateMachine  = newTermName(fresh("stateMachine"))
    val stateMachineT = stateMachine.toTypeName

    def maybeFresh(name: TermName): TermName = if (self.asyncBase.futureSystem.freshenAllNames) fresh(name) else name
    def maybeFresh(name: String): String = if (self.asyncBase.futureSystem.freshenAllNames) fresh(name) else name
    def fresh(name: TermName): TermName = c.freshName(name)

    def fresh(name: String): String = c.freshName(name)
  }

  def maybeTry(block: Tree, catches: List[CaseDef], finalizer: Tree) = if (asyncBase.futureSystem.emitTryCatch) Try(block, catches, finalizer) else block

  def isAsync(fun: Tree) =
    fun.symbol == defn.Async_async

  def isAwait(fun: Tree) =
    fun.symbol == defn.Async_await

  def newBlock(stats: List[Tree], expr: Tree): Block = {
    Block(stats, expr)
  }

  def isLiteralUnit(t: Tree) = t match {
    case Literal(Constant(())) =>
      true
    case _ => false
  }

  def isPastTyper =
    c.universe.asInstanceOf[scala.reflect.internal.SymbolTable].isPastTyper

  // Copy pasted from TreeInfo in the compiler.
  // Using a quasiquote pattern like `case q"$fun[..$targs](...$args)" => is not
  // sufficient since https://github.com/scala/scala/pull/3656 as it doesn't match
  // constructor invocations.
  class Applied(val tree: Tree) {
    /** The tree stripped of the possibly nested applications.
     *  The original tree if it's not an application.
     */
    def callee: Tree = {
      def loop(tree: Tree): Tree = tree match {
        case Apply(fn, _) => loop(fn)
        case tree         => tree
      }
      loop(tree)
    }

    /** The `callee` unwrapped from type applications.
     *  The original `callee` if it's not a type application.
     */
    def core: Tree = callee match {
      case TypeApply(fn, _)       => fn
      case AppliedTypeTree(fn, _) => fn
      case tree                   => tree
    }

    /** The type arguments of the `callee`.
     *  `Nil` if the `callee` is not a type application.
     */
    def targs: List[Tree] = callee match {
      case TypeApply(_, args)       => args
      case AppliedTypeTree(_, args) => args
      case _                        => Nil
    }

    /** (Possibly multiple lists of) value arguments of an application.
     *  `Nil` if the `callee` is not an application.
     */
    def argss: List[List[Tree]] = {
      def loop(tree: Tree): List[List[Tree]] = tree match {
        case Apply(fn, args) => loop(fn) :+ args
        case _               => Nil
      }
      loop(tree)
    }
  }

  /** Returns a wrapper that knows how to destructure and analyze applications.
   */
  def dissectApplied(tree: Tree) = new Applied(tree)

  /** Destructures applications into important subparts described in `Applied` class,
   *  namely into: core, targs and argss (in the specified order).
   *
   *  Trees which are not applications are also accepted. Their callee and core will
   *  be equal to the input, while targs and argss will be Nil.
   *
   *  The provided extractors don't expose all the API of the `Applied` class.
   *  For advanced use, call `dissectApplied` explicitly and use its methods instead of pattern matching.
   */
  object Applied {
    def apply(tree: Tree): Applied = new Applied(tree)

    def unapply(applied: Applied): Option[(Tree, List[Tree], List[List[Tree]])] =
      Some((applied.core, applied.targs, applied.argss))

    def unapply(tree: Tree): Option[(Tree, List[Tree], List[List[Tree]])] =
      unapply(dissectApplied(tree))
  }
  private lazy val Boolean_ShortCircuits: Set[Symbol] = {
    import definitions.BooleanClass
    def BooleanTermMember(name: String) = BooleanClass.typeSignature.member(newTermName(name).encodedName)
    val Boolean_&& = BooleanTermMember("&&")
    val Boolean_|| = BooleanTermMember("||")
    Set(Boolean_&&, Boolean_||)
  }

  private def isByName(fun: Tree): ((Int, Int) => Boolean) = {
    if (Boolean_ShortCircuits contains fun.symbol) (i, j) => true
    else if (fun.tpe == null) (x, y) => false
    else {
      val paramss = fun.tpe.paramss
      val byNamess = paramss.map(_.map(_.asTerm.isByNameParam))
      (i, j) => util.Try(byNamess(i)(j)).getOrElse(false)
    }
  }
  private def argName(fun: Tree): ((Int, Int) => String) = {
    val paramss = fun.tpe.paramss
    val namess = paramss.map(_.map(_.name.toString))
    (i, j) => util.Try(namess(i)(j)).getOrElse(s"arg_${i}_${j}")
  }

  object defn {
    def mkList_apply[A](args: List[Expr[A]]): Expr[List[A]] = {
      c.Expr(Apply(Ident(definitions.List_apply), args.map(_.tree)))
    }

    def mkList_contains[A](self: Expr[List[A]])(elem: Expr[Any]) = reify {
      self.splice.contains(elem.splice)
    }

    def mkAny_==(self: Expr[Any])(other: Expr[Any]) = reify {
      self.splice == other.splice
    }

    def mkTry_get[A](self: Expr[util.Try[A]]) = reify {
      self.splice.get
    }

    val NonFatalClass = rootMirror.staticModule("scala.util.control.NonFatal")
    val ThrowableClass = rootMirror.staticClass("java.lang.Throwable")
    lazy val Async_async   = asyncBase.asyncMethod(c.universe)(c.macroApplication.symbol)
    lazy val Async_await   = asyncBase.awaitMethod(c.universe)(c.macroApplication.symbol)
    val IllegalStateExceptionClass = rootMirror.staticClass("java.lang.IllegalStateException")
  }

  // `while(await(x))` ... or `do { await(x); ... } while(...)` contain an `If` that loops;
  // we must break that `If` into states so that it convert the label jump into a state machine
  // transition
  final def containsForiegnLabelJump(t: Tree): Boolean = {
    val labelDefs = t.collect {
      case ld: LabelDef => ld.symbol
    }.toSet
    val result = t.exists {
      case rt: RefTree => rt.symbol != null && isLabel(rt.symbol) && !(labelDefs contains rt.symbol)
      case _ => false
    }
    result
  }

  def isLabel(sym: Symbol): Boolean = {
    val LABEL = 1L << 17 // not in the public reflection API.
    (internal.flags(sym).asInstanceOf[Long] & LABEL) != 0L
  }
  def isSynth(sym: Symbol): Boolean = {
    val SYNTHETIC = 1 << 21 // not in the public reflection API.
    (internal.flags(sym).asInstanceOf[Long] & SYNTHETIC) != 0L
  }
  def symId(sym: Symbol): Int = {
    val symtab = this.c.universe.asInstanceOf[reflect.internal.SymbolTable]
    sym.asInstanceOf[symtab.Symbol].id
  }
  def substituteTrees(t: Tree, from: List[Symbol], to: List[Tree]): Tree = {
    val symtab = this.c.universe.asInstanceOf[reflect.internal.SymbolTable]
    val subst = new symtab.TreeSubstituter(from.asInstanceOf[List[symtab.Symbol]], to.asInstanceOf[List[symtab.Tree]])
    subst.transform(t.asInstanceOf[symtab.Tree]).asInstanceOf[Tree]
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
  private def mapArguments[A](args: List[Tree])(f: (Tree, Int) => (A, Tree)): (List[A], List[Tree]) = {
    args match {
      case args :+ Typed(tree, Ident(tpnme.WILDCARD_STAR)) =>
        val (a, argExprs :+ lastArgExpr) = (args :+ tree).zipWithIndex.map(f.tupled).unzip
        val exprs = argExprs :+ atPos(lastArgExpr.pos.makeTransparent)(Typed(lastArgExpr, Ident(tpnme.WILDCARD_STAR)))
        (a, exprs)
      case args                                            =>
        args.zipWithIndex.map(f.tupled).unzip
    }
  }

  case class Arg(expr: Tree, isByName: Boolean, argName: String)

  /**
   * Transform a list of argument lists, producing the transformed lists, and lists of auxillary
   * results.
   *
   * The function `f` need not concern itself with varargs arguments e.g (`xs : _*`). It will
   * receive `xs`, and it's result will be re-wrapped as `f(xs) : _*`.
   *
   * @param fun   The function being applied
   * @param argss The argument lists
   * @return      (auxillary results, mapped argument trees)
   */
  def mapArgumentss[A](fun: Tree, argss: List[List[Tree]])(f: Arg => (A, Tree)): (List[List[A]], List[List[Tree]]) = {
    val isByNamess: (Int, Int) => Boolean = isByName(fun)
    val argNamess: (Int, Int) => String = argName(fun)
    argss.zipWithIndex.map { case (args, i) =>
      mapArguments[A](args) {
        (tree, j) => f(Arg(tree, isByNamess(i, j), argNamess(i, j)))
      }
    }.unzip
  }


  def statsAndExpr(tree: Tree): (List[Tree], Tree) = tree match {
    case Block(stats, expr) => (stats, expr)
    case _                  => (List(tree), Literal(Constant(())))
  }

  def emptyConstructor: DefDef = {
    val emptySuperCall = Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), Nil)
    DefDef(NoMods, nme.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(emptySuperCall), Literal(Constant(()))))
  }

  def applied(className: String, types: List[Type]): AppliedTypeTree =
    AppliedTypeTree(Ident(rootMirror.staticClass(className)), types.map(TypeTree(_)))

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

    def nestedMethod(defdef: DefDef) {
    }

    def byNameArgument(arg: Tree) {
    }

    def function(function: Function) {
    }

    def patMatFunction(tree: Match) {
    }

    override def traverse(tree: Tree) {
      tree match {
        case _ if isAsync(tree) =>
          // Under -Ymacro-expand:discard, used in the IDE, nested async blocks will be visible to the outer blocks
        case cd: ClassDef          => nestedClass(cd)
        case md: ModuleDef         => nestedModule(md)
        case dd: DefDef            => nestedMethod(dd)
        case fun: Function         => function(fun)
        case m@Match(EmptyTree, _) => patMatFunction(m) // Pattern matching anonymous function under -Xoldpatmat of after `restorePatternMatchingFunctions`
        case q"$fun[..$targs](...$argss)" if argss.nonEmpty =>
          val isInByName = isByName(fun)
          for ((args, i) <- argss.zipWithIndex) {
            for ((arg, j) <- args.zipWithIndex) {
              if (!isInByName(i, j)) traverse(arg)
              else byNameArgument(arg)
            }
          }
          traverse(fun)
        case _                     => super.traverse(tree)
      }
    }
  }

  def transformAt(tree: Tree)(f: PartialFunction[Tree, (TypingTransformApi => Tree)]) = {
    typingTransform(tree)((tree, api) => {
      if (f.isDefinedAt(tree)) f(tree)(api)
      else api.default(tree)
    })
  }

  def toMultiMap[A, B](as: Iterable[(A, B)]): Map[A, List[B]] =
    as.toList.groupBy(_._1).mapValues(_.map(_._2).toList).toMap

  // Attributed version of `TreeGen#mkCastPreservingAnnotations`
  def mkAttributedCastPreservingAnnotations(tree: Tree, tp: Type): Tree = {
    atPos(tree.pos) {
      val casted = c.typecheck(gen.mkCast(tree, uncheckedBounds(withoutAnnotations(tp)).dealias))
      Typed(casted, TypeTree(tp)).setType(tp)
    }
  }

  def deconst(tp: Type): Type = tp match {
    case AnnotatedType(anns, underlying) => annotatedType(anns, deconst(underlying))
    case ExistentialType(quants, underlying) => existentialType(quants, deconst(underlying))
    case ConstantType(value) => deconst(value.tpe)
    case _ => tp
  }

  def withAnnotation(tp: Type, ann: Annotation): Type = withAnnotations(tp, List(ann))

  def withAnnotations(tp: Type, anns: List[Annotation]): Type = tp match {
    case AnnotatedType(existingAnns, underlying) => annotatedType(anns ::: existingAnns, underlying)
    case ExistentialType(quants, underlying) => existentialType(quants, withAnnotations(underlying, anns))
    case _ => annotatedType(anns, tp)
  }

  def withoutAnnotations(tp: Type): Type = tp match {
    case AnnotatedType(anns, underlying) => withoutAnnotations(underlying)
    case ExistentialType(quants, underlying) => existentialType(quants, withoutAnnotations(underlying))
    case _ => tp
  }

  def tpe(sym: Symbol): Type = {
    if (sym.isType) sym.asType.toType
    else sym.info
  }

  def thisType(sym: Symbol): Type = {
    if (sym.isClass) sym.asClass.thisPrefix
    else NoPrefix
  }

  private def derivedValueClassUnbox(cls: Symbol) =
    (cls.info.decls.find(sym => sym.isMethod && sym.asTerm.isParamAccessor) getOrElse NoSymbol)

  def mkZero(tp: Type): Tree = {
    val tpSym = tp.typeSymbol
    if (tpSym.isClass && tpSym.asClass.isDerivedValueClass) {
      val argZero = mkZero(derivedValueClassUnbox(tpSym).infoIn(tp).resultType)
      val baseType = tp.baseType(tpSym) // use base type here to dealias / strip phantom "tagged types" etc.

      // By explicitly attributing the types and symbols here, we subvert privacy.
      // Otherwise, ticket86PrivateValueClass would fail.

      // Approximately:
      // q"new ${valueClass}[$..targs](argZero)"
      val target: Tree = gen.mkAttributedSelect(
        c.typecheck(atMacroPos(
        New(TypeTree(baseType)))), tpSym.asClass.primaryConstructor)

      val zero = gen.mkMethodCall(target, argZero :: Nil)
      // restore the original type which we might otherwise have weakened with `baseType` above
      c.typecheck(atMacroPos(gen.mkCast(zero, tp)))
    } else {
      gen.mkZero(tp)
    }
  }

  // =====================================
  // Copy/Pasted from Scala 2.10.3. See SI-7694.
  private lazy val UncheckedBoundsClass = {
    try c.mirror.staticClass("scala.reflect.internal.annotations.uncheckedBounds")
    catch { case _: ScalaReflectionException => NoSymbol }
  }
  final def uncheckedBounds(tp: Type): Type = {
    if ((tp.typeArgs.isEmpty && (tp match { case _: TypeRef => true; case _ => false}))|| UncheckedBoundsClass == NoSymbol) tp
    else withAnnotation(tp, Annotation(UncheckedBoundsClass.asType.toType, Nil, ListMap()))
  }
  // =====================================

  /**
   * Efficiently decorate each subtree within `t` with the result of `t exists isAwait`,
   * and return a function that can be used on derived trees to efficiently test the
   * same condition.
   *
   * If the derived tree contains synthetic wrapper trees, these will be recursed into
   * in search of a sub tree that was decorated with the cached answer.
   */
  final def containsAwaitCached(t: Tree): Tree => Boolean = {
    if (c.macroApplication.symbol == null) return (t => false)

    def treeCannotContainAwait(t: Tree) = t match {
      case _: Ident | _: TypeTree | _: Literal => true
      case _ => isAsync(t)
    }
    def shouldAttach(t: Tree) = !treeCannotContainAwait(t)
    val symtab = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
    def attachContainsAwait(t: Tree): Unit = if (shouldAttach(t)) {
      val t1 = t.asInstanceOf[symtab.Tree]
      t1.updateAttachment(ContainsAwait)
      t1.removeAttachment[NoAwait.type]
    }
    def attachNoAwait(t: Tree): Unit = if (shouldAttach(t)) {
      val t1 = t.asInstanceOf[symtab.Tree]
      t1.updateAttachment(NoAwait)
    }
    object markContainsAwaitTraverser extends Traverser {
      var stack: List[Tree] = Nil

      override def traverse(tree: Tree): Unit = {
        stack ::= tree
        try {
          if (isAsync(tree)) {
            ;
          } else {
            if (isAwait(tree))
              stack.foreach(attachContainsAwait)
            else
              attachNoAwait(tree)
            super.traverse(tree)
          }
        } finally stack = stack.tail
      }
    }
    markContainsAwaitTraverser.traverse(t)

    (t: Tree) => {
      object traverser extends Traverser {
        var containsAwait = false
        override def traverse(tree: Tree): Unit = {
          def castTree = tree.asInstanceOf[symtab.Tree]
          if (!castTree.hasAttachment[NoAwait.type]) {
            if (castTree.hasAttachment[ContainsAwait.type])
              containsAwait = true
            else if (!treeCannotContainAwait(t))
              super.traverse(tree)
          }
        }
      }
      traverser.traverse(t)
      traverser.containsAwait
    }
  }

  final def cleanupContainsAwaitAttachments(t: Tree): t.type = {
    val symtab = c.universe.asInstanceOf[scala.reflect.internal.SymbolTable]
    t.foreach {t =>
      t.asInstanceOf[symtab.Tree].removeAttachment[ContainsAwait.type]
      t.asInstanceOf[symtab.Tree].removeAttachment[NoAwait.type]
    }
    t
  }

  // First modification to translated patterns:
  //  - Set the type of label jumps to `Unit`
  //  - Propagate this change to trees known to directly enclose them:
  //    ``If` / `Block`) adjust types of enclosing
  final def adjustTypeOfTranslatedPatternMatches(t: Tree, owner: Symbol): Tree = {
    import definitions.UnitTpe
    typingTransform(t, owner) {
      (tree, api) =>
        tree match {
          case LabelDef(name, params, rhs) =>
            val rhs1 = api.recur(rhs)
            if (rhs1.tpe =:= UnitTpe) {
              internal.setInfo(tree.symbol, internal.methodType(tree.symbol.info.paramLists.head, UnitTpe))
              treeCopy.LabelDef(tree, name, params, rhs1)
            } else {
              treeCopy.LabelDef(tree, name, params, rhs1)
            }
          case Block(stats, expr) =>
            val stats1 = stats map api.recur
            val expr1 = api.recur(expr)
            if (expr1.tpe =:= UnitTpe)
              internal.setType(treeCopy.Block(tree, stats1, expr1), UnitTpe)
            else
              treeCopy.Block(tree, stats1, expr1)
          case If(cond, thenp, elsep) =>
            val cond1 = api.recur(cond)
            val thenp1 = api.recur(thenp)
            val elsep1 = api.recur(elsep)
            if (thenp1.tpe =:= definitions.UnitTpe && elsep.tpe =:= UnitTpe)
              internal.setType(treeCopy.If(tree, cond1, thenp1, elsep1), UnitTpe)
            else
              treeCopy.If(tree, cond1, thenp1, elsep1)
          case Apply(fun, args) if isLabel(fun.symbol) =>
            internal.setType(treeCopy.Apply(tree, api.recur(fun), args map api.recur), UnitTpe)
          case vd @ ValDef(mods, name, tpt, rhs) if isCaseTempVal(vd.symbol) =>
            def addUncheckedBounds(t: Tree) = {
              typingTransform(t, owner) {
                (tree, api) =>
                  if (tree.tpe == null) tree else internal.setType(api.default(tree), uncheckedBoundsIfNeeded(tree.tpe))
              }

            }
            val uncheckedRhs = addUncheckedBounds(api.recur(rhs))
            val uncheckedTpt = addUncheckedBounds(tpt)
            internal.setInfo(vd.symbol, uncheckedBoundsIfNeeded(vd.symbol.info))
            treeCopy.ValDef(vd, mods, name, uncheckedTpt, uncheckedRhs)
          case t => api.default(t)
        }
    }
  }

  private def isExistentialSkolem(s: Symbol) = {
    val EXISTENTIAL: Long = 1L << 35
    internal.isSkolem(s) && (internal.flags(s).asInstanceOf[Long] & EXISTENTIAL) != 0
  }
  private def isCaseTempVal(s: Symbol) = {
    s.isTerm && s.asTerm.isVal && s.isSynthetic && s.name.toString.startsWith("x")
  }

  def uncheckedBoundsIfNeeded(t: Type): Type = {
    var quantified: List[Symbol] = Nil
    var badSkolemRefs: List[Symbol] = Nil
    t.foreach {
      case et: ExistentialType =>
        quantified :::= et.quantified
      case TypeRef(pre, sym, args) =>
        val illScopedSkolems = args.map(_.typeSymbol).filter(arg => isExistentialSkolem(arg) && !quantified.contains(arg))
        badSkolemRefs :::= illScopedSkolems
      case _ =>
    }
    if (badSkolemRefs.isEmpty) t
    else t.map {
      case tp @ TypeRef(pre, sym, args) if args.exists(a => badSkolemRefs.contains(a.typeSymbol)) =>
        uncheckedBounds(tp)
      case t => t
    }
  }


  final def mkMutableField(tpt: Type, name: TermName, init: Tree): List[Tree] = {
    if (isPastTyper) {
      // If we are running after the typer phase (ie being called from a compiler plugin)
      // we have to create the trio of members manually.
      val ACCESSOR = (1L << 27).asInstanceOf[FlagSet]
      val STABLE = (1L << 22).asInstanceOf[FlagSet]
      val field = ValDef(Modifiers(Flag.MUTABLE | Flag.PRIVATE | Flag.LOCAL), name + " ", TypeTree(tpt), init)
      val getter = DefDef(Modifiers(ACCESSOR | STABLE), name, Nil, Nil, TypeTree(tpt), Select(This(tpnme.EMPTY), field.name))
      val setter = DefDef(Modifiers(ACCESSOR), name + "_=", Nil, List(List(ValDef(NoMods, TermName("x"), TypeTree(tpt), EmptyTree))), TypeTree(definitions.UnitTpe), Assign(Select(This(tpnme.EMPTY), field.name), Ident(TermName("x"))))
      field :: getter :: setter :: Nil
    } else {
      val result = ValDef(NoMods, name, TypeTree(tpt), init)
      result :: Nil
    }
  }

  def deriveLabelDef(ld: LabelDef, applyToRhs: Tree => Tree): LabelDef = {
    val rhs2 = applyToRhs(ld.rhs)
    val ld2 = treeCopy.LabelDef(ld, ld.name, ld.params, rhs2)
    if (ld eq ld2) ld
    else {
      val info2 = ld2.symbol.info match {
        case MethodType(params, p) => internal.methodType(params, rhs2.tpe)
        case t => t
      }
      internal.setInfo(ld2.symbol, info2)
      ld2
    }
  }
  object MatchEnd {
    def unapply(t: Tree): Option[LabelDef] = t match {
      case ValDef(_, _, _, t) => unapply(t)
      case ld: LabelDef if ld.name.toString.startsWith("matchEnd") => Some(ld)
      case _ => None
    }
  }
}

case object ContainsAwait
case object NoAwait