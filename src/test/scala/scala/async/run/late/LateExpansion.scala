package scala.async.run.late

import java.io.File

import junit.framework.Assert.assertEquals
import org.junit.Test

import scala.annotation.StaticAnnotation
import scala.async.internal.{AsyncId, AsyncMacro}
import scala.reflect.internal.util.ScalaClassLoader.URLClassLoader
import scala.tools.nsc._
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.transform.TypingTransformers

// Tests for customized use of the async transform from a compiler plugin, which
// calls it from a new phase that runs after patmat.
class LateExpansion {
  @Test def test0(): Unit = {
    val result = wrapAndRun(
      """
        | @autoawait def id(a: String) = a
        | id("foo") + id("bar")
        | """.stripMargin)
    assertEquals("foobar", result)
  }
  @Test def testGuard(): Unit = {
    val result = wrapAndRun(
      """
        | @autoawait def id[A](a: A) = a
        | "" match { case _ if id(false) => ???; case _ => "okay" }
        | """.stripMargin)
    assertEquals("okay", result)
  }

  @Test def testExtractor(): Unit = {
    val result = wrapAndRun(
      """
        | object Extractor { @autoawait def unapply(a: String) = Some((a, a)) }
        | "" match { case Extractor(a, b) if "".isEmpty => a == b }
        | """.stripMargin)
    assertEquals(true, result)
  }

  @Test def testNestedMatchExtractor(): Unit = {
    val result = wrapAndRun(
      """
        | object Extractor { @autoawait def unapply(a: String) = Some((a, a)) }
        | "" match {
        |   case _ if "".isEmpty =>
        |     "" match { case Extractor(a, b) => a == b }
        | }
        | """.stripMargin)
    assertEquals(true, result)
  }

  @Test def testCombo(): Unit = {
    val result = wrapAndRun(
      """
        | object Extractor1 { @autoawait def unapply(a: String) = Some((a + 1, a + 2)) }
        | object Extractor2 { @autoawait def unapply(a: String) = Some(a + 3) }
        | @autoawait def id(a: String) = a
        | println("Test.test")
        | val r1 = Predef.identity("blerg") match {
        |   case x if " ".isEmpty => "case 2: " + x
        |   case Extractor1(Extractor2(x), y: String) if x == "xxx" => "case 1: " + x + ":" + y
        |     x match {
        |       case Extractor1(Extractor2(x), y: String) =>
        |       case _ =>
        |     }
        |   case Extractor2(x) => "case 3: " + x
        | }
        | r1
        | """.stripMargin)
    assertEquals("case 3: blerg3", result)
  }

  def wrapAndRun(code: String): Any = {
    run(
      s"""
         |import scala.async.run.late.{autoawait,lateasync}
         |object Test {
         |  @lateasync
         |  def test: Any = {
         |    $code
         |  }
         |}
         | """.stripMargin)
  }

  def run(code: String): Any = {
    val reporter = new StoreReporter
    val settings = new Settings(println(_))
    settings.outdir.value = sys.props("java.io.tmpdir")
    settings.embeddedDefaults(getClass.getClassLoader)
    val isInSBT = !settings.classpath.isSetByUser
    if (isInSBT) settings.usejavacp.value = true
    val global = new Global(settings, reporter) {
      self =>

      object late extends {
        val global: self.type = self
      } with LatePlugin

      override protected def loadPlugins(): List[Plugin] = late :: Nil
    }
    import global._

    val run = new Run
    val source = newSourceFile(code)
    run.compileSources(source :: Nil)
    assert(!reporter.hasErrors, reporter.infos.mkString("\n"))
    val loader = new URLClassLoader(Seq(new File(settings.outdir.value).toURI.toURL), global.getClass.getClassLoader)
    val cls = loader.loadClass("Test")
    cls.getMethod("test").invoke(null)
  }
}

abstract class LatePlugin extends Plugin {
  import global._

  override val components: List[PluginComponent] = List(new PluginComponent with TypingTransformers {
    val global: LatePlugin.this.global.type = LatePlugin.this.global

    lazy val asyncIdSym = symbolOf[AsyncId.type]
    lazy val asyncSym = asyncIdSym.info.member(TermName("async"))
    lazy val awaitSym = asyncIdSym.info.member(TermName("await"))
    lazy val autoAwaitSym = symbolOf[autoawait]
    lazy val lateAsyncSym = symbolOf[lateasync]

    def newTransformer(unit: CompilationUnit) = new TypingTransformer(unit) {
      override def transform(tree: Tree): Tree = {
        super.transform(tree) match {
          case ap@Apply(fun, args) if fun.symbol.hasAnnotation(autoAwaitSym) =>
            localTyper.typed(Apply(TypeApply(gen.mkAttributedRef(asyncIdSym.typeOfThis, awaitSym), TypeTree(ap.tpe) :: Nil), ap :: Nil))
          case dd: DefDef if dd.symbol.hasAnnotation(lateAsyncSym) => atOwner(dd.symbol) {
            val expandee = localTyper.context.withMacrosDisabled(
              localTyper.typed(Apply(TypeApply(gen.mkAttributedRef(asyncIdSym.typeOfThis, asyncSym), TypeTree(dd.rhs.tpe) :: Nil), List(dd.rhs)))
            )
            val c = analyzer.macroContext(localTyper, gen.mkAttributedRef(asyncIdSym), expandee)
            val asyncMacro = AsyncMacro(c, AsyncId)(dd.rhs)
            val code = asyncMacro.asyncTransform[Any](localTyper.typed(Literal(Constant(()))))(c.weakTypeTag[Any])
            deriveDefDef(dd)(_ => localTyper.typed(code))
          }
          case x => x
        }
      }
    }

    override def newPhase(prev: Phase): Phase = new StdPhase(prev) {
      override def apply(unit: CompilationUnit): Unit = {
        val translated = newTransformer(unit).transformUnit(unit)
        //println(show(unit.body))
        translated
      }
    }

    override val runsAfter: List[String] = "patmat" :: Nil
    override val phaseName: String = "postpatmat"

  })
  override val description: String = "postpatmat"
  override val name: String = "postpatmat"
}

// Methods with this annotation are translated to having the RHS wrapped in `AsyncId.async { <original RHS> }`
final class lateasync extends StaticAnnotation

// Calls to methods with this annotation are translated to `AsyncId.await(<call>)`
final class autoawait extends StaticAnnotation
