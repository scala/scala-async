package scala.async
package run
package uncheckedBounds

import org.junit.{Test, Assert}
import scala.async.TreeInterrogation

class UncheckedBoundsSpec {
  @Test def insufficientLub_SI_7694() {
    suppressingFailureBefore2_10_3 {
      eval( s"""
      object Test {
        import _root_.scala.async.run.toughtype._
        import _root_.scala.async.internal.AsyncId.{async, await}
        async {
          (if (true) await(null: L[A, A]) else await(null: L[B, B]))
        }
    }
    """, compileOptions = s"-cp ${toolboxClasspath} ")
    }
  }

  @Test def insufficientLub_SI_7694_ScalaConcurrent() {
    suppressingFailureBefore2_10_3 {
      eval( s"""
      object Test {
        import _root_.scala.async.run.toughtype._
        import _root_.scala.async.Async.{async, await}
        import scala.concurrent._
        import scala.concurrent.ExecutionContext.Implicits.global
        async {
          (if (true) await(null: Future[L[A, A]]) else await(null: Future[L[B, B]]))
        }
    }
    """, compileOptions = s"-cp ${toolboxClasspath} ")
    }
  }

  private def suppressingFailureBefore2_10_3(body: => Any) {
    try {
      body
    } catch {
      case x: Throwable =>
        // @uncheckedBounds was only introduced in 2.10.3/ 2.11.0-M5, so avoid reporting this test failure in those cases.
        scala.util.Properties.versionNumberString match {
          case "2.10.0" | "2.10.1" | "2.10.2" | "2.11.0-M4" => // ignore, the @uncheckedBounds doesn't exist yet
          case _                                            =>
            val annotationExists =
              reflect.runtime.currentMirror.staticClass("scala.reflect.internal.annotations.uncheckedBounds") == reflect.runtime.universe.NoSymbol
            if (annotationExists)
              Assert.fail("@uncheckedBounds not found in scala-reflect.jar")
            else
              Assert.fail(s"@uncheckedBounds exists, but it didn't prevent this failure: $x")
        }
    }
  }
}
