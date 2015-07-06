/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run

import org.junit.Test

import scala.async.internal.AsyncId
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.{postfixOps, reflectiveCalls}


class WarningsSpec {

  @Test
  // https://github.com/scala/async/issues/74
  def noPureExpressionInStatementPositionWarning_t74() {
    val tb = mkToolbox(s"-cp ${toolboxClasspath} -Xfatal-warnings")
    // was: "a pure expression does nothing in statement position; you may be omitting necessary parentheses"
    tb.eval(tb.parse {
      """
        |  import scala.async.internal.AsyncId._
        |   async {
        |     if ("".isEmpty) {
        |       await(println("hello"))
        |       ()
        |     } else 42
        |   }
      """.stripMargin
    })
  }
}
