/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.async
package run
package stackoverflow

import org.junit.Test
import scala.async.internal.AsyncId


class StackOverflowSpec {

  @Test
  def stackSafety(): Unit = {
    import AsyncId._
    async {
      var i = 100000000
      while (i > 0) {
        if (false) {
          await(())
        }
        i -= 1
      }
    }
  }
}
