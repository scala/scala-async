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
package neg

import org.junit.Test

class SampleNegSpec {
  @Test
  def `missing symbol`(): Unit = {
    expectError("not found: value kaboom") {
      """
        | kaboom
      """.stripMargin
    }
  }
}
