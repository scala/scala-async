/*
 * Copyright (C) 2012-2014 Lightbend Inc. <http://www.lightbend.com>
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
