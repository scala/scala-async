/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package neg

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test

@RunWith(classOf[JUnit4])
class SampleNegSpec {
  @Test
  def `missing symbol`() {
    expectError("not found: value kaboom") {
      """
        | kaboom
      """.stripMargin
    }
  }
}
