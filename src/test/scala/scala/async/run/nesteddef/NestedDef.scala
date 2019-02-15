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
package nesteddef

import org.junit.Test
import scala.async.internal.AsyncId

class NestedDef {

  @Test
  def nestedDef(): Unit = {
    import AsyncId._
    val result = async {
      val a = 0
      val x = await(a) - 1
      val local = 43
      def bar(d: Double) = -d + a + local
      def foo(z: Any) = (a.toDouble, bar(x).toDouble, z)
      foo(await(2))
    }
    result mustBe ((0d, 44d, 2))
  }


  @Test
  def nestedFunction(): Unit = {
    import AsyncId._
    val result = async {
      val a = 0
      val x = await(a) - 1
      val local = 43
      val bar = (d: Double) => -d + a + local
      val foo = (z: Any) => (a.toDouble, bar(x).toDouble, z)
      foo(await(2))
    }
    result mustBe ((0d, 44d, 2))
  }

  // We must lift `foo` and `bar` in the next two tests.
  @Test
  def nestedDefTransitive1(): Unit = {
    import AsyncId._
    val result = async {
      val a = 0
      val x = await(a) - 1
      def bar = a
      def foo = bar
      foo
    }
    result mustBe 0
  }

  @Test
  def nestedDefTransitive2(): Unit = {
    import AsyncId._
    val result = async {
      val a = 0
      val x = await(a) - 1
      def bar = a
      def foo = bar
      0
    }
    result mustBe 0
  }


  // checking that our use/definition analysis doesn't cycle.
  @Test
  def mutuallyRecursive1(): Unit = {
    import AsyncId._
    val result = async {
      val a = 0
      val x = await(a) - 1
      def foo: Int = if (true) 0 else bar
      def bar: Int = if (true) 0 else foo
      bar
    }
    result mustBe 0
  }

  // checking that our use/definition analysis doesn't cycle.
  @Test
  def mutuallyRecursive2(): Unit = {
    import AsyncId._
    val result = async {
      val a = 0
      def foo: Int = if (true) 0 else bar
      def bar: Int = if (true) 0 else foo
      val x = await(a) - 1
      bar
    }
    result mustBe 0
  }
}
