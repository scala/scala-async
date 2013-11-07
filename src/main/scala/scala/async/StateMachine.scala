/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async

/** Internal class used by the `async` macro; should not be manually extended by client code */
// NOTE: this is not in the `internal` package as we must keep this binary compatible as it extended
// by the translated code.
abstract class StateMachine[Result, EC] extends (scala.util.Try[Any] => Unit) with (() => Unit) {
  def result: Result

  def execContext: EC
}
