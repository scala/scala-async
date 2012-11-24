/*
 * Copyright (C) 2012 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async

private[async] final class StateAssigner {
  private var current = -1

  def nextState(): Int = {
    current += 1
    current
  }
}