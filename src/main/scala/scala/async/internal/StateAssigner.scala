/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async.internal

private[async] final class StateAssigner {
  private var current = -1

  def nextState(): Int = {
    current += 1
    current
  }
}
