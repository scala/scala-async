/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async.internal

private[async] final class StateAssigner {
  private var current = StateAssigner.Initial

  def nextState(): Int = try current finally current += 1
}

object StateAssigner {
  final val Initial = 0
}