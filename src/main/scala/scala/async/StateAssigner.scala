package scala.async

private[async] final class StateAssigner {
  private var current = -1

  def nextState(): Int = {
    current += 1
    current
  }
}