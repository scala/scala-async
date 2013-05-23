package scala.async.iterators

import scala.util.{Try, Success, Failure}

trait IteratorState[T] {

  def apply(v: Try[Any]): Unit
  def apply: Unit
  def `result$async` : IteratorState[T]

  private[this] var _value: Option[T] = None
  private[this] var _exc: Throwable   = null

  def result_= (value: T) =
    _value = Some(value)

  def result: T = {
    if (_exc != null)
      throw _exc
    else
      _value.get
  }

  def exception_= (exc: Throwable) =
    _exc = exc

  def exception: Throwable =
    _exc

  def isFailed: Boolean =
    _exc != null

  def onComplete(cont: IteratorState[_]) = {
    // cont will always be `this`
    /* do nothing */
  }

  def next: T = {
    // continue iteration with next state
    this.apply(Success(result))
    // return current result
    result
  }

  def hasNext: Boolean = {
    println("invoking apply")
    apply
    _value.nonEmpty
  }
}
