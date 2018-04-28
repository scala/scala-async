/*
 * Copyright (C) 2018 Lightbend Inc. <http://www.lightbend.com>
 */
package scala.async.internal

import java.util
import java.util.function.{Consumer, IntConsumer}

import scala.collection.JavaConverters.{asScalaIteratorConverter, iterableAsScalaIterableConverter}
import scala.collection.mutable

// Set for StateIds, which are either small positive integers or -symbolID.
final class StateSet {
  private var bitSet = new java.util.BitSet()
  private var caseSet = new util.HashSet[Integer]()
  def +=(stateId: Int): Unit = if (stateId > 0) bitSet.set(stateId) else caseSet.add(stateId)
  def contains(stateId: Int): Boolean = if (stateId > 0 && stateId < 1024) bitSet.get(stateId) else caseSet.contains(stateId)
  def iterator: Iterator[Integer] = {
    bitSet.stream().iterator().asScala ++ caseSet.asScala.iterator
  }
  def foreach(f: IntConsumer): Unit = {
    bitSet.stream().forEach(f)
    caseSet.stream().forEach(new Consumer[Integer] {
      override def accept(value: Integer): Unit = f.accept(value)
    })
  }
}
