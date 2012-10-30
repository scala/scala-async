#!/bin/bash
mkdir -p test-classes
scalac -cp classes -d test-classes src/async/test/async-spec/MinimalScalaTest.scala
scalac -cp classes:test-classes -d test-classes src/async/test/async-spec/AsyncSpec.scala
scala -cp test-classes scala.async.Test
