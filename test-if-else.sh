#!/bin/bash
mkdir -p test-classes
scalac -cp classes -d test-classes src/async/test/async-spec/MinimalScalaTest.scala
scalac -Xprint:typer -cp classes:test-classes -d test-classes src/async/test/async-spec/if-else0.scala
scala -cp test-classes scala.async.Test
