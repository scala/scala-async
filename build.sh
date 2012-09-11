#!/bin/bash
mkdir -p classes
scalac -d classes -deprecation -feature src/async/library/scala/async/Async.scala src/async/library/scala/async/AsyncUtils.scala
mkdir -p test-classes
#scalac -cp classes -d test-classes src/async/test/async-spec/Example.scala
scalac -cp classes -d test-classes src/async/test/async-spec/MinimalScalaTest.scala
scalac -cp classes:test-classes -d test-classes src/async/test/async-spec/AsyncSpec.scala
