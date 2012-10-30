#!/bin/bash
mkdir -p test-classes
scalac -cp classes -d test-classes test/files/run/block0/MinimalScalaTest.scala
scalac -cp classes:test-classes -d test-classes test/files/run/block0/AsyncSpec.scala
scala -cp test-classes Test
