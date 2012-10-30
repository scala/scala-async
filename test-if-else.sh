#!/bin/bash
mkdir -p test-classes
scalac -cp classes -d test-classes test/files/run/if-else0/MinimalScalaTest.scala
scalac -cp classes:test-classes -d test-classes test/files/run/if-else0/if-else0.scala
scala -cp test-classes Test
