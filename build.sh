#!/bin/bash
scalac -version
mkdir -p classes
scalac -d classes -deprecation -feature src/async/library/scala/async/Async.scala src/async/library/scala/async/AsyncUtils.scala
