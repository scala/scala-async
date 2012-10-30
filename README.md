Scala Async Project
===================

Building
--------

The async macro can be built using the `build.sh` script.

Running the test suite
----------------------

Currently, the tests can be run using `partest` the testing tool used
to test the Scala compiler and standard library. At the moment,
running `partest` requires a working copy of the Scala compiler.

In the following it is assumed that the build of the Scala compiler is
located at `../scala` (root of the "scala" project when cloned using
git) relative to the root directory of the async project.

Moreover, in the Scala build it's necessary to copy the directory
"build/asm/classes/scala/tools/asm" into
"build/quick/classes/compiler/scala/tools".

Finally, it's necessary to set the following environment variables:

{{{
SCALAC_OPTS='-cp classes'
JAVA_OPTS='-cp classes'
}}}

After this setup, we can run `partest` as follows:

{{{
$ ../scala/test/partest --classpath ../scala/build/quick/classes --run
}}}

