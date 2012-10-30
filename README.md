Scala Async Project
===================

Building
--------

The async macro can be built using the `build.sh` script.
It requires either Scala 2.10.0-RC1 or a nightly build of Scala 2.10.x.

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

```
SCALAC_OPTS='-cp classes'
JAVA_OPTS='-cp classes'
```

After this setup, we can run `partest` as follows:

```
$ ../scala/test/partest --classpath ../scala/build/quick/classes --run
```

If you are interested in contributing code, we ask you to complete and submit
to us the Scala Contributor License Agreement, which allows us to ensure that
all code submitted to the project is unencumbered by copyrights or patents.
The form is available at:
http://www.scala-lang.org/sites/default/files/contributor_agreement.pdf

Before submitting a pull-request, please make sure you have followed the guidelines
outlined in our [Pull Request Policy](https://github.com/scala/scala/wiki/Pull-Request-Policy).
