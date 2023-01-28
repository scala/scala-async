# scala-async [<img src="https://img.shields.io/maven-central/v/org.scala-lang.modules/scala-async_2.12.svg?label=latest%20release%20for%202.12">](http://search.maven.org/#search%7Cga%7C1%7Cg%3Aorg.scala-lang.modules%20a%3Ascala-async_2.12) [<img src="https://img.shields.io/maven-central/v/org.scala-lang.modules/scala-async_2.13.svg?label=latest%20release%20for%202.13">](http://search.maven.org/#search%7Cga%7C1%7Cg%3Aorg.scala-lang.modules%20a%3Ascala-async_2.13)

A Scala DSL to enable a direct style of coding when composing `Future`s.

## Usage

As of scala-async 1.0, Scala 2.12.12+ or 2.13.3+ are required.

### Add dependency

#### SBT Example

```scala
libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "1.0.1"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
```

For Maven projects add the following to your <dependencies> (make sure to use the correct Scala version suffix
to match your project’s Scala binary version):

#### Maven Example

```scala
<dependency>
  <groupId>org.scala-lang.modules</groupId>
  <artifactId>scala-async_2.13</artifactId>
  <version>1.0.1</version>
</dependency>
<dependency>
  <groupId>org.scala-lang</groupId>
  <artifactId>scala-reflect</artifactId>
  <version>2.13.8</version>
  <scope>provided</scope>
</dependency>
```

### Enable compiler support for `async`

Add the `-Xasync` to the Scala compiler options.

#### SBT Example

```scala
scalacOptions += "-Xasync"
```

#### Maven Example

```xml
<project>
  ...
  <plugin>
    <groupId>net.alchim31.maven</groupId>
    <artifactId>scala-maven-plugin</artifactId>
    <version>4.4.0</version>
    <configuration>
      <args>
        <arg>-Xasync</arg>
      </args>
    </configuration>
  </plugin>
  ...
</project>
```

### Start coding

```scala
import scala.concurrent.ExecutionContext.Implicits.global
import scala.async.Async.{async, await}

val future = async {
  val f1: Future[Boolean] = async { ...; true }
  val f2 = async { ...; 42 }
  if (await(f1)) await(f2) else 0
}
```

## What is `async`?

`async` marks a block of asynchronous code. Such a block usually contains
one or more `await` calls, which marks a point at which the computation
will be suspended until the awaited `Future` is complete.

By default, `async` blocks operate on `scala.concurrent.{Future, Promise}`.
The system can be adapted to alternative implementations of the
`Future` pattern.

Consider the following example:

```scala
def slowCalcFuture: Future[Int] = ...             // 01
def combined: Future[Int] = async {               // 02
  await(slowCalcFuture) + await(slowCalcFuture)   // 03
}
val x: Int = Await.result(combined, 10.seconds)   // 05
```

Line 1 defines an asynchronous method: it returns a `Future`.

Line 2 begins an `async` block. During compilation,
the contents of this block will be analyzed to identify
the `await` calls, and transformed into non-blocking
code.

Control flow will immediately pass to line 5, as the
computation in the `async` block is not executed
on the caller's thread.

Line 3 begins by triggering `slowCalcFuture`, and then
suspending until it has been calculated. Only after it
has finished, we trigger it again, and suspend again.
Finally, we add the results and complete `combined`, which
in turn will release line 5 (unless it had already timed out).

It is important to note that while lines 1-4 are non-blocking,
they are not parallel. If we wanted to parallelize the two computations,
we could rearrange the code as follows:

```scala
def combined: Future[Int] = async {
  val future1 = slowCalcFuture
  val future2 = slowCalcFuture
  await(future1) + await(future2)
}
```

## Limitations

### `await` must be directly in the control flow of the async expression

The `await` cannot be nested under a local method, object, class or lambda:

```
async {
  List(1).foreach { x => await(f(x) } // invalid
}
```

### `await` must be not be nested within `try` / `catch` / `finally`.

This implementation restriction may be lifted in future versions.

## Comparison with direct use of `Future` API

This computation could also be expressed by directly using the
higher-order functions of Futures:

```scala
def slowCalcFuture: Future[Int] = ...
val future1 = slowCalcFuture
val future2 = slowCalcFuture
def combined: Future[Int] = for {
  r1 <- future1
  r2 <- future2
} yield r1 + r2
```

The `async` approach has two advantages over the use of
`map` and `flatMap`:
  1. The code more directly reflects the programmer's intent,
     and does not require us to name the results `r1` and `r2`.
     This advantage is even more pronounced when we mix control
     structures in `async` blocks.
  2. `async` blocks are compiled to a single anonymous class,
     as opposed to a separate anonymous class for each closure
     required at each generator (`<-`) in the for-comprehension.
     This reduces the size of generated code, and can avoid boxing
     of intermediate results.
