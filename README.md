# Scala Async Project

[![Build Status](https://secure.travis-ci.org/scala/async.png)](http://travis-ci.org/scala/async)

Note: this branch targets Scala 2.11.x, support for Scala 2.10.x has been moved to [this branch](https://github.com/scala/async/tree/2.10.x).

## Quick start

Add a dependency:

```scala
// SBT
libraryDependencies += "org.scala-lang.modules" %% "scala-async" % "0.9.0"
```

Write your first `async` block:

```scala
import ExecutionContext.Implicits.global
import scala.async.Async.{async, await}

val future = async {
  val f1 = async { ...; true }
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

Lines 1 defines an asynchronous method: it returns a `Future`.

Line 2 begins an `async` block. During compilation,
the contents of this block will be analyzed to identify
the `await` calls, and transformed into non-blocking
code.

Control flow will immediately pass to line 5, as the
computation in the `async` block is not executed
on the caller's thread.

Line 3 begins by triggering `slowCalcFuture`, and then
suspending until it has been calculating. Only after it
has finished, we trigger it again, and suspend again.
Finally, we add the results and complete `combined`, which
in turn will release line 5 (unless it had already timed out).

It is important to note that while line 1-4 is non-blocking,
it is not parallel. If we wanted to parallelize the two computations,
we could rearrange the code as follows.

```scala
def combined: Future[Int] = async {
  val future1 = slowCalcFuture
  val future2 = slowCalcFuture
  await(future1) + await(future2)
}
```

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
`map` and `flatMap`.
  1. The code more directly reflects the programmers intent,
     and does not require us to name the results `r1` and `r2`.
     This advantage is even more pronounced when we mix control
     structures in `async` blocks.
  2. `async` blocks are compiled to a single anonymous class,
     as opposed to a separate anonymous class for each closure
     required at each generator (`<-`) in the for-comprehension.
     This reduces the size of generated code, and can avoid boxing
     of intermediate results.

## Comparison with CPS plugin

The existing continuations (CPS) plugin for Scala can also be used
to provide a syntactic layer like `async`. This approach has been
used in Akka's [Dataflow Concurrency](http://doc.akka.io/docs/akka/snapshot/scala/dataflow.html)

CPS-based rewriting of asynchronous code also produces a closure
for each suspension. It can also lead to type errors that are
difficult to understand.

## How it works

 - The async macro analyses the block of code, looking for control
   structures and locations of await calls. It then breaks the code
   into 'chunks'. Each chunk contains a linear sequence of statements
   that concludes with a branching decision, or with the registration
   of a subsequent state handler as the continuation.
 - Before this analysis and transformation, the program is normalized
   into a form amenable to this manipulation. This is called the
   "A Normal Form" (ANF), and roughly means that:
     - `if` and `match` constructs are only used as statements;
       they cannot be used as an expression.
     - calls to await are not allowed in compound expressions.
 - Identify vals, vars and defs that are accessed from multiple
   states. These will be lifted out to fields in the state machine
   object.
 - Synthesize a class that holds:
   - an integer representing the current state ID
   - the lifted definitions
   - an `apply(value: Try[Any]): Unit` method that will be
     called on completion of each future. The behavior of
     this method is determined by the current state. It records
     the downcast result of the future in a field, and calls the
     `resume()` method.
   - the `resume(): Unit` method that switches on the current state
     and runs the users code for one 'chunk', and either:
       a) registers the state machine as the handler for the next future
       b) completes the result Promise of the async block, if at the terminal state.
   - an `apply(): Unit` method that starts the computation.

## Limitations
 - See the [neg](https://github.com/scala/async/tree/master/src/test/scala/scala/async/neg) test cases for
   for constructs that are not allowed in a async block
 - See the [issue list](https://github.com/scala/async/issues?state=open) for which of these restrictions are planned
   to be dropped in the next milestone.
 - See [#13](https://github.com/scala/async/issues/13) for why `await` is not possible in closures, and for suggestions on
   ways to structure the code to work around this limitation.
