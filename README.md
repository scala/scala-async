# Scala Async Project

## Quick start

 - Add `scala-async.jar` to your classpath
 - Use Scala 2.10.0

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

Line 3 begins an `async` block. During compilation,
the contents of this block will be analyzed to identify
the `await` calls, and transformed into non-blocking
code.

Control flow will immediately pass to line 5, as the
computation in the `async` block is not executed
on the caller's thread.

Line 4 begins by triggering `slowCalcFuture`, and then
suspending until it has been calculating. Only after it
has finished, we trigger it again, and suspend again.
Finally, we add the results and complete `combined`, which
in turn will release line 5 (unless it had already timed out).

It is important to note that while this code is non-blocking,
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

## Troubleshooting
 - Logging of the transform can be enabled with `scalac -Dscala.async.debug=true`.
 - Tracing of the ANF transform: `scalac -Dscala.async.trace=true`
 - Debug the macro expansion by checking out the project and executing the application
   [`TreeInterrogation`](https://github.com/phaller/scala-async/blob/master/src/test/scala/scala/async/TreeInterrogation.scala#L59)

## Limitations
 - See the [neg](https://github.com/phaller/scala-async/tree/master/src/test/scala/scala/async/neg) test cases for
   for constructs that are not allowed in a async block
 - See the [issue list](https://github.com/phaller/scala-async/issues?state=open) for which of these restrictions are planned
   to be dropped in the next milestone.
 - See [#13](https://github.com/phaller/scala-async/issues/13) for why `await` is not possible in closures, and for suggestions on
   ways to structure the code to work around this limitation.

## Building

The async macro and its test suite can be built and run with SBT.

## Contributing

If you are interested in contributing code, we ask you to complete and submit
to us the Scala Contributor License Agreement, which allows us to ensure that
all code submitted to the project is unencumbered by copyrights or patents.
The form is available at:
http://www.scala-lang.org/sites/default/files/contributor_agreement.pdf

Before submitting a pull-request, please make sure you have followed the guidelines
outlined in our [Pull Request Policy](https://github.com/scala/scala/wiki/Pull-Request-Policy).

### Generated Code examples

```scala
val future = async {                                     
 val f1 = async { true }                                 
 val x = 1                                               
 def inc(t: Int) = t + x                                 
 val t = 0                                               
 val f2 = async { 42 }                                   
 if (await(f1)) await(f2) else { val z = 1; inc(t + z) }
}                                                                            
```

After ANF transform.

 - await calls are moved to only appear on the LHS of a value definition.
 - `if` is not used as an expression, instead each branch writes its result
   to a synthetic `var`.

```scala
 {
  ();
  val f1: scala.concurrent.Future[Boolean] = {
    scala.concurrent.Future.apply[Boolean](true)(scala.concurrent.ExecutionContext.Implicits.global)
  };
  val x: Int = 1;
  def inc(t: Int): Int = t.+(x);
  val t: Int = 0;
  val f2: scala.concurrent.Future[Int] = {
    scala.concurrent.Future.apply[Int](42)(scala.concurrent.ExecutionContext.Implicits.global)
  };
  val await$1: Boolean = scala.async.Async.await[Boolean](f1);
  var ifres$1: Int = 0;
  if (await$1)
    {
      val await$2: Int = scala.async.Async.await[Int](f2);
      ifres$1 = await$2
    }
  else
    {
      ifres$1 = {
        val z: Int = 1;
        inc(t.+(z))
      }
    };
  ifres$1
}
```

After async transform:

 - one class synthesized to act as the state machine. It's `apply()` method will
   be used to start the computation (even the code before the first await call
   is executed asynchronously), and the `apply(tr: scala.util.Try[Any])` method
   will continue after each completed background task.
 - each chunk of code moved into the a branch of the pattern match in `resume$async`.
 - value and method definitions accessed from multiple states are lifted to be
   members of `class stateMachine`. Others remain local, e.g. `val z`.

```scala
 {
  class stateMachine$7 extends StateMachine[scala.concurrent.Promise[Int], scala.concurrent.ExecutionContext] {
    def <init>() = {
      super.<init>();
      ()
    };
    var state$async: Int = 0;
    val result$async: scala.concurrent.Promise[Int] = scala.concurrent.Promise.apply[Int]();
    val execContext$async = scala.concurrent.ExecutionContext.Implicits.global;
    var x$1: Int = 0;
    def inc$1(t: Int): Int = t.$plus(x$1);
    var t$1: Int = 0;
    var f2$1: scala.concurrent.Future[Int] = null;
    var await$1: Boolean = false;
    var ifres$1: Int = 0;
    var await$2: Int = 0;
    def resume$async(): Unit = try {
      state$async match {
        case 0 => {
          ();
          val f1 = {
            scala.concurrent.Future.apply[Boolean](true)(scala.concurrent.ExecutionContext.Implicits.global)
          };
          x$1 = 1;
          t$1 = 0;
          f2$1 = {
            scala.concurrent.Future.apply[Int](42)(scala.concurrent.ExecutionContext.Implicits.global)
          };
          f1.onComplete(this)(execContext$async)
        }
        case 1 => {
          ifres$1 = 0;
          if (await$1)
            {
              state$async = 2;
              resume$async()
            }
          else
            {
              state$async = 3;
              resume$async()
            }
        }
        case 2 => {
          f2$1.onComplete(this)(execContext$async);
          ()
        }
        case 5 => {
          ifres$1 = await$2;
          state$async = 4;
          resume$async()
        }
        case 3 => {
          ifres$1 = {
            val z = 1;
            inc$1(t$1.$plus(z))
          };
          state$async = 4;
          resume$async()
        }
        case 4 => {
          result$async.complete(scala.util.Success.apply(ifres$1));
          ()
        }
      }
    } catch {
      case NonFatal((tr @ _)) => {
        {
          result$async.complete(scala.util.Failure.apply(tr));
          ()
        };
        ()
      }
    };
    def apply(tr: scala.util.Try[Any]): Unit = state$async match {
      case 0 => {
        if (tr.isFailure)
          {
            result$async.complete(tr.asInstanceOf[scala.util.Try[Int]]);
            ()
          }
        else
          {
            await$1 = tr.get.asInstanceOf[Boolean];
            state$async = 1;
            resume$async()
          };
        ()
      }
      case 2 => {
        if (tr.isFailure)
          {
            result$async.complete(tr.asInstanceOf[scala.util.Try[Int]]);
            ()
          }
        else
          {
            await$2 = tr.get.asInstanceOf[Int];
            state$async = 5;
            resume$async()
          };
        ()
      }
    };
    def apply: Unit = resume$async()
  };
  val stateMachine$7: StateMachine[scala.concurrent.Promise[Int], scala.concurrent.ExecutionContext] = new stateMachine$7();
  scala.concurrent.Future.apply(stateMachine$7.apply())(scala.concurrent.ExecutionContext.Implicits.global);
  stateMachine$7.result$async.future
}
```
