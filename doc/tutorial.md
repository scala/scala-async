# Async Tutorial

By Philipp Haller

## Preliminaries

Async is a macro library for Scala 2.10.1 or higher. (Since it's
slated for inclusion in the Scala 2.11 distribution there will be
binary artifacts for all upcoming Scala 2.11 releases.) To start using
Async, it suffices to include a compatible binary jar on the
classpath.

To use Async with the default binding for Scala's futures, the members
`async` and `await` of the `scala.async.Async` object have to be
imported. (In a future stable release, these members might be moved
to the `scala.async` package object.) A language import to enable
macros is not required for users of async/await.

## The `async` Construct

The `async` construct has the following signature:

    def async[T](body: => T): Future[T]

As can be seen from its type, it creates a future; `body` is a by-name
parameter which means that it is evaluated asynchronously as the body
of the future. Calling `async` requires having an implicit
`ExecutionContext` in scope on which the new future is scheduled for
execution. If there is no such `ExecutionContext` in scope,
compilation fails. (Similar to how `future { ... }` works.)

The body of `async` may contain calls to `await` which is explained next.

## The `await` Construct

Calling `await` inside the body of an `async` block suspends the
evaluation of the `async` block until a given future is completed
(successfully or unsuccessfully). Its signature is as follows:

    def await[T](fut: Future[T]): T

Even though the signature and semantics of `await` are very similar to
typical blocking calls, calls to `await` are translated to efficient
non-blocking code under the hood. Example:

    01:  val fut: Future[String] = ...
    02:  
    03:  val messageFut = async {
    04:    // potentially expensive computation
    05:    val user = ...
    06:    // to continue we need the result of `fut`:
    07:    val res = await(fut)
    08:    val (month, day) = res
    09:    s"Hello $user, it's $month!"
    10:  }

In the above example, the evaluation of the body of `async` suspends
at line 7 until the future `fut` is completed. There are several
possible outcomes: either `fut` is completed successfully in which case
the evaluation resumes on line 8 such that `res` is bound to the
successful result. Another possibility is that `fut` is completed
with an exception. In that case, `messageFut` is immediately completed
with the same exception. (A timeout is handled like an unsuccessful
completion with a `TimeoutException`.)

## Illegal Uses of `await`

The `await` construct can be used within most of Scala's standard
control structures such as if-else and match expressions. However,
certain uses of `await` are illegal, since they make it impossible to
generate the state machine of the `async` block. This section explains
where it is illegal to use `await` and what options exist to work
around issues that arise from those restrictions.

The most important limitation of `await` is that it cannot be called
from within closures inside an `async` block. This means an `await`
can only occur within a directly enclosing `async` block. Sometimes
this can be hard to avoid, for example, when calling higher-order
functions on collections. Here is an example:

    async {
      val list = ...

      list map { idx =>
        // need to suspend
        val res = await(someFut(idx))

        transform(res)
      }
    }

The above example will fail to compile since `await` is called inside
the closure passed to `map`. In situations like this it can be useful
to wrap (parts of) the closure's body in a nested `async` block. Of
course, this changes the result type of the `map` call: we're now
dealing with a collection of futures instead of a collection of strict
results. Fortunately, the futures API provides a few utilities for
working with collections of futures. In this case, the `sequence`
combinator, which converts a collection with elements of type `Future[T]` into a future of a
collection of elements of type `T`, is most useful:

    async {
      val list = ...

      val colFuts = list map { idx =>
        async {
          // need to suspend
          val res = await(someFut(idx))

          transform(res)
        }
      }

      await(Future.sequence(colFuts))
    }

Besides closures, there are other points in a program where `await` cannot be used. For example, if the `async` block contains a nested class, trait, or object, it is illegal to call `await` inside of it without an `async` block that directly encloses the `await` call.

Similar to closures, it is illegal to use `await` within an argument of a by-name parameter. In some cases a work-around is to implement the method with the by-name parameter as a macro, so that the method body is inlined. In many cases that's enough to avoid any further issues. However, it is only recommended for advanced users, since the solution relies on using macros.

## Debugging

The Async distribution contains utilities that simplify debugging
`async` blocks. By importing the `async` and `await` members from the
`scala.async.BlockingAsync` object instead of from the
`scala.async.Async` object, a different evaluation strategy for
`async` blocks is selected. The "blocking" strategy ensures that each
`async` block is executed on a single thread in such a way that
execution never leaves that thread. As a result, regular debuggers can
be used to step through the code of an `async` block without losing
the thread's stack. To make this possible, in this mode all calls to
`await` are blocking: the current thread is blocked until the
corresponding future is completed in which case the thread is
unblocked to resume its computation.

