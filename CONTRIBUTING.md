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

 - await calls are moved to only appear on the RHS of a value definition.
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
  class stateMachine$7 extends ... {
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