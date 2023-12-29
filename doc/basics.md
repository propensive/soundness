All Parasite terms and types are defined in the `parasite` package:
```scala
import parasite.*
```

A Parasite task, an instance of `Async`, is similar to a Scala or Java
`Future`: it encapsulates a block of code which it starts executing
immediately, and once evaluated, holds the result. There are, however, a few
differences.

We can create one inside a `supervise` block with:
```scala
import perforate.errorHandlers.throwUnsafely

def slowTask(): Unit = ???

def run() = supervise:
  val async = Async:
    slowTask()
```

This creates a new `Async[ResultType]` where `ResultType` is the return type of
`someSlowTask()`.

This will create *and start* the new task in a thread forked from the current
thread. Execution of `someSlowTask()` will proceed in a forked thread until it
completes.

We can call `async.await()` in the current thread to wait until the forked
thread finishes, and return the value which results from its execution.



Asynchronous tasks form a hierarchy, and a task spawned within the body of
another task will use the latter's context to determine its owner, effectively
making the former a child task. This has few implications for how the task
runs, unless the parent task is canceled, in which case all descendants will
also be canceled.

### `Async` methods

An `Async` instance has several useful methods:
- `await()`, which blocks the current thread until the `Async` produces a value
- `await(timeout)`, which takes a `timeout`, after which a `TimeoutError` will
  be thrown if no value has been produced
- `map` and `flatMap`, providing standard monadic operations on the `Async`
- `cancel()`, which will stop the task running

### Platform or Virtual threading

From JDK 20 and above, threads may be either _platform_ (corresponding to
threads managed by the operating system) or _virtual_ (managed by the JVM).
Prior to JDK 20, all threads are _platform threads_. Parasite needs to know
which type of thread to use, and this requires one of two imports:
- `parasite.threading.virtual`
- `parasite.threading.platform`

Note that choosing `threading.virtual` will result an a runtime error on JDKs
older than JDK 20.

### Cancelation

A task may be cancelled at any time, though cancellation is co-operative: it
requires the body of the task to be written to expect possible cancellation.
Within a task's body, the method `acquiesce()` may be called multiple times. For
as long as the task is running normally, `acquiesce()` will do nothing. But if a
task is cancelled, the task will stop immediately, without a value being
produced. Any `await()` calls on the task will throw a `CancelError`.

However, this happens _only_ when `acquiesce()` is called, so if no such calls
are run as the task is executing, that task cannot be cancelled, and it must
execute to completion.

