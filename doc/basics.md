A Parasitism task, an instance of `Task`, is similar to a Scala or Java
`Future`: it encapsulates a thunk of code which it starts executing
immediately, and once evaluated, holds the result. There are, however, a few
differences.

Tasks must be named, so a new task can be constructed with,
```scala
Task(t"do-something"):
  // code to run
```

This creates a new `Task[T]` where `T` is the return type of the task's body.

Tasks form a hierarchy, and a task spawned within the body of another task will
use the latter's context to determine its owner, effectively making the former
a child task. This has few implications for how the task runs, unless the
parent task is canceled, in which case all descendants will also be canceled.

### `Task` methods

A `Task` instance has several useful methods:
- `await()`, which blocks the current thread until the `Task` produces a value
- `await(timeout)`, which takes a `timeout`, after which a `TimeoutError` will be thrown if no value has been produced
- `map` and `flatMap`, providing standard monadic operations on the `Task`
- `name`, which returns the full name (a path) of the `Task`
- `cancel()`, which will stop the task running

### Platform or Virtual threading

From JDK 20 and above, threads may be either _platform_ (corresponding to
threads managed by the operating system) or _virtual_ (managed by the JVM).
Prior to JDK 20, all threads are _platform threads_. Parasitism needs to know
which type of thread to use, and this requires one of two imports:
- `parasitism.threading.virtual`
- `parasitism.threading.platform`

Note that choosing `threading.virtual` will result an a runtime error on JDKs
older than JDK 20.

### Cancelation

A task may be cancelled at any time, though cancellation is co-operative: it
requires the body of the task to be written to expect possible cancellation.
Within a task's body, the method `accede()` may be called multiple times. For
as long as the task is running normally, `accede()` will do nothing. But if a
task is cancelled, the task will stop immediately, without a value being
produced. Any `await()` calls on the task will throw a `CancelError`.

However, this happens _only_ when `accede()` is called, so if no such calls are
run as the task is executing, that task cannot be cancelled, and it must
execute to completion.

