## Concurrency

### About

Concurrency in Soundness is structured. A task is spawned inside a supervised scope that owns it, and
the scope does not complete until its tasks have. A task runs a computation on a thread and yields a
result that is awaited; a failure in a task propagates to the scope rather than vanishing; and
cancelling a scope cancels everything beneath it. Whether tasks run on the JVM's platform threads or
its lightweight virtual threads is a choice made in scope.

### On concurrency

Unstructured concurrency leaks. A thread started with no owner can outlive the code that started it;
an exception thrown on it disappears unless someone thought to catch it; and cancelling a piece of
work means tracking down every thread it spawned. The lifetimes of concurrent tasks bear no relation
to the structure of the code, so reasoning about what is still running, and what happened to it, is
hard.

Soundness gives concurrent work the same shape as ordinary blocks. Every task is a child of the scope
that spawned it, and a scope waits for its children before it finishes, so no task outlives its
scope. A failure travels up to the scope, and cancelling the scope cancels its children. The result
is that a concurrent program nests, and its structure is visible. Everything comes from the
`soundness` package, with a thread model and a completion policy in scope:

```scala
import soundness.*
import threading.virtualThreading
import probates.cancelProbate
import strategies.throwUnsafely
```

### A supervised scope

`supervise` opens a scope that owns the tasks spawned within it. It does not return until those tasks
have settled, so concurrent work has a clear boundary:

```scala
supervise:
  // tasks spawned here are owned by this scope
```

### Spawning a task

`async` spawns a task, which runs concurrently and yields its result from `await`:

```scala
supervise:
  val task = async(expensiveComputation())
  task.await()
```

Because the task belongs to the enclosing scope, the scope accounts for it whether or not it is
explicitly awaited.

### Combining tasks

Tasks compose. A collection of tasks awaits together with `sequence`, the first to finish is taken
with `race`, and `map` and `bind` derive one task from another:

```scala
supervise:
  Seq(async(1), async(2), async(3)).sequence.await()   // Seq(1, 2, 3)
  async(3).bind(n => async(n + 4)).await()             // 7
```

### Cancellation

A task is cancelled with `cancel`, and a cancellable task cooperates by pausing at points where it
can be interrupted. A `snooze` is such a point, so a task sleeping on one wakes to its cancellation
rather than running on:

```scala
supervise:
  val task = async:
    snooze(10.0*Second)
    compute()
  task.cancel()   // the snoozing task is interrupted
```

### Promises

A `Promise` is a value that will be supplied later, perhaps by another thread. One side awaits it and
the other fulfils it, which is how work running elsewhere hands back a result:

```scala
supervise:
  val promise = Promise[Int]()
  daemon(promise.fulfill(7))
  promise.await()   // 7
```

### Daemons

A `daemon` is fire-and-forget work — a background loop, a listener — that the scope does not wait for
and cancels when it ends. It runs for the life of its scope and no longer.

### Threads and completion

The thread model is chosen by import: `virtualThreading` runs tasks on virtual threads, cheap enough
to spawn in great numbers, while `platformThreading` uses platform threads. A separate choice, the
*probate*, decides what a scope does with a child that has not finished when the scope ends —
`cancelProbate` cancels it, `awaitProbate` waits for it — so the policy for tidying up concurrent work
is explicit rather than assumed.
