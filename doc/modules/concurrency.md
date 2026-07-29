## Concurrency

### About

Concurrency in Soundness is structured. A task is spawned inside a supervised scope that owns it, and
the scope does not complete until its tasks have. A task runs a computation on a thread and yields a
result that is awaited; a failure in a task propagates to the scope rather than vanishing; and
cancelling a scope cancels everything beneath it. Whether tasks run on the JVM's platform threads or
its lightweight virtual threads is a choice made in scope. The reasoning behind this shape
is set out under [structured concurrency](../philosophy/structured-concurrency.md).

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

`sequence` runs its tasks in parallel and collects the results *in order*, so the ordering of the
results says nothing about the order in which they finished. `race` returns the first to finish
and the rest are cancelled, since their results were not wanted.

### Naming tasks

A task may be given a name, checked as the code compiles against the rules for a task name — no
path separators, since names compose into a hierarchy mirroring the scope tree:

```scala
val name: Name[Async] = n"worker"
```

Named tasks make a running program legible: a stack trace, a monitor dump or a debugger shows
`server/connection-4/reader` rather than an anonymous thread number.

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

A daemon's body is *hygienic*: it cannot capture an error handler from the code that spawned it.
That is deliberate. The spawning code has already moved on, so a handler there is not in any
meaningful sense enclosing the daemon, and delivering an error to it would mean resuming a
computation that has finished. A daemon that fails therefore escalates as a `Fault`, which a
program intercepts where it handles the unexpected, rather than silently reaching a handler that
was never intended for it.

### Cancellation in both directions

Cancelling a scope cancels its children, and the *probate* decides what happens to a child that is
still running when its parent's body completes. Under `cancelProbate` the child is cancelled;
under `awaitProbate` the parent waits for it.

Cancellation also runs upward: a task that fails propagates its failure to the scope that owns it,
which cancels the scope's other children. Work that has become pointless therefore stops, rather
than continuing to consume resources on behalf of a computation that has already failed.

### Threads and completion

The thread model is chosen by import: `virtualThreading` runs tasks on virtual threads, cheap enough
to spawn in great numbers, while `platformThreading` uses platform threads. A separate choice, the
*probate*, decides what a scope does with a child that has not finished when the scope ends —
`cancelProbate` cancels it, `awaitProbate` waits for it — so the policy for tidying up concurrent work
is explicit rather than assumed.

### Suspension as an effect

Waiting is not free, and it is not invisible. Awaiting a task or a promise, sleeping, and yielding
all *suspend* the running strand, and each demands a `Monitor` capability in scope. A method that
can block therefore says so in its signature, exactly as a method that can fail says so with
`raises`.

Underneath, the unit of execution is a `Strand` rather than a thread: an abstraction with the four
operations suspension needs — interrupt, join, park and unpark. A supervisor supplies strands, and
a supervisor is a value, so a scheduler that is not built on threads at all — an event loop over
WebAssembly's waitable sets, for instance — plugs in as a supervisor without any change to the
code that spawns and awaits.
