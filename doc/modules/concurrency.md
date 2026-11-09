## Concurrency

### About

Concurrency is structured. A task is spawned inside a supervised scope, which owns it, and
the scope does not complete until its tasks have:

- A task runs a computation on a thread, and gives a result, which is awaited.
- A failure in a task propagates to the scope. It does not vanish.
- Canceling a scope cancels everything beneath it.

Whether tasks run on the JVM's platform threads, or on its lightweight virtual threads, is a
choice which is made in scope.
[Structured concurrency](../philosophy/structured-concurrency.md) sets out the reasoning
behind this shape.

### On concurrency

Unstructured concurrency leaks. A thread which is started with no owner can outlive the code
which started it. An exception which is thrown on it disappears, unless someone thought to
catch it. Canceling a piece of work means tracking down every thread which it spawned. The
lifetimes of concurrent tasks bear no relation to the structure of the code, so it is hard
to reason about what is still running, and what happened to it.

Here concurrent work has the same shape as ordinary blocks. Every task is a child of the
scope which spawned it, and a scope waits for its children before it finishes, so no task
outlives its scope. A failure travels up to the scope, and canceling the scope cancels its
children. A concurrent program therefore nests, and its structure is visible.

Everything comes from the `soundness` package. A thread model, and a policy for unfinished
tasks, are in scope, and the last import lets the samples print their results:

```scala
import soundness.*
import threads.virtualThreads
import probates.cancelProbate
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### A supervised scope

`supervise` opens a scope, which owns the tasks that are spawned within it. It does not
return until those tasks have settled, so concurrent work has a clear boundary. Outside one,
no task can be spawned:

```scala fuel
compile
errors show
##
async(1 + 1)
```

### Spawning a task

`async` spawns a task, which runs concurrently. `await` waits for the task, and gives its
result:

```scala
def expensiveComputation(): Int = (1 to 1000000).sum

val sum = supervise:
  val task = async(expensiveComputation())
  task.await()
```

```scala fuel
run
##
Out.println(sum.show)
```

The task belongs to the enclosing scope, so the scope accounts for it, whether or not it is
explicitly awaited.

Parenthood travels through a `Monitor`. Both `supervise` and `async` introduce one into
their bodies, but `async` also *consumes* one, and `supervise` does not. That is what makes
`supervise` the root of a hierarchy, and `async` a branch beneath it, and it is why every
`async` must ultimately stem from a `supervise`. Where several monitors are in scope, the
innermost wins, so a task which is nested three deep is a child of the task which
immediately encloses it, not of the scope at the top.

### Why a parent waits

A task may not finish, whether by producing a result or by being canceled, until its
children have finished. That is what makes a completed task safe to reason about. Consider a
parent which returns almost immediately, while a child goes on counting. What becomes of the
child is decided by the [probate](#cancellation-in-both-directions) in scope, and under
`awaitProbate` the parent waits for it:

```scala
var written = 0

def outcome(): Text =
  import probates.awaitProbate

  supervise:
    val parent = async:
      async:
        for i <- 0 to 10 do
          snooze(0.01*Second)
          written += 1

      t"complete"

    parent.await()
```

Nothing stands between the parent and its result. Were `await` to return at once, `written`
would be read while the child was still counting. Instead, `parent.await()` does not return
until the child has settled. Under `awaitProbate` that means all eleven increments have been
made. Under `cancelProbate`, the child is canceled instead, and the parent still does not
finish until that has happened.

That is the invariant in general. Once a task has been awaited, no further execution can
take place on the resources which were captured in its body, including any which escaped
through a child.

### Combining tasks

Tasks compose. A collection of tasks is awaited together with `sequence`. The first to
finish is taken with `race`. `map` and `bind` derive one task from another:

```scala
val together = supervise(List(async(1), async(2), async(3)).sequence.await())
val chained = supervise(async(3).bind(n => async(n + 4)).await())
```

```scala fuel
run
##
Out.println(together.inspect)
Out.println(chained.show)
```

`sequence` runs its tasks in parallel, and collects the results *in order*, so the order of
the results says nothing about the order in which the tasks finished. `race` gives the
result of the first to finish, and the rest are canceled, since their results were not
wanted.

### Bounded parallelism

A task is a thread, and starting one costs a few microseconds, so a thousand cheap jobs
should not become a thousand tasks. `concurrently` runs numbered jobs across a fixed number
of tasks. Each task takes the next job from a shared counter, and the results are returned
in the order of the jobs, once every task has finished:

```scala
val squares = supervise(concurrently(1000, 8)(i => i*i))
```

```scala fuel
run
##
Out.println(squares.to[List].excerpt(0, 5).inspect)
```

The cost of spawning is paid eight times, not a thousand. No job runs before an earlier one
has been claimed, so the load balances itself: a slow job simply holds its task, while the
others continue through the rest. An exception in a job fails the task which it ran on, and
is thrown again when the tasks are joined.

### Naming tasks

`task` is `async` with a name. The name is checked at [compiletime] against the rules for a
task's [name](names.md). It has no path separators, since names compose into a hierarchy
which mirrors the tree of scopes. Inside the task, `monitor.name` is the task's own name,
and `monitor.stack` is the full path:

```scala
import Async.nominative

val worker = supervise(task(n"worker")(monitor.name).await())
```

```scala fuel
run
##
Out.println(worker.inspect)
```

Named tasks make a running program legible. A [stack trace](stack-traces.md), a dump of the
monitors or a [debugger](debugging.md) shows `server/connection-4/reader`, not an anonymous
thread number.

### Cancellation

A task is canceled with `cancel`. A task which can be canceled cooperates, by pausing at
points where it can be interrupted. A `snooze` is such a point, so a task which is sleeping
in one wakes to its cancellation, and does not run on:

```scala
val interrupted = supervise:
  val task = async:
    snooze(10.0*Second)
    expensiveComputation()

  task.cancel()
  t"canceled without waiting ten seconds"
```

```scala fuel
run
##
Out.println(interrupted)
```

Work which does not pause has to volunteer its own cancellation points. `relent()` is such a
point. While the task is running normally, it does nothing. If the task has been canceled,
it stops there, without producing a value. A task whose body never calls `relent()`, and
never pauses, cannot be canceled at all, and must run to completion.

```scala fuel
typecheck
##
supervise:
  val task = async:
    for i <- 0 to 10 do
      sleep(0.1*Second)
      relent()
      written += 1

  task.cancel()
```

A task which is canceled at the `relent()` never reaches the increment on the next line. It
does still wait out the pause first, because `sleep` cannot be interrupted, which is the
distinction that the next section is about.

### Pausing

Two methods stop the current task temporarily. They differ in one way: whether the pause can
end early, because the task was canceled.

```tabular
column  Method
column  When the task is canceled during the pause
row  `snooze`  the task wakes at once: it is a cancellation point
row  `sleep`  the pause continues for the time which remains, so it always lasts its full length
```

The names are chosen to be remembered, not looked up. A *snooze* is the button on an alarm
clock, and can be cut short by a second alarm, while a *sleep* is slept through.

Each takes either a *duration* or an *instant*, and the type of the argument says which is
meant. A duration pauses for that long, and an instant pauses until it arrives:

```scala fuel
typecheck
##
supervise:
  snooze(2.0*Second)
  snooze(now() + 2.0*Second)
  sleep(2.0*Second)
  sleep(now() + 2.0*Second)
```

The bound which both share is `Schedulable`, which any type that is
`Abstractable across Durations` or `Abstractable across Instants` satisfies. A
[quantity](quantities.md) of time, a [`Timespan`](time.md#durations-and-timespans) and an
[`Instant`](time.md#instants) are therefore all accepted without conversion.

#### Units

Both take a *typed* duration or instant, such as `0.2*Second` or `10*Minute`, and a typed
value cannot be misread. A bare `Long` can, so nothing interprets one until a unit has been
chosen by name:

```scala fuel
compile
errors show
##
supervise:
  snooze(200L)
```

The package `abstractables` has four readings of a `Long`:

```tabular
column  Given
column  Reads a `Long` as
row  `nanosecondsAbstractable`  a duration in nanoseconds
row  `microsecondsAbstractable`  a duration in microseconds
row  `millisecondsAbstractable`  a duration in milliseconds
row  `epochMillisecondsAbstractable`  an instant, in milliseconds since the epoch
```

```scala
import abstractables.millisecondsAbstractable
```

```scala fuel
run
##
supervise:
  snooze(200L)
  Out.println(t"paused for 200 milliseconds")
```

The three readings as a duration share a type, so exactly one of them may be imported into
a file. The package `instantiables` mirrors all four, for the opposite direction. A reading
as a duration and the reading as an instant may not both be in scope where a bare `Long` is
passed, because nothing at the call could then say which was meant, and the two are reported
as ambiguous.

The distinction matters, because the underlying representation of a duration is
*nanoseconds*. An unqualified `snooze(200L)`, read in nanoseconds, returns almost
immediately, and a loop around it is a busy loop, with nothing at the call to say so.

### Retrying

Work which fails for a transient reason should be tried again. The *schedule* on which it is
retried is a value, not a loop written at each use. `retry` runs a block under the
`Tenacity` in scope, which decides how long to wait before each attempt, and when to stop.
The block is lent two operations: `persevere()` asks for another attempt, and `surrender()`
gives up at once.

```scala
import retryTenacities.exponentialTenTimesTenacity

var attempts = 0

def fetchRemoteValue(): Optional[Text] =
  attempts += 1
  if attempts < 3 then Unset else t"payload"

val fetched = supervise:
  retry: (surrender, persevere) ?=>
    fetchRemoteValue().or(persevere())
```

```scala fuel
run
##
Out.println(fetched)
Out.println(attempts.show)
```

The schedules which are provided, in the package `retryTenacities`, cover the usual choices:

```tabular
column  Given
column  Delay before a retry
column  Attempts
row  `exponentialForeverTenacity`  10ms, growing by a factor of 1.2 each time  unlimited
row  `exponentialFiveTimesTenacity`  10ms, growing by a factor of 1.2 each time  5
row  `exponentialTenTimesTenacity`  10ms, growing by a factor of 1.2 each time  10
row  `fixedNoDelayForeverTenacity`  none  unlimited
row  `fixedNoDelayFiveTimesTenacity`  none  5
row  `fixedNoDelayTenTimesTenacity`  none  10
```

`Tenacity.exponential` and `Tenacity.fixed`, with `limit`, build others. Running out of
attempts raises a `Tenacity.Error`, which names how many were made.

A body therefore tells a failure which is worth retrying from one which never will be, and
the schedule decides the rest.

### Promises

A `Promise` is a value which will be supplied later, perhaps by another thread. One side
awaits it, and the other fulfills it, which is how work which runs elsewhere hands back a
result:

```scala
val promised = supervise:
  val promise = Promise[Int]()
  daemon(promise.fulfill(7))
  promise.await()
```

```scala fuel
run
##
Out.println(promised.show)
```

### Daemons

A `daemon` is work which is started and then left: a background loop, or a listener. The
scope does not wait for it, and cancels it when the scope ends. It runs for the life of its
scope, and no longer.

A daemon's body is *hygienic*: it cannot capture an error handler from the code which
spawned it. That is deliberate. The spawning code has already moved on, so a handler there
does not, in any meaningful sense, enclose the daemon, and delivering an error to it would
mean resuming a computation which has finished. A daemon which fails therefore escalates as
a `Fault`, which a program intercepts where it handles the unexpected. It never silently
reaches a handler which was not intended for it.

A long-lived service is a different thing, which [Daemons](daemons.md) describes.

### Timeouts

An `await` may be given a duration, after which it raises an `Async.Error` instead of
waiting on. A task which has taken too long is thereby abandoned, at a point which the code
chooses:

```scala
def abandoned(): Optional[Unit] = supervise:
  val slow = async(sleep(10.0*Second))
  safely(slow.await(0.1*Second))
```

`safely` turns the error into `Unset`, so the caller sees an absent result when the deadline
passes before the task finishes.

### Cancellation in both directions

Canceling a scope cancels its children. The *probate* decides what happens to a child which
is still running when the body of its parent completes:

```tabular
column  Probate
column  An unfinished child
row  `cancelProbate`  is canceled
row  `awaitProbate`  is waited for
row  `failProbate`  makes the parent's `await` raise a checked `Async.Error`
row  `panicProbate`  makes the parent's `await` panic
```

Which to choose follows from what the children do:

- Work which changes state wants `awaitProbate`, so that every child runs to its end.
  Writing to a disk is an example, where a write which is half finished leaves things
  inconsistent.
- Pure work, with no side effects, can take `cancelProbate`, and stop wherever it happens to
  be, since nothing observes the difference.
- Code whose design is to await every task which it spawns wants a forgotten `await` to be
  an error, not a silent wait. That is what `failProbate` and `panicProbate` are for,
  checked or unchecked, according to how the program handles the unexpected.

Cancellation also runs upward. A task which fails propagates its failure to the scope which
owns it, and the scope cancels its other children. Work which has become pointless therefore
stops, and does not go on consuming resources on behalf of a computation which has already
failed.

### Threads

The thread model is chosen by import, from the package `threads`:

```tabular
column  Thread model
column  Runs each task on
row  `platformThreads`  a platform thread of its own
row  `virtualThreads`  a virtual thread of its own; fails at runtime before Java 21
row  `adaptiveThreads`  a virtual thread where they exist, and a platform thread where they do not
row  `pooledThreads`  a reusable carrier thread: virtual on the JVM, platform on Scala Native
row  `javascriptThreads`  the Scala.js event loop
```

Virtual threads are cheap enough to spawn in great numbers. `adaptiveThreads` is what an
application which cannot dictate its JVM should import. Under `javascriptThreads`, where
there are no threads at all, the same code, with `supervise`, `async` and `await`, runs
unchanged.

`pooledThreads` hands each new task to an idle carrier, where one is waiting, and starts
another only where none is. Handing a task over costs a few hundred nanoseconds, where
starting and joining a thread costs a few microseconds, so it suits fine-grained work: a
task for each element, or a spawn and a join inside a loop. A task which blocks keeps its
carrier, and the pool grows, so it can never starve, and cancellation is delivered to the
task, not to the carrier. The price is that a task no longer has a thread of its own.
Thread-locals persist across the tasks which a carrier runs, and a thread dump shows
carriers, where the tree of monitors shows tasks.

### Intercepting the VM

Some events belong to no scope: the VM shutting down, an exception escaping from a thread
which nothing owns, the garbage collector running. These are intercepted on `Vm`, which
stands for the running virtual machine. The action runs each time the event occurs, until
the `Hook` which is returned is canceled:

```scala fuel
typecheck
##
val hook = Vm.intercept[Fault]:
  Out.println(t"A thread died")

hook.cancel()
```

`Vm.intercept[Shutdown]` runs its action once, when the VM exits, whether through
`System.exit`, through the last non-daemon thread ending, or on a `SIGINT` or `SIGTERM`.
That is where a server removes the socket file which it listened on, so that a stale one
never greets the next process. Shutdown actions run on threads of their own, concurrently,
and in no particular order. An exception in one is swallowed, so that it cannot prevent the
others from running:

```scala fuel
typecheck
##
Vm.intercept[Shutdown]:
  Out.println(t"Exiting")
```

[Environment](environment.md#the-machine) describes the other facts about the machine.

### Suspension as an effect

Waiting is not free, and it is not invisible. Awaiting a task or a promise, sleeping, and
yielding all *suspend* the running task, and each demands a `Monitor` [capability] in scope.
A method which can block therefore says so in its signature, exactly as a method which can
fail says so with [`raises`][raises].

That suspension is allowed to be *blocking* is itself a decision of design which is worth
defending, since blocking has a poor reputation. It began as a convenience: if a value is
not ready, wait for it, instead of failing, or writing out both the ready case and the
unready one. The convenience is real. Code which blocks is code written in terms of values,
not callbacks, which is [direct style](../philosophy/direct-style.md).

What earned blocking its reputation was the cost. A platform thread which is waiting is a
thread which is not working, and an application which holds thousands of them spends more of
its capacity waiting than computing.

Virtual threads change that arithmetic. Orders of magnitude more calls can sit in a blocking
state at once, without meaningful overhead. The convenience can therefore be had at the
price which the alternatives based on callbacks were invented to avoid. Blocking where it
reads most clearly, on threads which are cheap enough that blocking does not matter, is the
shape of everything here. It is the shape of [streaming](streams.md) too, which pulls on the
consumer's own thread for the same reason.

Underneath, the unit of execution is a `Strand`, not a thread. A strand is an abstraction
with the four operations which suspension needs: interrupt, join, park and unpark. A
supervisor supplies strands, and a supervisor is a value. A scheduler which is not built on
threads at all, such as an event loop over WebAssembly's waitable sets, is therefore plugged
in as a supervisor, without any change to the code which spawns and awaits.

### Related

- [Streams](streams.md#combining-and-replicating-streams) merges and replicates streams
  across tasks.
- [HTTP server](http-server.md) handles each connection on a task.
- [Errors](errors.md) describes how a failure in a task is handled.
- [Daemons](daemons.md) keeps a program running as a service.
- [Caching](caching.md#an-expiring-value) computes a value once for many concurrent callers.
