[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/parasite/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/parasite/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Parasite

__Safe, structured asynchronous tasks__

_Parasite_ provides a safe interface for working with structured concurrency,
built upon Java platform or virtual threads.
Asynchronous tasks form a supervisor hierarchy, where each task is
owned by a parent task which becomes bound to its lifecycle.

## Features

- simple interface for creating and running tasks
- supports platform or virtual threads
- asynchronous tasks form an intuitive hierarchy
- safe hierarchical cancellation
- ready for capture checking for improved resource safety


## Availability

TBC






## Getting Started

All Parasite terms and types are defined in the `parasite` package:
```scala
import parasite.*
```
and are exported to `soundness`, so you can alternatively just:
```scala
import soundness.*
```

A Parasite task, an instance of `Task`, is similar to a Scala or Java
`Future`: it encapsulates a block of code which it starts executing
immediately, and once evaluated, holds the result. There are, however, a few
differences.

We can create one inside a `supervise` block with:
```scala
import soundness.*

import strategies.throwUnsafely
import threadingModels.virtual

def run() = supervise:
  val task = async:
    delay(60*Second)
    1000
```

This creates a new `Task[Int]` where `Int` comes from the result of evaluating the `async`
body.

This will create *and start* the new task in a thread forked from the current
thread. Execution of `someSlowTask()` will proceed in a forked thread until it
completes.

We can call `task.await()` in the current thread to wait until the forked
thread finishes, and return the value which results from its execution.



Asynchronous tasks form a hierarchy. A new task may be started within the body of an
existing task, and the new task will be established a child of the latter. This
relationship does not change over the lifetime of the child task.

Parenthood is conveyed to a new child task through a `Monitor` instance, which is
introduced as a contextual instance in the body of an `async` or `supervise` block.

While both `async` and `supervise` provide `Monitor`s to their contexts, `async`
additionally _consumes_ a `Monitor` whereas `supervise` does not. Thus, `supervise` is
intended to be used at the top of an asynchronous hierarchy, while `async` blocks
delimit the branches of the hierarchy beneath it. Only `supervise` can create new
`Monitor`s without an existing `Monitor`, so every `async` must ultimately stem
from a `supervise` block.

Here's an example:

```scala
supervise:
  val task1: Task[Text] = async:
    val taskA: Task[Double] = async(readNumber())
    val taskB: Task[Double] = async(readNumber())

    t"The result is ${taskA.await() + taskB.await()}"

  Out.println(t"Running the task")
  val message = task1.await()
  Out.println(message)
```

This example includes three `async` blocks and a `supervise` block. The outer `supervise`
creates a new contextual `Monitor` from nothing. The `async` block in `task1` uses this
`Monitor` and introduces a new `Monitor`, linked to the first. And the `async` blocks in
`taskA` and `taskB` both use this child monitor and introduce their own `Monitor` instances.

Note that while the contextual `Monitor` instances of `supervise` _and_ the `Monitor` of
`task1`'s `async` block are in-scope when `readNumber()` is called, the latter has higher
priority because it is more deeply nested. This is exactly what we need.

Unlike an `async` block, which immediately returns a `Task`, and executes concurrently,
`supervise` runs synchronously, and returns a direct value.

The relationships between child and parent tasks constrain their lifecycles. A task may not
finish (that is, produce a result or be cancelled) until its children have also finished.

This has a few implications.

Usually, tasks will be `await`ed within the body in which they are created, but this is
not required, and it is not considered an error if a task is never explicitly awaited.

For example in,
```scala
supervise:
  val parent: Task[Text] = async:
    val child = async(slowOperation())
    t"finished"
  Out.println(parent.await())
```
a `child` task is started, but there's no explicit call to `child.await()`.

So despite reaching the return value `t"finished"` almost immediately, since `slowOperation()`
is running concurrently, the call to `parent.await()` will not return it until `child` has
finished executing.

This is important to maintain the invariant that following the completion of a task
(by `await`ing it), no further execution can take place on resources captured in its body—and
allowed to escape through a child task.

Here's an example where this matters:
```scala
supervise:
  val log = Log(p"/var/log/app.log")
  val parent: Task[Text] = async:
    log.info(t"Starting")

    async:
      for i <- 0 to 10 do
        delay(1*Second)
        log.info(t"Still running")

    t"complete"

  log.info(parent.await())
  log.close()
```

At the point `parent.await()` is called, the child task
will have been spawned and no long-running tasks stand in the way of the result `t"complete"`
being evaluated.

But the child task will keep running because it's in an infinite loop. _If_ we were to permit
the result to be returned right away, logged, and the logger `close`d, then the continued
execution of the child task would be problematic: it would attempt to log after the log is
closed.

So instead, the call to `parent.await()` will not return until the child task finishes.

### Termination

The child task runs continuously for ten seconds. Does that mean the call to `parent.await()` will
take ten seconds too? That depends!

Three options are available, as contextual values:
- `asyncTermination.await` will wait for all child tasks to terminate,
- `asyncTermination.cancel` will cancel the child asks at the first opportunity, and
- `asyncTermination.fail` will cause the call to `parent.await()` to throw an error
  (which must be handled) if any incomplete tasks exist when the parent task returns.
- `asyncTermination.panic` behaves like `asyncTermination.fail`, but throws an unchecked `Panic`
  error.

Each of these options may be more useful in different scenarios.

For example, if child tasks are involved in modifying mutable state (e.g. on disk), and an
incomplete write could result in data being in an inconsistent state, then it might be better
to use `await` to ensure each child task completes.

Whereas pure operations without side-effects should be able to use `cancel` to terminate
child tasks without consequences.

If our intention was to explicitly await every child task we spawn, by design, then we may wish
to make it an error if we forget to await one task. In this case, we can fail with a checked or
an unchecked exception.

### Cancellation

We can cancel a running task by calling its `cancel()` method. This does, however, require
the child's cooperation: a child task should call `relent()` every so often, at a point where
it would be safe to stop execution, if the task has been cancelled from another thread.

Adapting the example above, we could add a call to `relent()` to the infinite loop to check
abort execution if the task has been cancelled.


```scala
val task = async:
  for i <- 0 to 10 do
    delay(1*Second)
    relent()
    log.info(t"Still running")
```

Now, if `task` is cancelled because its parent task finishes before it, when it reaches the call
to `relent()`, it was cease execution. The `log.info` on the next line will not be executed, and
it will not complete with a result. But since, by definition, this value was never `await`ed
anyway, the absence of a result is opaque.

Despite the cancellation, the method will nevertheless delay for a full second, because `delay`
is uninterruptible. But alternatives exist.

### Snoozing, sleeping, pausing and hibernating

Four methods, `snooze`, `sleep`, `delay` and `hibernate` allow execution of the current
thread to stop temporarily. Between them, they provide _interruptible_ or _uninterruptible_
pauses for a time _duration_ or until an _instant_. Here, "interruptible" means that the pause
may end sooner if the paused task is cancelled.

- `snooze` and `sleep` are interruptible
- `delay` and `hibernate` are not interruptible
- `snooze` and `delay` take a time duration
- `sleep` and `hibernate` take an instant in time when they should wake up

To help remember the names,
- A `snooze` is a short delay of light sleep, like the few extra minutes (a fixed duration) of sleep
  the "snooze" button on an alarm clock offers.
- A `sleep` usually involves waking up at a particular time in the morning, whatever time of the evening
  it starts; but it's still possible to be woken in the night.
- An animal will `hibernate` until a particular time in the spring, and can't easily be woken.
- If a train experiences a `delay`, it is usually expressed as a duration of time (for example, "a
  five-minute delay"), but once there's a delay, it can't be "cancelled" to magically make the
  train on-time again.

### `Task` methods

An `Async` instance has several useful methods:
- `await()`, which blocks the current thread until the `Async` produces a value
- `await(timeout)`, which takes a `timeout`, after which a `TimeoutError` will
  be thrown if no value has been produced
- `map` and `flatMap`, providing standard monadic operations on the `Async`
- `cancel()`, which will stop the task running

### Platform or Virtual threading

From Java 21 and above, threads may be either _platform_ (corresponding to
threads managed by the operating system) or _virtual_ (managed by the JVM).
Prior to Java 21, all threads are _platform threads_. Parasite needs to know
which type of thread to use, and this requires one of three imports:
- `threadingModels.virtual`
- `threadingModels.platform`
- `threadingModels.adaptive`

Note that choosing `threadingModels.virtual` will result an a runtime error on JDKs
older than Java 21. To avoid this, use `threadingModels.adaptive` which will fall
back to platform threads on earlier JVMs.

### Cancellation

A task may be cancelled at any time, though cancellation requires cooperation from the task:
the task's body must be written to expect possible cancellation.

Within a task's body, the method `relent()` may be called multiple times. For
as long as the task is running normally, `relent()` will do nothing. But if a
task is cancelled, the task will stop immediately, without a value being
produced. Any `await()` calls on the task will throw a `CancelError`.

However, this happens _only_ when `relent()` is called, so if no such calls
are run as the task is executing, that task cannot be cancelled, and it must
execute to completion.


## Status

Parasite is classified as __fledgling__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Parasite is designed to be _small_. Its entire source code currently consists
of 728 lines of code.

## Building

Parasite will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Parasite?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Parasite's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Parasite and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `parasite`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Parasite's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Parasite are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/parasite/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Parasite
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Parasite was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

A tick indicates the completion of a task, while also being the name of a common human _parasite_, hence the name, and the allusion to the parasitic nature of threads.

In general, Soundness project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows a tick symbol, indicative of a task (which has been completed).

## License

Parasite is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

