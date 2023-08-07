[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/parasite/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/parasite/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Parasite

__Structured asynchronous task management__

_Parasite_ provides an implementation of asynchronous tasks, built upon Java threads for use in high-availability applications running on a Loom JVM,
or smaller-scale applications on other JVMs. All tasks form a supervisor hierarchy, where each task is "owned" by a supervising parent task, and
cancelation of tasks cascades through the hierarchy. This makes it easier to avoid thread leaks in complex systems. Scala 3's context functions are
used to track tasks unintrusively, while documenting a thread's blocking nature in its signature.

## Features

- simple interface for creating and running tasks
- optimized for Loom JVMs
- organizes tasks into a hierarchy
- avoids thread leakage


## Availability

Parasite has not yet been published as a binary.

## Getting Started

A Parasite task, an instance of `Task`, is similar to a Scala or Java
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
Prior to JDK 20, all threads are _platform threads_. Parasite needs to know
which type of thread to use, and this requires one of two imports:
- `parasite.threading.virtual`
- `parasite.threading.platform`

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




## Status

Parasite is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Parasite is designed to be _small_. Its entire source code currently consists
of 297 lines of code.

## Building

Parasite can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Parasite are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/parasite/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Parasite easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Parasite was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

A tick represents the completion of a task, while also being the name of a common human parasite, while threads can be parasites to a JVM.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## License

Parasite is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
