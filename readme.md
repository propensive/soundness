[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/feudalism/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/feudalism/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Feudalism

__Controlled borrowing of mutable values for safe concurrency.__

A _mutex_ is a data structure designed for safe reading and writing to a
mutable variable in a concurrent environment. Specifically, a mutex variable
may be mutated (that is, its old value read and transformed into a new value)
so long as no other threads are reading or writing to the mutex at the same
time. However, any number of threads may read the mutex variable concurrently.

Feudalism implements a generic `Mutex` type which guarantees these constraints.

## Features

- implements a basic, generic mutex
- provides fast and safe concurrent access to a variable
- especially performant on Java 21 with virtual threads


## Availability Plan

Feudalism has not yet been published. The medium-term plan is to build Feudalism
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Feudalism.

Subsequently, Feudalism will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

TBC



## Status

Feudalism is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Feudalism is designed to be _small_. Its entire source code currently consists
of 65 lines of code.

## Building

Feudalism will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Feudalism?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Feudalism's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Feudalism and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `feudalism`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Feudalism's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Feudalism are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/feudalism/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Feudalism
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Feudalism was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

_Feudalism_ was the predominant social system in Medieval times, whereby
wealthy landowners would divide their land into strips, and lease it to tenants
to work. This is vaguely analogous, on some level, to the controlled access a
mutex provides to readers and writers of its variable.

In general, Scala One project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows a ring, split into two halves representing read-access and
write-access; in the read-access half, the ring splits into multiple threads,
while in the write-access half, the ring is just a single thread.

## License

Feudalism is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

