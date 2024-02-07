[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/inimitable/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/inimitable/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Inimitable

__UUIDs for Scala__

UUIDs are a convenient and standardized way to assign IDs, with guarantees that
those IDs will be unique, not just within a local system, but globally across
all systems. _Inimitable_ provides a few utilities for working with them.

## Features

- construct new UUIDs trivially
- check hard-coded UUIDs at compiletime
- parse UUIDs at runtime
- XOR and invert UUIDs


## Availability Plan

Inimitable has not yet been published. The medium-term plan is to build Inimitable
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Inimitable.

Subsequently, Inimitable will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

All Inimitable terms and types are defined in the `inimitable` package:
```scala
import inimitable.*
```

### Constructing a new UUID

A UUID can be constructed with the `Uuid()` factory method. This will create a
new, and by definition, universally unique, identifier. The `Uuid` instance is
composed of two 64-bit longs, `msb` (for "most significant bits") and `lsb`
("least significant bits"), implying (in theory) 128 bits of entropy.

#### Specific UUIDs

A particular UUID, for example `e6388c03-3dd2-4044-bb38-e58dbf8368fd`, may be
constructed using the `uuid""` interpolator, like so,
```scala
val uuid = uuid"e6388c03-3dd2-4044-bb38-e58dbf8368fd"
```
which will parse (at compiletime) the UUID hex digits and their format
ensuring, in particular, that all are present to represent 128 bits of data.

Additionally, a `Uuid` can be created at runtime with,
```scala
val long1 = 234827342384709201L
val long2 = 928160134367288378L
val uuid2 = Uuid(long1, long2)
```
which will use the bits from `long1` and `long2`.

Alternatively, a `Text` string containing a UUID can be parsed using
[Spectacular](https://github.com/propensive/spectacular/)'s `decodeAs`
extension method:
```scala
import spectacular.decodeAs
import gossamer.t
import perforate.errorHandlers.throwUnsafely

val text = t"e6388c03-3dd2-4044-bb38-e58dbf8368fd"
val uuid3 = text.decodeAs[Uuid]
```

This will raise a `UuidError` if it is not in the correct format.

### Methods on `Uuid`s

Two convenience methods are provided on `Uuid`s:
- the unary `~` operator, which will construct a new `Uuid` by inverting its bits, and
- the binary `^` operator, which will combine two `Uuid`s by XORing their bits




## Status

Inimitable is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Inimitable is designed to be _small_. Its entire source code currently consists
of 96 lines of code.

## Building

Inimitable will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Inimitable?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Inimitable's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Inimitable and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `inimitable`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Inimitable's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Inimitable are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/inimitable/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Inimitable
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Inimitable was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

The name _Inimitable_ describes the core feature of UUIDs: that they are universally unique, and cannot be imitated.

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

The logo shows an arrangement of the 128 bits which form a UUID in a grid.

## License

Inimitable is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

