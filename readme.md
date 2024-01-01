[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/surveillance/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/surveillance/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Surveillance

__To make I/O in Scala elegant and typesafe__

__Surveillance__ watches directories for changes, and provides a `LazyList` of streaming `WatchEvent`s. While it
works well with [Galilei](https://github.com/propensive/galilei/), it can work with any representation of
paths.

## Features

- simple API for most common filesystem operations
- read from and write to files on disk with a variety of different types
- simple streaming to and from disk with `LazyList`s
- employs `IArray[Byte]` for fast, random-access, immutable chunking of byte data
- encoding-aware operations involving `String`s


## Availability Plan

Surveillance has not yet been published. The medium-term plan is to build Surveillance
with [Fury](/propensive/fury) and to publish it as a source build on
[Vent](/propensive/vent). This will enable ordinary users to write and build
software which depends on Surveillance.

Subsequently, Surveillance will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

Surveillance watches directories for changes to their contents, through an extension method, `watch()`, on a
type representing a directory or a `Seq` of such types. For [Galilei](https://github.com/propensive/galilei)'s
`Directory` type, this works straight away. Other libraries which provide directory-like types can integrate with
Surveillance just by defining two simple [typeclass instances](#defining-typeclass-instances).

### Watching

A simple setup for watching a directory looks like this:
```scala
import galilei.*
import serpentine.*
import surveillance.*

val dir = (Unix / p"home" / p"work" / p"updates").directory()
val watcher = dir.watch()
```

Constructing a new `Watcher` on a directory will register that directory with the filesystem's filewatching service
and start a new thread to respond to updates.

The most important method of a `Watcher` is its `stream` method, which will return a `LazyList[WatchEvent]`





## Status

Surveillance is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Surveillance is designed to be _small_. Its entire source code currently consists
of 109 lines of code.

## Building

Surveillance will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Surveillance?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Surveillance's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Surveillance and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `surveillance`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Surveillance's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Surveillance are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/surveillance/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Surveillance
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Surveillance was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

This library watches directories for changes, which is to say it keeps them under __surveillance__.

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

The logo shows a glistening eye, watching or _surveilling_.

## License

Surveillance is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

