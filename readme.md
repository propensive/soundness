[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/diuretic/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/diuretic/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Diuretic

__Simple typeclass instances for integration with the Java Standard Library__

_Diuretic_ provides typeclass instances for
[Anticipation](https://github.com/propensive/anticipation/) typeclasses for the
Java Standard Library. Many of these types have better alternatives in the
[Scala One](https://github.com/propensive/one/) ecosystem, but there are times
when it is necessary to work with types from the Java Standard Library.

## Features

- provides typeclasses for working with time, filesystem objects and URLs
- implementations for `java.time.Instant` & `java.util` time types, `java.io` &
  `java.nio` filesystem types, and `java.net.URL`
- enables Java URLs to be used with the
  [Telekinesis](https://github.com/propensive/telekinesis/) HTTP client
- enables numerous projects like
  [Surveillance](https://github.com/propensive/surveillance/) to work with Java
IO and NIO types like `File` and `Path`
- allows numerous projects to use `java.time` or `java.util` types
- integration with other libraries that use
  [Anticipation](https://github.com/propensive/anticipation/) is seamless


## Availability Plan

Diuretic has not yet been published. The medium-term plan is to build Diuretic
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Diuretic.

Subsequently, Diuretic will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

Using Diuretic is usually as simple as adding an import, alongside another
project which implements
[Anticipation](https://github.com/propensive/anticipation/) typeclasses.
Currently six contextual imports are available for three different Anticipation
modules.

### Time Representation

Diuretic provides a choice of three different representations of durations and instants:

- `timeApi.javaTime`—uses `java.time.Instant` for instants and
  `Long` for durations
- `timeApi.javaLongTime`—uses `Long` for both instants and durations
- `timeApi.javaUtil`—uses `java.util.Date` for instants and `Long`
  for durations

### File Representation

Diuretic provides a choice of two different file representations:

- `fileApi.javaNio`—uses `java.nio.file.Path` for files, directories and paths
- `fileApi.javaIo`—uses `java.io.File` for files, directories and paths

A possible future version may offer representations which use distinct types
for paths (which may not relate to a file or directory) and files/directories.

### URL Representation

- `urlApi.javaNet`—uses `java.net.URL` for URLs





## Status

Diuretic is classified as __embryotic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Diuretic is designed to be _small_. Its entire source code currently consists
of 83 lines of code.

## Building

Diuretic will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Diuretic?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Diuretic's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Diuretic and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `diuretic`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Diuretic's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Diuretic are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/diuretic/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Diuretic
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Diuretic was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

The name _Diuretic_ alludes to a side-effect of drinking coffee, or in our case, Java.

### Pronunciation

`/ˌdaɪəˈretɪk/`

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

The logo shows a stylized cup of coffee, a well-known diuretic.

## License

Diuretic is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

