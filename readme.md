[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/zeppelin/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/zeppelin/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Zeppelin

____

_Zeppelin_ makes it easy to work with ZIP files in Scala, providing methods for efficiently reading
and writing files within a ZIP archive, in a streaming or random access style.

## Features

- provides methods for reading and writing ZIP files
- integrates seamlessly with any file representations
- both streaming and random-access APIs are provided
- integrates directly with Turbulence readable and writable types


## Availability Plan

Zeppelin has not yet been published. The medium-term plan is to build Zeppelin
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Zeppelin.

Subsequently, Zeppelin will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

### Creating a ZIP file

A ZIP file can be constructed from an existing file by passing it to the `ZipFile` constructor.
Provided there is an appropriate `GenericPathReader` and `GenericPathMaker` (from
[Anticipation](https://github.com/propensive/anticipation)) in scope, any file representation (such
as `java.io.File`) may be used. For example,
```scala
import zeppelin.*
import diuretic.*
val file: java.io.File = java.io.File("/home/work/data.zip")
val zip = ZipFile(file)
```
or,
```scala
import zeppelin.*
import anticipation.fileApi.galileiApi
val file: galilei.DiskPath = Unix / p"home" / p"work" / p"data.zip"
val zip = ZipFile(file)
```

`ZipFile` provides several methods for working with the file.

#### Reading entries from a ZIP file

To read every entry from a `ZipFile`, call `ZipFile#entries()`. This will return a `LazyList[ZipEntry]`, a stream
of `ZipEntry`s in the order they are stored within the file, each one consisting of a `Relative` path (relative to
the root of the ZIP file) and a method to get its contents.

`ZipEntry`s support [Turbulence](https://github.com/propensive/turbulence/)'s `Readable` interface, so they can be
read as any type for which an `Aggregable` instance exists.

#### Appending files to an existing ZIP file

To add additional entries to a `ZipFile`, use `ZipFile#append`, which takes a
`LazyList` of `ZipEntry`s to append.

This method includes two additional parameters: a `prefix`, a `Bytes`
(`IArray[Byte]`) value to be inserted in raw form at the start of the ZIP file,
typically used to make the ZIP file executable; and a `timestamp` value for
specifying the timestamp of each entry appended to the ZIP file. If no
`timestamp` is specified, the current time will be used.





## Status

Zeppelin is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Zeppelin is designed to be _small_. Its entire source code currently consists
of 177 lines of code.

## Building

Zeppelin will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Zeppelin?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Zeppelin's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Zeppelin and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `zeppelin`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Zeppelin's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Zeppelin are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/zeppelin/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Zeppelin
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Zeppelin was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Like a ZIP file, a zeppelin airship may be inflated or deflated, and the name furthermore is an allusion to _zip_ping.

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

The logo shows the overlaid shapes of three zippers.

## License

Zeppelin is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

