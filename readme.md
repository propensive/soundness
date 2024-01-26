[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/cellulose/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/cellulose/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Cellulose

__Comprehensive tools for working with CoDL in Scala__

_Cellulose_ is the reference implementation of the CoDL specification, and aims
to provide and demonstrate the working machinery of the language in Scala. The
functionality in Cellulose is similar to that found in typical JSON or XML
libraries.

## Features

- Implements the CoDL spec for Scala
- Read and write CoDL
- Support for reading and writing both Binary CoDL formats
- Convert between CoDL and case classes


## Availability Plan

Cellulose has not yet been published. The medium-term plan is to build Cellulose
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Cellulose.

Subsequently, Cellulose will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

Cellulose provides facilities for constructing, parsing, accessing,
manipulating and serializing CoDL data.

### Constructing

The easiest way to construct a new CoDL literal value is with an interpolated
`codl""` string, for example,
```scala
val codl: Codl = codl"""
  item 1
    size  1.08m
    mass  40kg
"""
```

Like any CoDL document, any amount of margin may be used as long is it is
consistent.

The CoDL literal will be parsed at compiletime, and any mistakes will present
themselves as compile errors. But the document will only be checked against the
"free" schema.

### Parsing

Text can be parsed as untyped CoDL at runtime with the `Codl.parse` method.
This currently accepts a `Reader`, but later versions will accept a
`DataStream`, i.e. a `LazyList[IArray[Byte]]`, and will be able to parse
directly from any streamable source.

Given a CoDL schema, it's possible to parse, checking for conformance at the same time, with,
```scala
schema.parse(input)
```

Both methods will produce a `CodlDoc` instance, which will include the schema
that checked it.

### Model

The CoDL document model accommodates nodes which may contain comments, data (as
parameters or child nodes) and remarks (comments at the ends of lines).

### Accessing values

A common way to work with CoDL data is to convert it to a case class structure.
This requires a `Codec` instance. `Codec`s are defined for simple types like
primitives and `Text`, and will be derived for product compositions of these
types on demand.

A `CodlDoc` can be converted to another type with the `CodlDoc#as` method, for example:
```scala
schema.parse(input).as[Person]
```

The exceptions which should be handled from such a call will depend on the
definition of `Person`, but the exceptions will be statically known.

### Serialization

Likewise, any type which has a `Codec` instance can be serialized to CoDL, with the `codl` extension method, for example,
```scala
val personCodl: CodlDoc =
  Person(t"Timothy", 28, TeamRef(t"Zeta")).codl
```

This can then be serialized to `Text` with the `Codl#serialize` method:
```scala
val txt = personCodl.serialize
```





## Status

Cellulose is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Cellulose is designed to be _small_. Its entire source code currently consists
of 1933 lines of code.

## Building

Cellulose will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Cellulose?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Cellulose's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Cellulose and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `cellulose`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Cellulose's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Cellulose are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/cellulose/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Cellulose
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Cellulose was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

To _coddle_, a homonym of CoDL, means—proverbially—to "wrap in cotton wool"; cotton wool is 90%
_cellulose_.

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

The logo imagines a high-level arrangement of cells in a hexagonal grid.

## License

Cellulose is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

