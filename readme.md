[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/merino
jawn/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/merino
jawn/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Merino
jawn

__A fast JSON parser for Scala.__

_Merino_ is a JSON parser, and nothing more. It provides just a single `parse`
method that reads a stream of bytes and returns structured data.

## Features

- bare-bones JSON parsing
- provides a single `parse` method
- produces "raw" output, mostly as primitive types
- optimized for performance


## Availability Plan

Merino
jawn has not yet been published. The medium-term plan is to build Merino
jawn
with [Fury](/propensive/fury) and to publish it as a source build on
[Vent](/propensive/vent). This will enable ordinary users to write and build
software which depends on Merino
jawn.

Subsequently, Merino
jawn will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

Merino will parse a `DataStream`, that is, a `LazyList[IArray[Byte]]`. This
makes it easy to parse any type which can be converted to a byte array or a
lazy stream.

The `JsonAst.parse` method will parse the input and return a `JsonAst`, which
is an opaque type alias for the union of the following types:
- `Long`, `Double` and `BigDecimal`, representing JSON number types,
- `String` for JSON strings,
- `Boolean` for JSON `true` and `false` values,
- `Null` for the `null` value,
- `IArray[Any]` representing a JSON array, and,
- `(IArray[String], IArray[Any])` representing a JSON object

Note that which type of `Long`, `Double` or `BigDecimal` the parser chooses to
represent a given number will be determined by whether the type can represent
the number, as specified in the JSON source, precisely.

The types `IArray[Any]` and `(IArray[String], IArray[Any])`, representing
arrays and objects, will only ever contain `JsonAst`-typed values, despite
having type parameters of `Any`. But type aliases cannot refer to themselves,
so `Any` is used instead.

Additionally, the type `(IArray[String], IArray[Any])` was chosen as an
alternative to `IArray[(String, Any)]` since the former requires `2n + 3`
objects to be constructed for each field in the JSON object, as opposed to
`3n + 1` in the latter case: for anything but an object of exactly one key, the
former requires fewer objects to be created. Additionally, the types may be
disambiguated reflectively by their erased types.

If parsing fails, a `JsonParseError` will be thrown, including the line and
column in which the error occurs, and an enumeration value (of type
`JsonParseError.Issue`) describing the error.





## Status

Merino
jawn is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Merino
jawn is designed to be _small_. Its entire source code currently consists
of 784 lines of code.

## Building

Merino
jawn will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Merino
jawn?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Merino
jawn's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Merino
jawn and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `merino
jawn`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Merino
jawn's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Merino
jawn are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/merino
jawn/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Merino
jawn
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Merino
jawn was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Merino is named after the variety of sheep known for the quality of their fleeces, in keeping with the trend for
JSON libraries to allude to the story of Jason and the Argonauts.

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

The logo represents a ball of wool, potentially merino wool.

## License

Merino
jawn is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

