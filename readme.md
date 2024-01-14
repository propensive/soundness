[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/hypotenuse/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/hypotenuse/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Hypotenuse

____

_Hypotenuse_ has the goal of improving the safety and aesthetics of working with numbers in Scala, without compromising on performance. It achieves this by several means. Numeric types have a consistent naming scheme of a letter—`U`, `I`, `F` or `B` for *i*nteger, *u*nsigned integer, *f*loating-point number and *b*itmap—and a number of bits, from `8` to `64`. Unsigned and signed (two's complement) numbers have different types, and both are distinct from _bitmap_ types which are used for bitwise operations; though conversions between them are easy (and free in terms of performance). Associated mathematical methods that were provided through the `java.lang.Math` class are made available, more consistently, as extension methods on each type. And operations such as division by zero on an integer, overflow or production of `NaN`s can be optionally checked.

## Features

- consistent representations of 8, 16, 32 and 64-bit signed and unsigned integers, and 32 and 64-bit floating point numbers
- uses opaque types and method inlining for native performance
- stricter typesafety over the standard Java/Scala primitive types
- opt-in range checking (e.g. overflow)
- distinct types for 8, 16, 32 and 64-bit bitmaps
- integration with standard mathematical operations (from `java.lang.Math`) through extension methods
- avoids implicit widening to `Int`


## Availability Plan

Hypotenuse has not yet been published. The medium-term plan is to build Hypotenuse
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Hypotenuse.

Subsequently, Hypotenuse will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started




## Status

Hypotenuse is classified as __embryotic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Hypotenuse is designed to be _small_. Its entire source code currently consists
of 1122 lines of code.

## Building

Hypotenuse will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Hypotenuse?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Hypotenuse's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Hypotenuse and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `hypotenuse`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Hypotenuse's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Hypotenuse are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/hypotenuse/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Hypotenuse
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Hypotenuse was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

The _hypotenuse_ is the longest edge of a right-angled triangle, whose length, according to Pythagoras' Theorem, is the square root of the sum of the squares of the other two sides. Pythagoras founded the school known as the _Pythagorean Brotherhood_, and it is to this school of mathematics that **Hypotenuse** alludes.

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

The logo shows a right-angled triange, the longest side of which is its _hypotenuse_.

## License

Hypotenuse is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

