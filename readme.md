[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/cardinality/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/cardinality/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Cardinality

__Constraints for constraining sets of real numbers__

_Cardinality_ introduces numerically-constrained `Double` types which are as usable as ordinary `Double`s, but safely constrained to a range that
is specified in their type.

## Features

- introduces `Double`s which are constrained to a numeric range, checked at compile time
- intuitive `a ~ b` syntax for representing a ranged `Double`
- ordinary `Double` literals can be used in positions which expect a ranged type
- numeric ranges compose under arithmetic operations
- escape-hatch (called `force`) for unsafe but easy conversions


## Availability Plan

Cardinality has not yet been published. The medium-term plan is to build Cardinality
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Cardinality.

Subsequently, Cardinality will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

_Cardinality_ provides a representation of numbers which must lie within a certain (closed) range. A range type is written with the
infix `~` type operator, between two doubles, for example, `-1.0 ~ 1.0` represents a `Double` which is at least `-1.0` and at most
`1.0`.

Compiletime operations check `Double` literals for conformance to the claimed bounds, for example:
```scala
val x: 0.0 ~ 100.0 = 33.3 // good
val y: 0.0 ~ 1.0 = 2.0    // compile error
```

Standard arithmetic operations are also implemented on ranged `Double`s. Depending on whether the right-hand operand is a statically-unknown
`Double`, a `Double` singleton literal, or a ranged `Double`, the result will be typed as precisely as possible. For example, adding `10.0` to
an instance of `3.0 ~ 5.0` will produce a result of type, `13.0 ~ 15.0`. These operations use typelevel arithmetic to calculate the resultant
range of the calculation, and can be composed like other arithmetic functions, with the return type inferred. For example,
```scala
var x: 0.0 ~ 1.0 = 0.2
var y: -1.0 ~ 1.0 = 0.2
var z: 1e3 ~ 1e8 = 10000

val result = (x + y*3.0)*z
```
will infer the type of `result` to be `-2.0e8 ~ 3.0e8` (while its value will be `6000.0`.

### Forcing values

Unranged `Double`s are pervasive in Scala, so a `Double#force` extension method is provided which can be used (carefully) to convert a `Double`
to an expected ranged type.





## Status

Cardinality is classified as __embryotic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Cardinality is designed to be _small_. Its entire source code currently consists
of 139 lines of code.

## Building

Cardinality will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Cardinality?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Cardinality's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Cardinality and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `cardinality`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Cardinality's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Cardinality are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/cardinality/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Cardinality
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Cardinality was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

The _cardinality_ of a set is the number of elements it contains, while _Cardinality_ controls the size of sets of `Double`s.

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

The logo shows the hat typically worn by a cardinal, alluding to the name _Cardinality_.

## License

Cardinality is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

