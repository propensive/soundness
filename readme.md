[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/symbolism/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/symbolism/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Symbolism

__A general mechanism to implement overloaded symbolic operators__

Many projects will find it useful to be able to define symbolic operators as
extension methods for a variety of different types. Unfortunately, such methods
do not always coexist happily, and overload resolution between different—but
like-named—extension methods do not happily coexist in the same project: the
compiler is often unable to disambiguate between different methods.  Contextual
resolution is, however, much more reliable, so _Symbolism_ provides a single
definition of each of the arithmetic and comparison operators, which defers
their implementation to typeclasses inferred from their parameter types.

## Features

- uses typeclasses as a modular way to implement symbolic operators
- avoids overloading errors when mixing different projects which define symbolic extension methods
- provides typeclasses for the arithmetic operators, `+`, `-`, `*` and `/`
- provides a single inequality typeclass for the comparison operators, `<`, `<=`, `>` and `>=`


## Availability Plan

Symbolism has not yet been published. The medium-term plan is to build Symbolism
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Symbolism.

Subsequently, Symbolism will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

### Using Symbolic Operators

A project which defines symbolic operator implementations using Symbolism will
need Symbolism's extension methods in scope. This is simply a matter of
importing it, like so:
```scala
import symbolism.*
```

Subsequently, any object which does not define its own `+`, `-`, `*`, `/`, `<`,
`>`, `<=` or `>=` method will use Symbolism's definition, and search for an
appropriate contextual instance for the implementation, based on the types of
the left and right operands.

For example, calling `a + b` will search for an instance of
`Operator["+", a.type, b.type]`, say `plus`, and invoke its `apply` method on
the parameters, `a` and `b`. The result of `plus(a, b)` will be `plus.Result`,
whatever that happens to be for the particular choice of typeclass instance.

In many cases, the types `a.type`, `b.type` and `plus.Result` would all be the
same, as they are for adding two `Int`s or joining two stringlike objects, but
there is flexibility for the left and right operand types to be different, and
for the result type to be different again.

In fact, the typeclass definitions allow for implementations to be provided
with metaprogramming, which allows the result type to be computed as a function
of the left and right operand types.

### Implementing Symbolism's Typeclasses

Every binary operator is implemented with the `Operator` typeclass, or its
simplified subtype, `ClosedOperator` (for the cases where the left, right and
result types are the same). Both are parameterized on the singleton string
type of the operator name, and can be used to represent any binary operator,
though extension methods are (for now) only provided for `+`, `-`, `*` and `/`.

Any implementation of `Operator` should define the two operand types and a
`Result` type member, which will be the type of the result of the binary
operation, as well as the implementation of the operator, as its two-parameter
`apply` method.

The `apply` method, and its parameters, are inlined, so as to minimize the
performance cost of deferring simple operations to a typeclass, and any
implementation of `Operator` should do the same.





## Status

Symbolism is classified as __embryotic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Symbolism is designed to be _small_. Its entire source code currently consists
of 118 lines of code.

## Building

Symbolism will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Symbolism?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Symbolism's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Symbolism and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `symbolism`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Symbolism's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Symbolism are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/symbolism/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Symbolism
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Symbolism was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

__Symbolism__ helps work with _symbolic_ operators

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

The logo shows three of the four arithmetic operators overlaid upon each other.

## License

Symbolism is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

