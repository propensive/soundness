[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/mercator/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/mercator/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Mercator

__Autogeneration of functor and monad instances for Scala types__

Mercator makes it possible to abstract over monads and functors, automatically
constructing contextual instances for types with the requisite methods.

This allows generic implementations of `sequence` and `traverse` to be provided
for all types which define `map` and `flatMap`, provided an appropriate
implementation of `Point` (providing the monadic "unit" operation) can be found.

## Features

- provides an abstraction over functor-like and monad-like types
- generates `Monad`, `Functor` and `Point` typeclasses for type constructors
- constructs a monad typeclass instance for any type with `flatMap`, `map` and a "unit" constructor
- implements `sequence` and `traverse` extension methods for instances of monadic types

## Availability Plan

Mercator has not yet been published. The medium-term plan is to build Mercator
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Mercator.

Subsequently, Mercator will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

It is not possible to write code such as,
```
def increment[MonadicType[_]](xs: MonadicType[Int]) =
  for(x <- xs) yield x + 1
```
because the compiler is not able to express the constraint that the type
constructor `F[_]` provides the methods `map` and `flatMap` (with the correct
signatures), which are necessary for the for-comprehension to compile.

With _Mercator_, it is possible to demand a contextual instance of `Monad[MonadicType]` to
enforce this constraint. Mercator will automatically instantiate such an
instance at the use-site for any type which has the required methods, like so,
```
import mercator._
def increment[F[_]: Monad](xs: F[Int]) = for(x <- xs) yield x + 1
```

The methods `flatMap` and `map` will be provided to the instance of `F[_]` as
extension methods, which are then used by the for-comprehension.

## Point

An instance of `Monad[F]` will generate an implementation of `point` (sometimes
called "unit", though not to be confused with `Unit`) which
constructs a new instance of the type from a single value. For example, `point(x)` for
`Option` is `Some(x)`, or for `Either` it is `Right(x)`. This implementation
assumes the existence of a unique `apply` method on the type's companion object, and
that applying the value to it will produce a result of the correct type.

A `Point[MonadicType]` instance can always be provided to explicitly specify the
`point` instance for the given type constructor.

## Functors

Mercator also provides a `Functor` typeclass, which provides implementations of just
`point` and `map`. If `map` is required for a particular operation, but `flatMap` is not,
then only the `Functor` typeclass should be summoned.




## Status

Mercator is classified as __embryotic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Mercator is designed to be _small_. Its entire source code currently consists
of 210 lines of code.

## Building

Mercator will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Mercator?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Mercator's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Mercator and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `mercator`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Mercator's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Mercator are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/mercator/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Mercator
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Mercator was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Gerardus Mercator was a cartographer who developed a general method of
projecting a map of the surface of a sphere onto a flat surface, while
preserving straight lines. Similarly, _Mercator_ provides `map` and `flatMap`
methods to objects which support them.

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

The logo shows the lines of latitude of an [Oblique Mercator Projection](https://en.wikipedia.org/wiki/Oblique_Mercator_projection).

## License

Mercator is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

