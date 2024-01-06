[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/wisteria/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/wisteria/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Wisteria

__Simple, fast and transparant generic derivation for typeclasses__

__Wisteria__ is a generic macro for automatic materialization of typeclasses for datatypes composed from product
types (e.g. case classes) and coproduct types (e.g. enums). It supports recursively-defined datatypes
out-of-the-box, and incurs no significant time-penalty during compilation.

## Features

 - derives typeclasses for case classes, case objects and sealed traits
 - offers a lightweight syntax for writing derivations without needing to understand complex parts of Scala
 - builds upon Scala 3's built-in generic derivation
 - works with recursive and mutually-recursive definitions
 - supports parameterized ADTs (GADTs), including those in recursive types
 - supports typeclasses whose generic type parameter is used in either covariant and contravariant positions


## Availability Plan

Wisteria has not yet been published. The medium-term plan is to build Wisteria
with [Fury](/propensive/fury) and to publish it as a source build on
[Vent](/propensive/vent). This will enable ordinary users to write and build
software which depends on Wisteria.

Subsequently, Wisteria will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

Given an ADT such as,
```scala
enum Tree[+T]:
  case class Branch(left: Tree[T], right: Tree[T])
  case class Leaf(value: T)
```
and provided an given instance of `Show[Int]` is in scope, and a Wisteria derivation for the `Show` typeclass
has been provided, we can automatically derive given typeclass instances of `Show[Tree[Int]]` on-demand, like
so,
```scala
Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).show
```
Typeclass authors may provide Wisteria derivations in the typeclass's companion object, but it is easy to create
your own.

The definition of a `Show` typeclass with generic derivation defined with Wisteria might look like this:
```scala
import wisteria.*

trait Show[T]:
  def show(value: T): String

object Show extends Derivation[Show]:
  def join[T](ctx: CaseClass[Show, T]): Show[T] =
    ctx.params.map { p =>
      s"${p.label}=${p.typeclass.show(p.dereference(value))}"
    }.mkString("{", ",", "}")

  override def split[T](ctx: SealedTrait[Show, T]): Show[T] = value =>
    ctx.dispatch(value) { sub => sub.typeclass.show(sub.cast(value))
```

The `Derivation` trait provides a `derived` method which will attempt to construct a corresponding typeclass
instance for the type passed to it. Importing `Show.derived` as defined in the example above will make generic
derivation for `Show` typeclasses available in the scope of the import.

While any object may be used to define a derivation, if you control the typeclass you are deriving for, the
companion object of the typeclass is the obvious choice since it generic derivations for that typeclass will
be automatically available for consideration during contextual search.

### Limitations

Wisteria is not currently able to access default values for case class parameters.




## Status

Wisteria is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Wisteria is designed to be _small_. Its entire source code currently consists
of 178 lines of code.

## Building

Wisteria will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Wisteria?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Wisteria's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Wisteria and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `wisteria`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Wisteria's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Wisteria are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/wisteria/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Wisteria
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Wisteria was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Wisteria is a flowering plant, much like magnolia is, and Wisteria is a derivative of Magnolia.

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

The logo shows a hazy, floral shape in pale colors.

## License

Wisteria is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

