[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/adversaria/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/adversaria/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Adversaria

__Providing typeclass interfaces to user-defined Scala annotations__

__Adversaria__ is a tiny library which provides a few tools to make it easier to work with static
_annotations_ in Scala, by making them available through _typeclass interfaces_.

## Features

- access all annotations on a type through a typeclass
- resolve a typeclass instance only if a type has an annotated field
- makes annotations more useful and accessible in Scala
- no macro code is required to use annotations


## Availability Plan

Adversaria has not yet been published. The medium-term plan is to build Adversaria
with [Fury](/propensive/fury) and to publish it as a source build on
[Vent](/propensive/vent). This will enable ordinary users to write and build
software which depends on Adversaria.

Subsequently, Adversaria will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

Annotations in Scala are rarely the best solution for any task, but can
nevertheless be convenient as a "feature of last resort" when no other solution
provides the right ergonomics. This small domain is where Adversaria helps.

Currently three use cases are supported:
- getting all the annotations applied to a particular type
- finding the particular parameter of a case class to which a certain
  annotation has been applied
- getting every annotation applied to a particular case class field

This list of supported use cases is likely to grow.

### Annotations

If we define the following annotations in the standard way (each starting with
a lower-case letter, as is the convention)
```scala
import scala.annotation.StaticAnnotation

final case class id() extends StaticAnnotation
final case class count(n: Int) extends StaticAnnotation
```
we could apply them to some case classes, such as:
```scala
@count(10)
case class Company(name: Text)
case class Person(name: Text, @id email: Text)
```

We would like to write code that can access annotations such as `@count` and
`@id` through a simple typeclass interface.

### Contextual evidence

Elsewhere, we may have a polymorphic method, say `inspect`, which inspects an
instance of a type:
```scala
def inspect[T](value: T): Unit
```

If we would like to get the annotations on `T` that are subtypes of `count`, we
can get these with the typeclass, `Annotations`:
```scala
def inspect[T](value: T)(using anns: Annotations[count, T]): Unit =
  anns.collect:
    case `count`(n: Int) => println(t"count = $n")
```

If `inspect` is called with a type, `T`, that does not have any `@count`
annotations, then no contextual `Annotations[count, T]` instance will be
constructed, and the code will not compile. So `inspect[Company](company)`
would compile, while `inspect[Person](person)` would not.

### Direct inspection

Three methods also provide access to annotations on fields:
- `Annotations.field[T](fn)` will return a list of annotations on the case
  class field indicated by the lambda, `fn`. This lambda must be a simple field
accessor, such as `_.email`, otherwise the method will not compile.
- `Annotations.fields[T, A]` will return a list of `CaseField` instances
  providing access to the name, annotation and value (if given an instance of
`T` to dereference) for each annotation on any field with an annotation of type
`A` in `T`'s definition.
- `Annotations.firstField[T, A]` will return the first such field, if it exists.






## Status

Adversaria is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Adversaria is designed to be _small_. Its entire source code currently consists
of 156 lines of code.

## Building

Adversaria will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Adversaria?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Adversaria's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Adversaria and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `adversaria`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Adversaria's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Adversaria are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/adversaria/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Adversaria
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Adversaria was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

_Adversaria_ are miscellaneous collections of notes or _annotations_, after which the library is named.

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

The logo is an [arobase](https://en.wikipedia.org/wiki/At_sign) or "at-sign", being the Scala (and Java) symbol which introduces an annotation.

## License

Adversaria is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

