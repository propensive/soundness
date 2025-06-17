[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/typonym/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/typonym/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Typonym

__Collections raised to the type-level in Scala__

When writing macros or generic type-level code, we often need a way to
statically represent collections of values, without them to existing at runtime
and requiring allocation on the heap. Scala's `Tuple` type provides a generic
way of representing a heterogeneous sequence of values at the type-level, but
the inline or macro code that's necessary to work with them—in particular, to
decompose them into values—can be intricate, fragile and repetitive. _Typonym_
makes it easy to convert between value-level and type-level representations of
three collection types, `List`s, `Set`s and `Map`s, as well as straigtforward
conversions of singleton literal types.

## Features

- Provides type-level encodings of `List`s, `Set`s and `Map`s
- Convert between runtime values and static types
- Automatically convert singleton literal types
- Types can be trivially composed for more complex structures
- Types may be reified from a static type in "normal" code
- Types can also be converted from a `Type` or `TypeRepr` value from within a
  macro implementation


## Availability







## Getting Started

All terms and types are defined in the `typonym` package:
```scala
import typonym.*
```

### Application

Scala's rich type system may be used as a means of carrying static information
from one point in the code to another, so that it may be used for static
analysis and to influence typechecking. Much as a _runtime value_ may be passed
as a parameter to a method, and influence that method's _runtime behavior_, a
_compiletime type_ may be passed as a type parameter, and affect how that
method is typechecked.

The runtime behavior of a method is, assuredly, implemented with ordinary
calling and branching logic, operating on runtime values and runs, of course,
at runtime. And we write almost all code this way because it's easier.

In contrast, typechecking behavior (ignoring, for a moment, its relationship
with the runtime behavior it constrains) may be implemented using type-level
constructs that operate on types, and are not so much _run_ as _applied_ at
_compiletime_. This is a harder way of implementing logic: type-level
operations are less powerful and less expressive than those available at
runtime. So it is not ideal.

However, macros offer the ability to define type-level behavior using the
power, expressivity and simplicity of value-level code, which will be _run_ at
_compiletime_.

Such code often benefits from collections, such as lists, sets and maps, which
presents a need to represent such types in the type system, and to convert
between types (both static types and value-level reflections of those types in
macros) and standard Scala collections of values.

### Type-level Representations

Singleton types of `Int`s, `String`s, `Double`s and `Boolean`s have a
straightforward isomorphism from their value-level to their type-level
representations.

A Scala `List` will be encoded as a `Tuple` parameter to the `TypeList` type
constuctor. For example, the list, `List("one", "two", "three")` would be
represented as, `TypeList[("one", "two", "three")]` (by trivially composing it
with the type-level representation of `String`s).

### Reification

Typonym offers two overloaded variants of the `reify` method for transforming
types into values, to be invoked with a static type; or with an instance of
`Type`, from within a macro implementation.

For example, to convert the `List` example above to a value, we could call,
```scala
val list: List[String] = reify[TypeList[("one", "two", "three")]]
```
or in a more general context,
```scala
inline def printElements[ItemsType <: TypeList[?]]: Unit =
  reify[ItemsType].foreach(println)
```

Similarly, within a macro,
```scala

def printLongest[ItemsType <: TypeList[?]: Type]: Macro[Unit] =
  val items: List[String] = reify(Type.of[ItemsType])
  println(items.maxBy(_.length))
  '{()}
```


## Status

Typonym is classified as __embryotic__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Typonym is designed to be _small_. Its entire source code currently consists
of 77 lines of code.

## Building

Typonym will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Typonym?".

1. *Copy the sources into your own project*

   Read the `fury` file in the repository root to understand Typonym's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Typonym and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.

   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `typonym`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Typonym's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Typonym are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/typonym/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Typonym
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Typonym was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

A typonym is a name derived from a type. Similarly, _Typonym_ derives a _value_
from a type (and types from values).

In general, Soundness project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows the square brackets which usually delimit a type in Scala.

## License

Typonym is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).
