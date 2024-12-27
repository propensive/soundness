[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/prepositional/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/prepositional/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Prepositional

__Expressive named type relations__

As Scala moves further and further into the realm of dependent typing, type members will become a
more prevalent way to express a variety of facets of the type. Unfortunately, type member syntax
remains verbose. But infix type aliases offer an elegant syntax for expressing them, without the
syntactic baggage.

Prepositional does nothing more than define some common infix type aliases for a variety of common
use-cases.

## Features

- defines eight general-purpose supplementary type combinators
- type supplements are introduced with common English prepositions
- provides an everyday language for expressing type members
- supplementary types are optional, which makes them versatile
- exact interpretation of supplementary types is context-dependent


## Availability

Prepositional has not yet been published. The medium-term plan is to build it with
[Fury](https://github.com/propensive/fury) and to publish it as a source build
on [Vent](https://github.com/propensive/vent). This will enable ordinary users
to write and build software which depends on Prepositional.

Subsequently, Prepositional will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).



## Getting Started

### Supplementary types

Prepositional defines the aliases, `by`, `from`, `in`, `into`, `of`, `on`, `onto`, `over` and `under`,
to be used in _type supplements_.

A type supplement is a transformation of a type `T`, in the form `T op S`, which specifies a
particular type member on `T`, according to the rules of the infix type alias, `op`.

For example, we could define the type `Expressible` as a typeclass interface for specifying how
things of a particular format should be expressed:
```amok
syntax scala
##
trait Expressible:
  type Self
  type Format

  def express(value: Self): Format
```

`Expressible` has two type members, `Self` and `Format`, which respectively
specify the type of the value to be expressed and the type in which it should be expressed. Here's
an example instance of `Expressible`:
```amok
syntax scala
##
given Expressible = new Expressible:
  type Self = Int
  type Format = Text

  def express(value: Int): Text = value.toString.tt
```

Using Scala 3's new modularity syntax,
```amok
syntax scala
##
import language.experimental.modularity
```
it is possible to write that as,
```amok
syntax scala
##
given Int is Expressible:
  type Format = Text

  def express(value: Int): Text = value.toString.tt
```
using the `is` type alias that "injects" the `Self` type member of `Expressible` with the type
`Int`. So the type
`Int is Expressible` is an alias of `Expressible { type Self = Int }`.

We can achieve similar with the type `Expressible in Text`, which dealiases to,
`Expressible { type Format = Text }`. The supplement `in Text` again "injects" the `Format` type
member into `Expressible` as the type `Text`.

Note that the type member name, `Format`, is specific to the `in` type alias.

We can compose a type using both `is` and `in`. The type `Int is Expressible in Text` is
equal to the type `Expressible { type Self = Int; type Format = Text }`.

### Type Members

The eight infix types defined in Prepositional, all of which are English prepositions, each adds
a different type member to an existing
type. These type member names relate semantically to the preposition, but not linguistically.

They are:
- `by` adds the `Operand` type member
- `from` adds the `Source` type member
- `in` adds the `Format` type member
- `into` adds the `Result` type member
- `of` adds the `Subject` type member
- `on` adds the `Platform` type member
- `onto` adds the `Target` type member
- `over` adds the `Carrier` type member
- `under` adds the `Constraint` type member

So, for example, a type such as `Fillable by Text on Linux into Data` would correspond to the type,
`Fillable { type Operand = Text; type Platform = Linux; type Result = Data }`.

It is important to be clear that Prepositional _does not_ specify how `Operand`, `Platform` and
`Result` should be interpreted. In the `Fillable` example, it is for the definition of `Fillable`
to specify that.

Prepositional does nothing more than to provide the means of specifying type members with the names
`Operand`, `Source`, `Format`, `Result`, `Subject`, `Platform`, `Target` and `Carrier`. And by
offering convenient syntax, it _suggests_ these type member names as general, convenient and
reusable. But it takes no responsibility for their meaning beyond their English language semantics.

### Prepositions

Prepositions are a rare commodity in English (or almost any language). They can be somewhat vague
in the meaning they convey, and are sometimes interchangeable. Consequently, a few prepositions go
a long way towards disambiguating the roles of different objects involved in an action.

So it is not necessary to define many infix operators corresponding to many prepositions. Types
rarely have _many_ type members, and the important thing is that their roles can be disambiguated.
And therefore, just a few definitions are sufficient to express a broad variety of relationships
between types.

However, it's possible that new general purpose infix type operators could be defined later.


## Status

Prepositional is classified as __fledgling__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Prepositional is designed to be _small_. Its entire source code currently consists
of 12 lines of code.

## Building

Prepositional will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Prepositional?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Prepositional's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Prepositional and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `prepositional`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Prepositional's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Prepositional are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/prepositional/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Prepositional
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Prepositional was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

The name _prepositional_ describes the grammatical role of the infix type aliases in type
definitions.

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

The logo shows a ball being placed into a box. These objects are often used to explain prepositions.

## License

Prepositional is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

