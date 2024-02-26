[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/escritoire/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/escritoire/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Escritoire

__A library for writing tables__

_Escritoire_ is a simple library for printing tabular data in monospaced fonts,
typically for use in console applications.

## Features

- prints data using Unicode box-drawing characters
- supports multiline cells
- cell contents may be left, right or centrally aligned
- compact display for tables which don't include multiline contents


## Availability Plan

Escritoire has not yet been published. The medium-term plan is to build Escritoire
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Escritoire.

Subsequently, Escritoire will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

All Escritoire terms and types are defined in the `escritoire` package:
```scala
import escritoire.*
```

Creating a table to be displayed in a monospaced font (e.g. for rendering in a console) is easy,
and first requires a `Tabulation` instance to be defined, specifying each column and how it should
be rendered.

For example,
```scala
import anticipation.Text
import gossamer.t

case class Person(name: Text, age: Int, active: Boolean)

val table = Table[Person](
  Column(t"Name")(_.name),
  Column(t"Age")(_.age),
  Column(t"Active"): person =>
    if person.active then t"Yes" else t"No"
)
```
describes a table of three columns, `Name`, `Age` and `Active`, defined for rows of type `Person`,
where the content for each column is defined by a lambda, such as `_.name` and `_.age`. The return
types of these lambdas are any types which can be rendered as `AnsiText`s. In other words, any
type for which an `AnsiShow` instance exists.

Given such a definition, any collection of instances of `Person`, `ps`, can be rendered as a table
(a `Seq[Text]` of each output line) of maximum width `width` by calling
`table.tabulate(width, ps)`.

For example,
```scala
import turbulence.Out
import turbulence.stdioSources.virtualMachine
import escritoire.tableStyles.default
import hieroglyph.textMetrics.uniform

val persons = List(Person(t"Bill", 48, true), Person(t"Janet", 54, false))

def renderTable(): Unit =
  table.tabulate(persons, 100).foreach(Out.println(_))
```
will return and print a sequence of `Text`s as,
```
┌───────┬─────┬────────┐
│ Name  │ Age │ Active │
├───────┼─────┼────────┤
│ Bill  │ 48  │ Yes    │
│ Janet │ 54  │ No     │
└───────┴─────┴────────┘
```





## Status

Escritoire is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Escritoire is designed to be _small_. Its entire source code currently consists
of 246 lines of code.

## Building

Escritoire will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Escritoire?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Escritoire's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Escritoire and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `escritoire`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Escritoire's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Escritoire are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/escritoire/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Escritoire
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Escritoire was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Escritoire is a pun: it is named after a type of writing table, since its purpose is for writing tables.

### Pronunciation

/ˌɛskɹiˈtwɑː/

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

The logo shows some stylized papers laid out on a green writing table, or _escritoire_.

## License

Escritoire is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

