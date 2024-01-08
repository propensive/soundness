[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/caesura/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/caesura/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Caesura

__Simple parsing of CSV into case classes__

Caesura provides an API for reading and writing CSV and TSV.

## Features

- parse CSV and TSV data
- serialize product-like data (e.g. tuples or case classes) to CSV/TSV rows
- typeclass-based serialization and deserialization
- generic derivation of typeclasses for product and coproduct types


## Availability Plan

Caesura has not yet been published. The medium-term plan is to build Caesura
with [Fury](/propensive/fury) and to publish it as a source build on
[Vent](/propensive/vent). This will enable ordinary users to write and build
software which depends on Caesura.

Subsequently, Caesura will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

### Reading CSV Data

CSV data can be read from any value whose type has a `Source` typeclass instance in scope, such as `Text` or a `File` from
[Galilei](https://github.com/propensive/galilei), passing it to the `Csv.parse` method. For example,
```scala
import caesura.*
import galilei.*
val file: File = ...
val csv: Csv = Csv.parse(file)
```

Likewise, TSV data can be read with:
```scala
import caesura.*
import galilei.*
val file: File = ...
val tsv: Tsv = Tsv.parse(file)
```

### `Row`, `Csv` and `Tsv` Types

Both `Csv` and `Tsv` values are nothing more than wrappers around a sequence of `Row` instances, which are themselves `IArray`s
of `Text` values. While `Row` instances are the same regardless of whether their purpose is for CSV or TSV data, when wrapped
in a `Csv` or `Tsv` instance, they become serializable with the appropriate column separator (`','` or `'\t'`) and escaping.

Indeed, the companion objects `Csv` and `Tsv` are just two instances of the `RowFormat` type with different `separator` values
and implementations of the `RowFormat#escape` method, and alternative formats may be created by subclassing `RowFormat` and
overriding parameters.

### Interpreting rows

A `Row` instance may be converted to a case class by calling its `Row#as` method with a target type parameter, for example,
```scala
val row: Row = Csv.parseRow(t"Richard,Smith,38")

case class Person(firstName: Text, lastName, Text, age: Int)
val person: Person = row.as[Person]
```
will instantiate an instance of `Person`, `Person("Richard", "Smith", 38)` by associating the positional fields in the case
class definition with those in the `Row`, and applying the appropriate conversions to construct parameters of the appropriate
types to instantiate the `Person`. In this example, the `age` field is parsed to construct an `Int`.

If a case class definition includes a nested case class, for example,
```scala
case class Person(firstName: Text, lastName: Text, age: Int)
case class Role(title: Text, person: Person, managerial: Boolean)
```
then the structure would first be flattened, so the order of the positional fields for the `Role` case class would be,
`title` (0), `person.firstName` (1), `person.lastName` (2), `person.age` (3), and `managerial` (4).

This means the mapping from rows to case class instances is brittle: an additional field in a nested case class would be
certain to break interpretation of rows. But that is unfortunately the nature of working with CSV.

The `as` method also exists on `Csv` and `Tsv`, and will return a `List` of values of the specified type.

### Serializing to rows

`Csv` and `Tsv` instances may also be serialized to streams of data. Any `Seq[T]` (e.g. `List[T]` or `LazyList[T]`) may be
transformed to CSV or TSV by calling the `csv` or `tsv` extension methods on it. For example,
```scala
val persons: List[Person] =
  List(Person(t"Richard", t"Smith", 38), person2, person3)

val personsTsv: Tsv = persons.tsv
```





## Status

Caesura is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Caesura is designed to be _small_. Its entire source code currently consists
of 184 lines of code.

## Building

Caesura will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Caesura?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Caesura's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Caesura and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `caesura`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Caesura's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Caesura are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/caesura/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Caesura
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Caesura was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

A _caesura_ is break or pause in a sentence, often indicated by a comma—the same symbol that is used to indicate breaks in a CSV file.

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

The logo is a comma, the significant character for separating values in CSV files.

## License

Caesura is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

