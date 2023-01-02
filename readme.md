[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/caesura/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/caesura/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/caesura-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/caesura-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Caesura

Caesura provides an API for reading and writing CSV and TSV.

## Features

- parse CSV and TSV data
- serialize product-like data (e.g. tuples or case classes) to CSV/TSV rows
- typeclass-based serialization and deserialization
- generic derivation of typeclasses for product and coproduct types


## Availability

The current latest release of Caesura is __0.4.0__.

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



## Related Projects

The following _Scala One_ libraries are dependencies of _Caesura_:

[![Anticipation](https://github.com/propensive/anticipation/raw/main/doc/images/128x128.png)](https://github.com/propensive/anticipation/) &nbsp; [![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/) &nbsp;

No other _Scala One_ libraries are dependents of _Caesura_.

## Status

Caesura is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Caesura is designed to be _small_. Its entire source code currently consists of 196 lines of code.

## Building

Caesura can be built on Linux or Mac OS with Irk, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Caesura are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/caesura/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Caesura easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Caesura was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

A _caesura_ is break or pause in a sentence, often indicated by a comma—the same symbol that is used to indicate breaks in a CSV file.

## License

Caesura is copyright &copy; 2018-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
