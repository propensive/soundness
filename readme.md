[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/escritoire/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/escritoire/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/escritoire-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/escritoire-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
<img src="/doc/images/github.png" valign="middle">

# Escritoire

_Escritoire_ is a simple library for printing tabular data in monospaced fonts,
typically for use in console applications.

## Features

- prints data using Unicode box-drawing characters
- supports multiline cells
- cell contents may be left, right or centrally aligned
- compact display for tables which don't include multiline contents


## Availability

The current latest release of Escritoire is __0.4.0__.

## Getting Started

Creating a table to be displayed in a monospaced font (e.g. for rendering in a console) is easy,
and first requires a `Tabulation` instance to be defined, specifying each column and how it should
be rendered.

For example,
```scala
case class Person(name: String, age: Int, active: Boolean)

val table = Tabulation[Person](
  Column("Name", _.name),
  Column("Age", _.age),
  Column("Active", p => if p.active then "Yes" else "No")
)
```
describes a table of three columns, `Name`, `Age` and `Active`, defined for rows of type `Person`,
where the content for each column is defined by a lambda, such as `_.name` and `_.age`. The return
types of these lambdas are any types which can be rendered as `AnsiString`s. In other words, any
type for which an `AnsiShow` instance exists.

Given such a definition, any collection of instances of `Person`, `ps`, can be rendered as a table
(a `Seq[String]` of each output line) of maximum width `width` by calling
`table.tabulate(width, ps)`.

For example,
```scala
val persons = List(Person("Bill", 48, true), Person("Janet", 54, false))
table.tabulate(100, persons)
```
will return a sequence of `String`s which will print as,
```
┌───────┬─────┬────────┐
│ Name  │ Age │ Active │
├───────┼─────┼────────┤
│ Bill  │ 48  │ Yes    │
│ Janet │ 54  │ No     │
└───────┴─────┴────────┘
```

## Related Projects

The following _Scala One_ libraries are dependencies of _Escritoire_:

[![Escapade](https://github.com/propensive/escapade/raw/main/doc/images/128x128.png)](https://github.com/propensive/escapade/) &nbsp;

The following _Scala One_ libraries are dependents of _Escritoire_:

[![Probably](https://github.com/propensive/probably/raw/main/doc/images/128x128.png)](https://github.com/propensive/probably/) &nbsp;

## Status

Escritoire is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Escritoire is designed to be _small_. Its entire source code currently consists of 199 lines of code.

## Building

Escritoire can be built on Linux or Mac OS with Vex, by running the `vex` script in the root directory:
```sh
./vex
```

This script will download `vex` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Escritoire are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/escritoire/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Escritoire easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Escritoire was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Escritoire is a pun: it is named after a type of writing table, since its purpose is for writing tables.

## License

Escritoire is copyright &copy; 2018-22 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
