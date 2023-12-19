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


## Availability

Escritoire has not yet been published as a binary.

## Getting Started

Creating a table to be displayed in a monospaced font (e.g. for rendering in a console) is easy,
and first requires a `Tabulation` instance to be defined, specifying each column and how it should
be rendered.

For example,
```scala
case class Person(name: Text, age: Int, active: Boolean)

val table = Tabulation[Person](
  Column("Name", _.name),
  Column("Age", _.age),
  Column("Active", p => if p.active then "Yes" else "No")
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
val persons = List(Person("Bill", 48, true), Person("Janet", 54, false))
table.tabulate(100, persons)
```
will return a sequence of `Text`s which will print as,
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

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Escritoire is designed to be _small_. Its entire source code currently consists
of 248 lines of code.

## Building

Escritoire can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Escritoire are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/escritoire/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Escritoire easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Escritoire was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Escritoire is a pun: it is named after a type of writing table, since its purpose is for writing tables.

### Pronunciation

/ˌɛskɹiˈtwɑː/

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows some stylized papers laid out on a green writing table, or _escritoire_.

## License

Escritoire is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
