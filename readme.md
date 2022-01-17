[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/caesura/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/caesura/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
[<img src="https://vent.dev/badge/propensive/caesura" height="24">](https://vent.dev/)
<img src="/doc/images/github.png" valign="middle">

# Caesura

Caesura provides an API for reading and writing CSV and TSV.

## Features

- parse CSV and TSV data
- serialize product-like data (e.g. tuples or case classes) to CSV/TSV rows
- typeclass-based serialization and deserialization
- generic derivation of typeclasses for product and coproduct types


## Getting Started

TBC

## Related Projects

The following _Niveau_ libraries are dependencies of _Caesura_:

[![Wisteria](https://github.com/propensive/wisteria/raw/main/doc/images/128x128.png)](https://github.com/propensive/wisteria/) &nbsp;

No other _Niveau_ libraries are dependents of _Caesura_.

## Status

Caesura is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Caesura is designed to be _small_. Its entire source code currently consists of 119 lines of code.

## Availability

Caesura&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/caesura`.
```
fury layer clone -i propensive/caesura
```
or imported into an existing layer with,
```
fury layer import -i propensive/caesura
```

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

Caesura is copyright &copy; 2018-22 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
