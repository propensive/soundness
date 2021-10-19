[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
[<img src="https://vent.dev/badge/propensive/euphemism" height="24">](https://vent.dev/)
<img src="/doc/images/github.png" valign="middle">

# Euphemism



## Features

- parse and represent JSON in Scala
- intuitive dynamic API for field access, without compromising typesafety
- typeclass-based conversion to and from JSON
- generic derivation of typeclass interfaces for reading and writing product and coproduct types to JSON


## Getting Started

TBC


## Related Projects

The following _Niveau_ libraries are dependencies of _Euphemism_:

[![Clairvoyant](https://github.com/propensive/clairvoyant/raw/main/doc/images/128x128.png)](https://github.com/propensive/clairvoyant/) &nbsp; [![Wisteria](https://github.com/propensive/wisteria/raw/main/doc/images/128x128.png)](https://github.com/propensive/wisteria/) &nbsp;

No other _Niveau_ libraries are dependents of _Euphemism_.

## Status

Euphemism is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Euphemism is designed to be _small_. Its entire source code currently consists of 224 lines of code.

## Availability

Euphemism&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/euphemism`.
```
fury layer clone -i propensive/euphemism
```
or imported into an existing layer with,
```
fury layer import -i propensive/euphemism
```

## Contributing

Contributors to Euphemism are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/euphemism/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Euphemism easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Euphemism was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

A _euphemism_ is a means to describe something harsh with an inoffensive expression, while the JSON format, through its use of just a few simple types, can be considered a way of hiding unpleasant details when exchanging data.

## License

Euphemism is copyright &copy; 2019-21 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
