[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/gastronomy/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/gastronomy/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/CHCPjERybv)
[<img src="https://img.shields.io/maven-central/v/com.propensive/gastronomy-core_2.12?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/gastronomy-core_2.12)
[<img src="https://vent.dev/badge/propensive/gastronomy" height="24">](https://vent.dev/)

<img src="/doc/images/github.png" valign="middle">

# Gastronomy

Gastronomy is a simple library to provide cryptographic digests of Scala objects through a simple functional API, and present them in different encoded forms.

## Features

- hashing of simple and primitive Scala types
- generically-derived digests for all case class and sealed trait types
- supports SHA-256, SHA-1 and MD5 hash algorithms
- encodes into Hex or BASE-64, or BASE-64 (URL) encodings


## Getting Started

TBC


## Status

Gastronomy is classified as __maturescent__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

## Availability

Gastronomy&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/gastronomy`.
```
fury layer clone -i propensive/gastronomy
```
or imported into an existing layer with,
```
fury layer import -i propensive/gastronomy
```

## Contributing

Contributors to Gastronomy are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/gastronomy/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Gastronomy easier.

Please __do not__ contact project maintainers privately with questions, as other users cannot then benefit from
the answers.

## Author

Gastronomy was designed and developed by [Jon Pretty](https://twitter.com/propensive), and commercial support and
training is available from [Propensive O&Uuml;](https://propensive.com/).



## License

Gastronomy is copyright &copy; 2018-21 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
