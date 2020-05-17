<a href="https://furore.dev/propensive/gastronomy"><img src="/doc/images/furore.png" style="vertical-align:middle" valign="middle"></a>&nbsp;&nbsp;<a href="https://furore.dev/propensive/gastronomy">__Develop with Fury__ </a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://riot.im/app/#/room/#propensive.gastronomy:matrix.org"><img src="/doc/images/riotim.png" style="vertical-arign:middle" valign="middle"></a>&nbsp;&nbsp;<a href="https://riot.im/app/#/room/#propensive.gastronomy:matrix.org">__Discuss on Riot__</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href="https://search.maven.org/search?q=g:com.propensive%20AND%20a:gastronomy_2.12"><img src="/doc/images/mavencentral.png" style="vertical-arign:middle" valign="middle"></a>&nbsp;&nbsp;<a href="https://search.maven.org/search?q=g:com.propensive%20AND%20a:gastronomy_2.12">__Download from Maven Central__</a>

<img src="/doc/images/github.png" valign="middle">

[![Build](https://github.com/propensive/gastronomy/workflows/Build/badge.svg)](https://github.com/propensive/gastronomy/actions)

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

- _embryonic_: for experimental or demonstrative purposes only, without guarantee of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement of designs
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
A binary will be made available on Maven Central.

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

Gastronomy is copyright &copy; 2018-20 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
