[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/cosmopolite/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/cosmopolite/actions)
[<img src="https://img.shields.io/badge/gitter-discuss-f00762?style=for-the-badge" height="24">](https://gitter.im/propensive/cosmopolite)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/CHCPjERybv)
[<img src="https://img.shields.io/matrix/propensive.cosmopolite:matrix.org?label=MATRIX&color=0dbd8b&style=for-the-badge" height="24">](https://app.element.io/#/room/#propensive.cosmopolite:matrix.org)
[<img src="https://img.shields.io/twitter/follow/propensive?color=%2300acee&label=TWITTER&style=for-the-badge" height="24">](https://twitter.com/propensive)
[<img src="https://img.shields.io/maven-central/v/com.propensive/cosmopolite_2.12?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/cosmopolite_2.12)
[<img src="https://vent.dev/badge/propensive/cosmopolite" height="24">](https://vent.dev/)

<img src="/doc/images/github.png" valign="middle">

# Cosmopolite

_Cosmopolite_ provides a typesafe representation for working with multilingual strings, `Messages`, with convenient constructors which guarantee through their types that translations for a specific set of languages exist in each instance. Furthermore, a `Language` type provides a coproduct representation of a language from the same set which can safely select one string from the set.

## Features

- provides a representation for multilingual strings
- provides a representation of a language
- interpolated string constructors for common languages
- support for all [ISO 639-1](https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes) languages
- users choose their exact set of languages with a union type
- support for all language guaranteed by type for all static strings
- additions to the language set produce compile errors for every incomplete multilingual string
- checks for duplicate languages in construction of multilingual strings

## Getting Started

When working with a front-end application that requires 

## Status

Cosmopolite is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without guarantee of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement of designs
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

## Availability

Cosmopolite&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/cosmopolite`.
```
fury layer clone -i propensive/cosmopolite
```
or imported into an existing layer with,
```
fury layer import -i propensive/cosmopolite
```
A binary is available on Maven Central as `com.propensive:cosmopolite_<scala-version>:0.1.0`. This may be added
to an [sbt](https://www.scala-sbt.org/) build with:
```
libraryDependencies += "com.propensive" %% "cosmopolite" % "0.1.0"
```

## Contributing

Contributors to Cosmopolite are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/cosmopolite/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Cosmopolite easier.

Please __do not__ contact project maintainers privately with questions, as other users cannot then benefit from
the answers.

## Author

Cosmopolite was designed and developed by [Jon Pretty](https://twitter.com/propensive), and commercial support and
training is available from [Propensive O&Uuml;](https://propensive.com/).



## License

Cosmopolite is copyright &copy; 2021-21 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
