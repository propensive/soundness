[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/iridesce/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/iridesce/actions)
[<img src="https://img.shields.io/badge/gitter-discuss-f00762?style=for-the-badge" height="24">](https://gitter.im/propensive/iridesce)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/CHCPjERybv)
[<img src="https://img.shields.io/matrix/propensive.iridesce:matrix.org?label=MATRIX&color=0dbd8b&style=for-the-badge" height="24">](https://app.element.io/#/room/#propensive.iridesce:matrix.org)
[<img src="https://img.shields.io/twitter/follow/propensive?color=%2300acee&label=TWITTER&style=for-the-badge" height="24">](https://twitter.com/propensive)
[<img src="https://img.shields.io/maven-central/v/com.propensive/iridesce-core_2.12?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/iridesce-core_2.12)
[<img src="https://vent.dev/badge/propensive/iridesce" height="24">](https://vent.dev/)

<img src="/doc/images/github.png" valign="middle">

# Iridesce

_Iridesce_ implements several algorithms for working with colors represented in different forms.

## Features

- represents colors using a variety of different color models
- work with colors in RGB, HSV, CMY, CMYK, HSL, L\*a\*b\* and XYZ
- convert between any colors
- utilize color profiles (where necessary)
- provides a standard palette of named colors
- print colors as CSS, Hex or ANSI
- brighten, lighten, darken and blend colors
- calculate perceptual deltas between colors


## Getting Started

TBC


## Status

Iridesce is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without guarantee of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement of designs
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

## Availability

Iridesce&rsquo;s source is available on GitHub, and may be built with [Fury](https://github.com/propensive/fury) by
cloning the layer `propensive/iridesce`.
```
fury layer clone -i propensive/iridesce
```
or imported into an existing layer with,
```
fury layer import -i propensive/iridesce
```
A binary is available on Maven Central as `com.propensive:iridesce-core_<scala-version>:0.2.0`. This may be added
to an [sbt](https://www.scala-sbt.org/) build with:
```
libraryDependencies += "com.propensive" %% "iridesce-core" % "0.2.0"
```

## Contributing

Contributors to Iridesce are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/iridesce/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Iridesce easier.

Please __do not__ contact project maintainers privately with questions, as other users cannot then benefit from
the answers.

## Author

Iridesce was designed and developed by [Jon Pretty](https://twitter.com/propensive), and commercial support and
training is available from [Propensive O&Uuml;](https://propensive.com/).



## License

Iridesce is copyright &copy; 2021-21 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
