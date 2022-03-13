[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/tarantula/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/tarantula/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/tarantula-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/tarantula-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Tarantula

_Tarantula_ makes it possible to interact with a web browser through a programmatic interface. It
provides an immutable API for controlling the web browser from Scala, through the WebDriver
protocol.

## Features

- simulate keypresses and mouse clicks in a web browser
- automatically launch Chrome or Firefox programmatically
- uses the standard WebDriver protocol
- capture screenshots from web browsers


## Availability

The current latest release of Tarantula is __0.4.0__.

## Getting Started

TBC


## Related Projects

The following _Scala One_ libraries are dependencies of _Tarantula_:

[![Cataract](https://github.com/propensive/cataract/raw/main/doc/images/128x128.png)](https://github.com/propensive/cataract/) &nbsp; [![Euphemism](https://github.com/propensive/euphemism/raw/main/doc/images/128x128.png)](https://github.com/propensive/euphemism/) &nbsp; [![Guillotine](https://github.com/propensive/guillotine/raw/main/doc/images/128x128.png)](https://github.com/propensive/guillotine/) &nbsp; [![Honeycomb](https://github.com/propensive/honeycomb/raw/main/doc/images/128x128.png)](https://github.com/propensive/honeycomb/) &nbsp; [![Scintillate](https://github.com/propensive/scintillate/raw/main/doc/images/128x128.png)](https://github.com/propensive/scintillate/) &nbsp;

No other _Scala One_ libraries are dependents of _Tarantula_.

## Status

Tarantula is classified as __embryonic__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Tarantula is designed to be _small_. Its entire source code currently consists of 125 lines of code.

## Building

Tarantula can be built on Linux or Mac OS with Irk, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Tarantula are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/tarantula/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Tarantula easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Tarantula was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

__Tarantulas__ are spiders, known for making webs, and Tarantula is a library for the WebDriver protocol.

## License

Tarantula is copyright &copy; 2021-22 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
