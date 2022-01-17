[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/tarantula/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/tarantula/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
[<img src="https://vent.dev/badge/propensive/tarantula" height="24">](https://vent.dev/)
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


## Getting Started

TBC


## Related Projects

_Tarantula_ has no dependencies.

No other _Niveau_ libraries are dependents of _Tarantula_.

## Status

Tarantula is classified as __embryonic__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Tarantula is designed to be _small_. Its entire source code currently consists of 125 lines of code.

## Building

Tarantula can be built on Linux or Mac OS with Vex, by running the `vex` script in the root directory:
```sh
./vex
```

This script will download `vex` the first time it is run, start a daemon process, and run the build. Subsequent
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
