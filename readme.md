[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/aviation/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/aviation/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/aviation-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/aviation-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Aviation

The representation of time is complex, in part because the domain is
intrinsically complicated, and in part because we mean mean by "time" is
different in different contexts. _Aviation_ is an attempt to rationalize this
complexity by providing immutable representations of a variety concepts
relating to time and operations between them, taking advantage of the
opportunities Scala offers to make these APIs as intuitive as possible.

## Features

- representations points in time and lengths of time, in both exact/universal and civil forms
- types are all immutable and typesafe
- intuitive constructors for civil date and time values
- customisable rules for adding civil time units
- supports different calendar systems
- use and convert between different timezones and calendar systems


## Availability

The current latest release of Aviation is __0.4.0__.

## Getting Started

_Aviation_ provides a variety of types representing different time-related concepts.


## Related Projects

The following _Scala One_ libraries are dependencies of _Aviation_:

[![Anticipation](https://github.com/propensive/anticipation/raw/main/doc/images/128x128.png)](https://github.com/propensive/anticipation/) &nbsp; [![Cardinality](https://github.com/propensive/cardinality/raw/main/doc/images/128x128.png)](https://github.com/propensive/cardinality/) &nbsp; [![Eucalyptus](https://github.com/propensive/eucalyptus/raw/main/doc/images/128x128.png)](https://github.com/propensive/eucalyptus/) &nbsp; [![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/) &nbsp; [![Kaleidoscope](https://github.com/propensive/kaleidoscope/raw/main/doc/images/128x128.png)](https://github.com/propensive/kaleidoscope/) &nbsp; [![Probably](https://github.com/propensive/probably/raw/main/doc/images/128x128.png)](https://github.com/propensive/probably/) &nbsp;

No other _Scala One_ libraries are dependents of _Aviation_.

## Status

Aviation is classified as __embryonic__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Aviation is designed to be _small_. Its entire source code currently consists of 579 lines of code.

## Building

Aviation can be built on Linux or Mac OS with Irk, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Aviation are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/aviation/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Aviation easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Aviation was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

_Tempus fugit_, or, _time flies_.

## License

Aviation is copyright &copy; 2022-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
