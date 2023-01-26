[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/aviation/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/aviation/actions)
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

Aviation has not yet been published as a binary, though work is ongoing to fix this.

## Getting Started

_Aviation_ provides a variety of types representing different time-related concepts.

TBC


## Related Projects

The following _Scala One_ libraries are dependencies of _Aviation_:

[![Anticipation](https://github.com/propensive/anticipation/raw/main/doc/images/128x128.png)](https://github.com/propensive/anticipation/) &nbsp; [![Cardinality](https://github.com/propensive/cardinality/raw/main/doc/images/128x128.png)](https://github.com/propensive/cardinality/) &nbsp; [![Eucalyptus](https://github.com/propensive/eucalyptus/raw/main/doc/images/128x128.png)](https://github.com/propensive/eucalyptus/) &nbsp; [![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/) &nbsp; [![Kaleidoscope](https://github.com/propensive/kaleidoscope/raw/main/doc/images/128x128.png)](https://github.com/propensive/kaleidoscope/) &nbsp; [![Probably](https://github.com/propensive/probably/raw/main/doc/images/128x128.png)](https://github.com/propensive/probably/) &nbsp;

No other _Scala One_ libraries are dependents of _Aviation_.

## Status

Aviation is classified as __embryonic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Aviation is designed to be _small_. Its entire source code currently consists
of 610 lines of code.

## Building

Aviation can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Aviation are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/aviation/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Aviation easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Aviation was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

_Tempus fugit_, or, _time flies_.

## License

Aviation is copyright &copy; 2022-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
