[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/camouflage/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/camouflage/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Camouflage

____

Caching provides a useful tradeoff between the costs of _time_ and _space_; that is, a cache can reduce the
amount of computation time at the expense of using more memory. _Camouflage_ provides one implementation of a
cache which will be appropriate for some (but not all) applications: a least-recently-used cache, or LRU cache.

## Features

- provides an implementation of a least-recently-used (LRU) cache
- cache requires only a _size_ parameter to initialize
- serves as a simple drop-in memoization wrapper around expensive method calls

## Availability

Camouflage has not yet been published as a binary.

## Getting Started



## Status

Camouflage is classified as ____. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Camouflage is designed to be _small_. Its entire source code currently consists
of 75 lines of code.

## Building

Camouflage can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Camouflage are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/camouflage/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Camouflage easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Camouflage was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name



In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo



## License

Camouflage is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
