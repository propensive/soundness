[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/perforate/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/perforate/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Perforate

__Versatile error handling for every scenario.__

Perforate is a highly experimental library for abstracting over error handling. In particular, it gives developers a choice between throwing exceptions, returning errors in a variety of datatypes, and (most notably) accumulating several validation-style errors. Code must be written to accomodate Perforate's generic error handling, but the changes from exception-throwing code are trivial.

## Features

TBC


## Availability

Perforate has not yet been published as a binary.

## Getting Started

TBC


## Status

Perforate is classified as __embryotic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Perforate is designed to be _small_. Its entire source code currently consists
of 248 lines of code.

## Building

Perforate can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Perforate are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/perforate/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Perforate easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Perforate was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Tickets for public transport, and various other services, would historically have a hole punched into them to indicate that they had been checked, thus performing validation by perforation.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows three tickets, each of which has been _validated_.

## License

Perforate is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
