[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/fulminate/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/fulminate/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Fulminate

__Rich and composable error messages__

While plain text is a sufficient medium for communicating with users (whether
they be other programmers, or end-users), it lacks a versatile way of marking
up parts of the text in a way that could be rendered in different ways as
markdown, HTML or console text: usually, it's necessary to commit early to
the output format. _Fulminate_ fills this gap by providing a convenient
representation of a message, with constructors which automatically contrast
the dynamic parts of the message from the static parts.

## Features

- rich representation of user-facing messages, particularly for errors
- demarcation of fixed and variable parts of a message
- provides a general-purpose `Error` type for immutable exception objects


## Availability

Fulminate has not yet been published as a binary.

## Getting Started



## Status

Fulminate is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Fulminate is designed to be _small_. Its entire source code currently consists
of 99 lines of code.

## Building

Fulminate can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Fulminate are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/fulminate/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Fulminate easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Fulminate was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

To _fulminate_ is to express vehement protest, while _Fulminate_ provides the means to express protestations at errors.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## License

Fulminate is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
