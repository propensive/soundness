[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/parasitism/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/parasitism/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Parasitism

_Parasitism_ provides an implementation of asynchronous tasks, built upon Java threads for use in high-availability applications running on a Loom JVM,
or smaller-scale applications on other JVMs. All tasks form a supervisor hierarchy, where each task is "owned" by a supervising parent task, and
cancelation of tasks cascades through the hierarchy. This makes it easier to avoid thread leaks in complex systems. Scala 3's context functions are
used to track tasks unintrusively, while documenting a thread's blocking nature in its signature.

## Features

- simple interface for creating and running tasks
- optimized for Loom JVMs
- organizes tasks into a hierarchy
- avoids thread leakage


## Availability

Parasitism has not yet been published as a binary, though work is ongoing to fix this.

## Getting Started

TBC


## Related Projects

The following _Scala One_ libraries are dependencies of _Parasitism_:

[![Diuretic](https://github.com/propensive/diuretic/raw/main/doc/images/128x128.png)](https://github.com/propensive/diuretic/) &nbsp; [![Rudiments](https://github.com/propensive/rudiments/raw/main/doc/images/128x128.png)](https://github.com/propensive/rudiments/) &nbsp;

The following _Scala One_ libraries are dependents of _Parasitism_:

[![Turbulence](https://github.com/propensive/turbulence/raw/main/doc/images/128x128.png)](https://github.com/propensive/turbulence/) &nbsp;

## Status

Parasitism is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Parasitism is designed to be _small_. Its entire source code currently consists
of 262 lines of code.

## Building

Parasitism can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Parasitism are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/parasitism/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Parasitism easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Parasitism was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

A tick represents the completion of a task, while also being the name of a common human parasite, while threads can be parasites to a JVM.

## License

Parasitism is copyright &copy; 2022-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
