[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/parasitism/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/parasitism/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/parasitism-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/parasitism-core)
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

The current latest release of Parasitism is __0.4.0__.

## Getting Started

TBC


## Related Projects

The following _Scala One_ libraries are dependencies of _Parasitism_:

[![Anticipation](https://github.com/propensive/anticipation/raw/main/doc/images/128x128.png)](https://github.com/propensive/anticipation/) &nbsp; [![Rudiments](https://github.com/propensive/rudiments/raw/main/doc/images/128x128.png)](https://github.com/propensive/rudiments/) &nbsp;

No other _Scala One_ libraries are dependents of _Parasitism_.

## Status

Parasitism is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Parasitism is designed to be _small_. Its entire source code currently consists of 101 lines of code.

## Building

Parasitism can be built on Linux or Mac OS with Irk, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Parasitism are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/parasitism/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Parasitism easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Parasitism was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

A tick represents the completion of a task, while also being the name of a common human parasite, while threads can be parasites to a JVM.

## License

Parasitism is copyright &copy; 2022 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
