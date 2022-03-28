[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/imperial/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/imperial/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/imperial-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/imperial-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Imperial

_Imperial_ provides access to the standard directory structure in UNIX and Linux, as defined by the
[systemd](https://www.freedesktop.org/software/systemd/man/file-hierarchy.html) and [XDG Base
Directory](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) specifications.

## Features

TBC


## Availability

The current latest release of Imperial is __0.4.0__.

## Getting Started

TBC


## Related Projects

The following _Scala One_ libraries are dependencies of _Imperial_:

[![Clairvoyant](https://github.com/propensive/clairvoyant/raw/main/doc/images/128x128.png)](https://github.com/propensive/clairvoyant/) &nbsp; [![Rudiments](https://github.com/propensive/rudiments/raw/main/doc/images/128x128.png)](https://github.com/propensive/rudiments/) &nbsp;

No other _Scala One_ libraries are dependents of _Imperial_.

## Status

Imperial is classified as __fledgling__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Imperial is designed to be _small_. Its entire source code currently consists of 58 lines of code.

## Building

Imperial can be built on Linux or Mac OS with Irk, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Imperial are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/imperial/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Imperial easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Imperial was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

This project is particular to Linux operating systems, whos mascot is a penguin. While __Imperial__ defines the layout of the
operating system's own (little) empire, "Emperor" is also a species of penguin.

## License

Imperial is copyright &copy; 2022 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
