[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/dissonance/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/dissonance/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/dissonance-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/dissonance-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Dissonance

Dissonance implements Eugene Myers' diff algorithm in Scala as an immutable function. Using it is as
simple as calling `Diff.diff(left, right)`, where `left` and `right` are sequences of like-typed
data to be comparied; the result is an instance of `Diff`, a sequence of additions, deletions and
no-change nodes representing each item in the left and right sequence.

## Features

- implements Myers' diff algorithm
- can be used with any data type, not just strings
- specify a custom comparison function enabling fine-grained merges on "similar" data
- diffs are simple immutable structures of `Del`, `Ins` and `Keep` nodes


## Availability

The current latest release of Dissonance is __0.1.0__.

## Getting Started

TBC


## Related Projects

The following _Scala One_ libraries are dependencies of _Dissonance_:

[![Eucalyptus](https://github.com/propensive/eucalyptus/raw/main/doc/images/128x128.png)](https://github.com/propensive/eucalyptus/) &nbsp; [![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/) &nbsp; [![Probably](https://github.com/propensive/probably/raw/main/doc/images/128x128.png)](https://github.com/propensive/probably/) &nbsp;

The following _Scala One_ libraries are dependents of _Dissonance_:

[![Cellulose](https://github.com/propensive/cellulose/raw/main/doc/images/128x128.png)](https://github.com/propensive/cellulose/) &nbsp;

## Status

Dissonance is classified as __maturescent__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Dissonance is designed to be _small_. Its entire source code currently consists of 152 lines of code.

## Building

Dissonance can be built on Linux or Mac OS with Irk, by running the `irk` script in the root directory:
```sh
./irk
```

This script will download `irk` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Dissonance are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/dissonance/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Dissonance easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Dissonance was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

The name Myers' original meaning is derived from "marsh" or "marshland", or a _dissonance_.

## License

Dissonance is copyright &copy; 2022-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
