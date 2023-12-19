[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/inimitable/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/inimitable/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Inimitable

__UUIDs for Scala__

UUIDs are a convenient and standardized way to assign IDs, with guarantees that
those IDs will be unique, not just within a local system, but globally across
all systems. _Inimitable_ provides a few utilities for working with them.

## Features

- construct new UUIDs trivially
- check hard-coded UUIDs at compiletime
- parse UUIDs at runtime
- XOR and invert UUIDs


## Availability

Inimitable has not yet been published as a binary.

## Getting Started

### Constructing a new UUID

A UUID can be constructed with the `Uuid()` factory method. This will create a
new, and by definition, universally unique, identifier. The `Uuid` instance is
composed of two 64-bit longs, `msb` (for "most significant bits") and `lsb`
("least significant bits"), implying (in theory) 128 bits of entropy.

#### Specific UUIDs

A particular UUID, for example `e6388c03-3dd2-4044-bb38-e58dbf8368fd`, may be
constructed using the `uuid""` interpolator, like so,
```scala
val uuid = uuid"e6388c03-3dd2-4044-bb38-e58dbf8368fd"
```
which will parse (at compiletime) the UUID hex digits and their format
ensuring, in particular, that all are present to represent 128 bits of data.

Additionally, a `Uuid` can be created at runtime with,
```scala
val uuid = Uuid(value)
```
which will parse the `Text` value, `value`, raising a `UuidError` if it is not
in the correct format.

#### Methods on `Uuid`s

Two convenience methods are provided on `Uuid`s:
- the unary `~` operator, which will construct a new `Uuid` by inverting its bits, and
- the binary `^` operator, which will combine two `Uuid`s by XORing their bits


## Status

Inimitable is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Inimitable is designed to be _small_. Its entire source code currently consists
of 95 lines of code.

## Building

Inimitable can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Inimitable are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/inimitable/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Inimitable easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Inimitable was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

The name _Inimitable_ describes the core feature of UUIDs: that they are universally unique, and cannot be imitated.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows an arrangement of the 128 bits which form a UUID in a grid.

## License

Inimitable is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
