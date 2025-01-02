[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/monotonous/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/monotonous/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Monotonous

__Serialization of binary data to text, in BASE-64, hexadecimal and other formats__

When transmitting binary data, we sometimes need to present it in a
textual form for human-readibility or to embed within another textual form, such
as XML.

The most common formats for this are hexadecimal and BASE-64, but BASE-32, octal
and binary are also used. A number of variants of these formats exist, optimised
for different scenarios.

_Monotonous_ provides a consistent API for serializing binary data into these
formats.

## Features

- provides a simple and general mechanism for serialization and deserialization
- supports binary, quaternary, octal, hexadecimal, BASE-32 and BASE-64
- allows custom alphabets to be used (with custom padding, where appropriate)
- optionally permits "tolerant" deserialization of equivalent characters, e.g. `1` and `l`, or `A` and `a`


## Availability







## Getting Started

### Terminology

Usually the term _serialization_ can mean the transformation of one kind of data
into another sequential output format. And usually, it would be interchangeable
with _encoding_. Likewise, _deserialization_ and _decoding_.

In Soundness, we give more precise meanings to both terms, and profit from
the ability to disambiguate between them: _serialization_ is the conversion from
_bytes_ to _text_ (or a stream of bytes to a stream of text) in formats such as
BASE-64 or hexadecimal, where no semantic meaning is ascribed to the bytes.

Conversely, _encoding_ concerns conversion from values to some other format,
such as JSON or XML. Deserialization and decoding are their respective inverse
operations.

This distinction is made consistently throughout the documentation and naming in
APIs.

### Imports

All Monotonous terms and types are in the `monotonous` package, and exported to
the `soundness` package. If you are using Monotonous alone, without any other
Soundness modules then,
```scala
import monotonous.*
```
or if you are using it with other Soundness modules,
```scala
import soundness.*
```

### Usage

To serialize a `Bytes` value (an instance of `IArray[Byte]`) to BASE-64, we can
use:
```scala
import alphabets.base64.standard
val bytes: Bytes = ???
bytes.serialize[Base64]
```

In general, to convert from a `Bytes` instance—an `IArray[Byte]`—we can call
`serialize` on that value with an output serialization format; one of,
 - `Base64`
 - `Base32`
 - `Hex`
 - `Octal`
 - `Binary`
provided an appropriate contextual `Alphabet` instance is in-scope for that
format.

#### Alphabets

Each of these serializations formats has a number of possible representations. For
example,
- hexadecimal (`Hex`) may be upper-case or lower-case
- BASE-64 requires two or three non-alphanumeric characters to represent all 64
  different output characters, and certain characters are better for certain
  scenarios
- BASE-64 output may also be padded or unpadded (typically with additional `=`
  characters)

In general, these variations are called _alphabets_, since they primarily
specify the full set of valid characters for the serialization format. In the
case of BASE-64, they also configure the presence or absence of padding. (This
is not a feature of _alphabets_ in the real world, but it makes sense to include
this parameter in the `Alphabet` type.)

An `Alphabet` instance can also specify whether any other characters (which are
_not_ in the alphabet) should be considered "equivalent" to others that _are_ in
the alphabet. For example, `l` and `1` are easy to transcribe incorrectly. If an
alphabet contains, for example, `l`, then it might be helpful to specify that
`1` refers to the same index.

Monotonous provides a variety of different alphabets for different serializations.
While each of these has a unique, deterministic serialization (e.g. `hex.upperCase`
always uses the letters `A`-`F`), for deserialization the BASE-32 and BASE-64
alphabets are _tolerant_ by default.

That means that `a5bc29ae` can be deserialized successfully using the `hex.upperCase`
alphabet, even though that alphabet would serialize the same data as `A5BC29AE`.
Alphabets which are not tolerant are labelled `strict`, for example,
`base32.strictLowerCase`.

Monotonous provides implementations of most commonly-used alphabets for each
output format. These are:
- `base64`
  - `standard`
  - `unpadded`
  - `url`
  - `xml`
  - `imap`
  - `yui`
  - `radix64`
  - `bcrypt`
  - `sasl`
  - `uuencoding`
- `base32`
  - `strictUpperCase`
  - `strictLowerCase`
  - `upperCase`
  - `lowerCase`
  - `extendedHexUpperCase`
  - `extendedHexLowerCase`
  - `zBase32`
  - `zBase32Unpadded`
  - `geohash`
  - `wordSafe`
  - `crockford`
- `hex`
  - `strictUpperCase`
  - `strictLowerCase`
  - `upperCase`
  - `lowerCase`
  - `bioctal`
- `octal`
  - `standard`
- `quaternary`
  - `standard`
  - `dnaNucleotide`
- `binary`
  - `standard`

and are all available in the `alphabets` package.


## Status

Monotonous is classified as __fledgling__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Monotonous is designed to be _small_. Its entire source code currently consists
of 261 lines of code.

## Building

Monotonous will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Monotonous?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Monotonous's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Monotonous and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `monotonous`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Monotonous's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Monotonous are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/monotonous/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Monotonous
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Monotonous was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

_Monotonous_ describes the continuous, unvarying nature of data that has been
serialized into a stream of characters from a limited alphabet.

In general, Soundness project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows several sine waves, representing a single monotonous tone,
overlaid upon each other, monotonously.

## License

Monotonous is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

