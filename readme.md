[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/merino
jawn/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/merino
jawn/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Merino
jawn

__A fast JSON parser for Scala.__

_Merino_ is a JSON parser, and nothing more. It provides just a single `parse`
method that reads a stream of bytes and returns structured data.

## Features

- bare-bones JSON parsing
- provides a single `parse` method
- produces "raw" output, mostly as primitive types
- optimized for performance


## Availability

Merino
jawn has not yet been published as a binary.

## Getting Started

Merino will parse a `DataStream`, that is, a `LazyList[IArray[Byte]]`. This
makes it easy to parse any type which can be converted to a byte array or a
lazy stream.

The `JsonAst.parse` method will parse the input and return a `JsonAst`, which
is an opaque type alias for the union of the following types:
- `Long`, `Double` and `BigDecimal`, representing JSON number types,
- `String` for JSON strings,
- `Boolean` for JSON `true` and `false` values,
- `Null` for the `null` value,
- `IArray[Any]` representing a JSON array, and,
- `(IArray[String], IArray[Any])` representing a JSON object

Note that which type of `Long`, `Double` or `BigDecimal` the parser chooses to
represent a given number will be determined by whether the type can represent
the number, as specified in the JSON source, precisely.

The types `IArray[Any]` and `(IArray[String], IArray[Any])`, representing
arrays and objects, will only ever contain `JsonAst`-typed values, despite
having type parameters of `Any`. But type aliases cannot refer to themselves,
so `Any` is used instead.

Additionally, the type `(IArray[String], IArray[Any])` was chosen as an
alternative to `IArray[(String, Any)]` since the former requires `2n + 3`
objects to be constructed for each field in the JSON object, as opposed to
`3n + 1` in the latter case: for anything but an object of exactly one key, the
former requires fewer objects to be created. Additionally, the types may be
disambiguated reflectively by their erased types.

If parsing fails, a `JsonParseError` will be thrown, including the line and
column in which the error occurs, and an enumeration value (of type
`JsonParseError.Issue`) describing the error.




## Status

Merino
jawn is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Merino
jawn is designed to be _small_. Its entire source code currently consists
of 784 lines of code.

## Building

Merino
jawn can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Merino
jawn are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/merino
jawn/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Merino
jawn easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Merino
jawn was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Merino is named after the variety of sheep known for the quality of their fleeces, in keeping with the trend for
JSON libraries to allude to the story of Jason and the Argonauts.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo represents a ball of wool, potentially merino wool.

## License

Merino
jawn is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
