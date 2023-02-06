[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/merino/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/merino/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Merino

_Merino_ is a JSON parser, and nothing more. It provides just a single `parse`
method that reads a stream of bytes and returns structured data.

## Features

- bare-bones JSON parsing
- provides a single `parse` method
- produces "raw" output, mostly as primitive types
- optimized for performance


## Availability

Merino has not yet been published as a binary, though work is ongoing to fix this.

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



## Related Projects

The following _Scala One_ libraries are dependencies of _Merino_:

[![Galilei](https://github.com/propensive/galilei/raw/main/doc/images/128x128.png)](https://github.com/propensive/galilei/) &nbsp; [![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/) &nbsp; [![Probably](https://github.com/propensive/probably/raw/main/doc/images/128x128.png)](https://github.com/propensive/probably/) &nbsp;

The following _Scala One_ libraries are dependents of _Merino_:

[![Jacinta](https://github.com/propensive/jacinta/raw/main/doc/images/128x128.png)](https://github.com/propensive/jacinta/) &nbsp;

## Status

Merino is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Merino is designed to be _small_. Its entire source code currently consists
of 612 lines of code.

## Building

Merino can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Merino are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/merino/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Merino easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Merino was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Merino is named after the variety of sheep known for the quality of their fleeces, in keeping with the trend for
JSON libraries to allude to the story of Jason and the Argonauts.

## License

Merino is copyright &copy; 2022-23 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
