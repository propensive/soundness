[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/propensive/gesticulate/Build/main?style=for-the-badge" height="24">](https://github.com/propensive/gesticulate/actions)
[<img src="https://img.shields.io/maven-central/v/com.propensive/gesticulate-core?color=2465cd&style=for-the-badge" height="24">](https://search.maven.org/artifact/com.propensive/gesticulate-core)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/v7CjtbnwDq)
<img src="/doc/images/github.png" valign="middle">

# Gesticulate

_Gesticulate_ provides a structural representation of media types (previously known as MIME types),
representing their main type (e.g. `image`), subtype (e.g. `png`), suffixes (e.g. `+xml`) and
parameters (e.g. `charset=UTF-8`). It endorses the philosophy that impossible states should be
unrepresentable, which is contravened when media types are represented by strings.

## Features

- statically-checked representations of Media Types (a.k.a. MIME types)
- uses a `media""` string interpolator, e.g. `media"text/plain"`
- checks media types against the official IANA list at compiletime
- supports arbitrary custom `x-`-, `vnd.`- and `prs.`-prefixed types
- checks main media type (e.g. `text` or `image`), and suffixes (e.g. `+gzip` or `+json`)
- supports media type parameters such as `charset=UTF-8`


## Getting Started

## `MediaType`

Gesticulate primarily provides the `MediaType` type, consisting of:
 - a main "type" (called `group` to avoid a conflict with Scala's `type` keyword)
 - a subtype
 - a list of suffixes
 - a list of parameters, as key/value pairs

According to [RFC2046](https://www.iana.org/go/rfc2046), the main type must be one of an
enumerable list of ten types: `application`, `audio`, `image`, `message`, `multipart`, `text`,
`video`, `font`, `example` and `model`. These names are encoded (with capitalized initial letters)
in the enumeration, `Media.Group`.

The subtype may be a standard subtype, from IANA's [registered
list](https://www.iana.org/assignments/media-types/media-types.xhtml), a vendor-specific subtype
(prefixed with `vnd.`) a "personal" subtype (prefixed with `prs.`) or a subtype prefixed with
`x.` or `x-`. The `Media.Subtype` enumeration distinguishes between these four categories.

Zero, one or more suffixes are allowed on any media type, of which 15 possibilities are currently
defined, including the common suffixes, `+xml`, `+json` and `+gzip`. These are enumerated in
`Media.Suffix`.

Parameters may be any key/value pair, and are represented as a `List[(String, String)]`, although
`charset` is the most common example.

The media type for XHTML, encoded as `UTF-8`, which would normally be written as
`application/xhtml+xml; charset=UTF-8` may be represented as the case class instance,
```scala
MediaType(
  group = Media.Group.Application,
  subtype = Media.Subtype.Standard("xhtml"),
  suffixes = List(Media.Suffix.Xml),
  parameters = List("charset" -> "UTF-8")
)
```

However, this may be expressed as, `media"application/xhtml+xml; charset=UTF-8"`, and Gesticulate
will statically parse, check and destructure the type into the `MediaType` instance above.

`MediaType` reimplements `toString` to render a media type as a `String`, and additionally provides
the method `basic` to provide the media type with any parameters removed. This may be useful in
some comparisons.

## Checking Registered Types

Media types are checked against a recent copy of IANA's list of registered types, generated from the
lists [published online](https://www.iana.org/assignments/media-types/media-types.xhtml), and
stored on the classpath at `/gesticulate/media.types`. This file is distributed with each published
release of Gesticulate, but may be supplanted by an alternative list appearing in the classpath. The
format for the file is a newline delimited list of media types including suffixes, but excluding
parameters.

## Parsing

Media types may be parsed using `MediaType.parse(string)` which returns a `MediaType` or throws an
`InvalidMediaTypeError`. The `InvalidMediaTypeError.Nature` type encodes different varieties of
parsing failure, should it be useful to distinguish between these.


## Related Projects

The following _Niveau_ libraries are dependencies of _Gesticulate_:

[![Gossamer](https://github.com/propensive/gossamer/raw/main/doc/images/128x128.png)](https://github.com/propensive/gossamer/) &nbsp;

The following _Niveau_ libraries are dependents of _Gesticulate_:

[![Scintillate](https://github.com/propensive/scintillate/raw/main/doc/images/128x128.png)](https://github.com/propensive/scintillate/) &nbsp;

## Status

Gesticulate is classified as __maturescent__. Propensive defines the following five stability levels for open-source projects:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Gesticulate is designed to be _small_. Its entire source code currently consists of 2193 lines of code.

## Building

Gesticulate can be built on Linux or Mac OS with Vex, by running the `vex` script in the root directory:
```sh
./vex
```

This script will download `vex` the first time it is run, start a daemon process, and run the build. Subsequent
invocations will be near-instantaneous.

## Contributing

Contributors to Gesticulate are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/gesticulate/labels/good%20first%20issue"><img alt="label: good first issue"
src="https://img.shields.io/badge/-good%20first%20issue-67b6d0.svg" valign="middle"></a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Gesticulate easier.

Please __do not__ contact project maintainers privately with questions. While it can be tempting to repsond to
such questions, private answers cannot be shared with a wider audience, and it can result in duplication of
effort.

## Author

Gesticulate was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Since _Gesticulate_ provides a representation for MIME types, it's appropriate that miming involves gesticulation.

## License

Gesticulate is copyright &copy; 2021-22 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
