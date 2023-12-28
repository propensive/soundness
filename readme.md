[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/gesticulate/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/gesticulate/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Gesticulate

__Safe representations of MIME types__

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


## Availability

Gesticulate has not yet been published as a binary.

## Getting Started

### `MediaType`

All terms and types are defined in the `gesticulate` package:
```scala
import gesticulate.*
```

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
import gossamer.t

val mediaType = MediaType(
  group = Media.Group.Application,
  subtype = Media.Subtype.Standard(t"xhtml"),
  suffixes = List(Media.Suffix.Xml),
  parameters = List(t"charset" -> t"UTF-8")
)
```

However, this may be expressed as, `media"application/xhtml+xml; charset=UTF-8"`, and Gesticulate
will statically parse, check and destructure the type into the `MediaType` instance above.

`MediaType` reimplements `toString` to render a media type as a `String`, and additionally provides
the method `basic` to provide the media type with any parameters removed. This may be useful in
some comparisons.

### Checking Registered Types

Media types are checked against a recent copy of IANA's list of registered types, generated from the
lists [published online](https://www.iana.org/assignments/media-types/media-types.xhtml), and
stored on the classpath at `/gesticulate/media.types`. This file is distributed with each published
release of Gesticulate, but may be supplanted by an alternative list appearing in the classpath. The
format for the file is a newline delimited list of media types including suffixes, but excluding
parameters.

### Parsing

Media types may be parsed using `MediaType.parse(string)` which returns a `MediaType` or throws an
`InvalidMediaTypeError`. The `InvalidMediaTypeError.Nature` type encodes different varieties of
parsing failure, should it be useful to distinguish between these.



## Status

Gesticulate is classified as __maturescent__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Gesticulate is designed to be _small_. Its entire source code currently consists
of 262 lines of code.

## Building

Gesticulate can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Gesticulate are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/gesticulate/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Gesticulate easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Gesticulate was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Since _Gesticulate_ provides a representation for MIME types, it's appropriate that miming involves gesticulation.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows a simple fast-forward symbol, familiar from playing many kinds of media, since _media types_ are the subject of Gesticulate.

## License

Gesticulate is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
