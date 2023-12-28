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

