## Media Types

### About

A [media type](https://en.wikipedia.org/wiki/Media_type) — `application/json`,
`text/html; charset=UTF-8` — names what a stream of bytes means, and Soundness represents one
structurally: its group, its subtype, its `+suffix`es and its parameters, each a typed part rather
than a substring. The `media"…"` interpolator checks a literal against the
[IANA registry](https://www.iana.org/assignments/media-types/media-types.xhtml) as the code
compiles, so a misspelled media type — the kind browsers silently mishandle — is a compile error,
with a suggestion for what was probably meant.

The `Media` typeclass lets any type declare its own media type, which is how HTTP responses,
served files and email attachments label themselves; and multipart bodies — the format of file
uploads — parse into their typed parts.

### On media types

Media types look like free text and are not: the grammar distinguishes registered types from
vendor trees, suffixes carry meaning (`svg+xml` *is* XML), and parameters like `charset` change
interpretation. Because they are usually handled as strings, a typo produces a syntactically
plausible type that no consumer recognises — and since receivers tend to fall back rather than
fail, the mistake shows up as subtly wrong behaviour far away.

Soundness parses them fully, validates literals against the registry itself, and types the value.
Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Writing a media type

The `media"…"` interpolator checks its literal at compiletime. A registered type passes; an
unregistered one in the standard tree fails, with a nearby registered name suggested; vendor
(`vnd.`), personal (`prs.`) and `x-` subtypes are accepted without registry lookup, as the
standard prescribes:

```scala
media"application/json"
media"application/epub+zip"
media"application/jsom"   // does not compile: did you mean application/json?
```

### Structure

A parsed `MediaType` exposes its parts, and parameters are added as named arguments:

```scala
val mediaType = t"application/json; charset=UTF-8".decode[MediaType]

mediaType.group      // Media.Group.Application
mediaType.basic      // t"application/json" — without parameters
mediaType.at(t"charset")   // t"UTF-8"

media"text/html"(charset = t"UTF-8")
```

Text that is not a media type raises a `MediaTypeError` naming the reason — a missing slash, an
invalid character, an unknown suffix.

### Types that know their media type

The `Media` typeclass gives a type its media type, so a value can be served or attached without
anyone re-stating what it is: text is `text/plain`, and anything with a filename guesses from its
extension. `ascribe` pairs any byte source with an explicit media type where none is intrinsic:

```scala
data.ascribe(media"application/octet-stream")   // a Content: bytes plus their type
```

This typeclass is what lets an [HTTP](http-server.md) handler return a value and have the
`Content-Type` header follow automatically.

### Multipart

The `multipart/form-data` format of file uploads parses into a `Multipart` of typed `Part`s, each
with its disposition, name, optional filename, headers and body stream — parsed incrementally, so
a large upload streams rather than accumulating:

```scala
val upload = Multipart.parse(stream)
upload.at(t"avatar").let(_.filename)   // the uploaded file's name, if present
```
