## Media Types

### About

A [media type](https://en.wikipedia.org/wiki/Media_type), such as `application/json` or
`text/html; charset=UTF-8`, names what a stream of bytes means. It is represented
structurally: its group, its subtype, its `+suffix`es and its parameters are each a typed
part, not a substring.

The `media"…"` [interpolator] checks a literal against the
[IANA registry](https://www.iana.org/assignments/media-types/media-types.xhtml) at
[compiletime]. A misspelled media type, of the kind which browsers silently mishandle, is a
compile error, with a suggestion of what was probably meant.

The `Media` [typeclass] lets any type declare its own media type. That is how HTTP responses,
served files and email attachments label themselves. Multipart bodies, the format of file
uploads, are parsed into their typed parts.

### On media types

Media types look like free text, and are not. The grammar distinguishes registered types from
vendor trees. Suffixes carry meaning: `svg+xml` *is* XML. Parameters such as `charset` change
how the bytes are interpreted.

Media types are usually handled as strings, so a typo produces a type which looks plausible,
and which no consumer recognizes. Receivers tend to fall back instead of failing, so the
mistake shows up as subtly wrong behavior, far away.

Here media types are parsed fully, a literal is validated against the registry itself, and the
value is typed. Checking a media type as the code compiles is
[safety by construction](../philosophy/safety-by-construction.md), applied to a string which
is otherwise trusted.

Everything comes from the `soundness` package. The samples throw any error as an exception,
and the last import lets them print their results:

```scala
import soundness.*
import strategies.throwUnsafely
import codepages.utf8Codepage
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Writing a media type

The `media"…"` interpolator checks its literal at compiletime. A registered type passes:

```scala
val json = media"application/json"
val epub = media"application/epub+zip"
```

An unregistered type in the standard tree fails, and a registered name close to it is
suggested:

```scala fuel
compile
errors show
##
media"application/jsom"
```

The other trees are accepted without being looked up in the registry, as the standard
prescribes:

```tabular
column  Tree
column  Subtype written
column  As a `Media.Subtype`
column  Checked against the registry
row  standard  `json`  `Standard`  yes
row  vendor  `vnd.…`  `Vendor`  no
row  personal  `prs.…`  `Personal`  no
row  experimental  `x-…`  `X`  no
```

### Structure

A parsed `MediaType` exposes its parts:

```scala
val mediaType = t"application/json; charset=UTF-8".as[MediaType]
```

```scala fuel
run
##
Out.println(mediaType.group.inspect)
Out.println(mediaType.basic)
Out.println(mediaType.at(t"charset").inspect)
```

`basic` is the media type without its parameters. Parameters are added as named arguments:

```scala fuel
run
##
Out.println(media"text/html"(charset = t"UTF-8").show)
```

The four parts are those which [RFC 2046](https://www.iana.org/go/rfc2046) defines:

```tabular
column  Part
column  Field
column  Is
row  group  `group`  one of eleven `Media.Group` values: `application`, `audio`, `image`, `message`, `multipart`, `text`, `video`, `font`, `example`, `model` and `haptics`
row  subtype  `subtype`  registered, vendor, personal or experimental
row  suffixes  `suffixes`  zero or more, such as `+xml`, `+json` and `+gzip`
row  parameters  `parameters`  key–value pairs, `charset` being much the most common
```

The group is called `group`, not `type`, only because `type` is a keyword in Scala. Which of
the four trees a subtype belongs to is part of the value, and is not a matter of reading its
prefix.

Registration is checked against a copy of IANA's published list, carried on the classpath, so
the check needs no network and no service. A newer list, or a list specific to a site, which
is placed earlier on the classpath, supersedes it. The file is a list of media types, one on
each line, with suffixes but without parameters.

Text which is not a media type raises a `MediaType.Error`, which names the reason: a missing
slash, an invalid character, or an unknown suffix.

```scala fuel
run
errors show
##
t"application".as[MediaType]
```

### Types that know their media type

The `Media` typeclass gives a type its media type, so a value can be served, or attached,
without anyone stating again what it is. Text is `text/plain`, and anything with a filename
guesses from its extension. `ascribe` pairs any source of bytes with an explicit media type,
where none is intrinsic. The result is a `Content`: bytes, together with their type.

```scala
val data = t"raw bytes".in[Data]
val content = data.ascribe(media"application/octet-stream")
```

This typeclass is what lets an [HTTP server](http-server.md#responses) return a value, and
have the `Content-Type` header follow automatically. The [HTTP client](http-client.md#headers)
takes a media type as the value of its `accept` header, and [email](email.md#composing)
attachments carry one.

### Detecting a media type

Content often arrives without a trustworthy label, and has to be recognised from its bytes. A
`Media["…"]` pattern does that for a stream — a Zephyrine `Cursor` — and binds a `Detection`:

```scala
import detectables.{gifDetectable, pngDetectable}

val cursor = Cursor(hex"89504e470d0a1a0a0000000d49484452")

cursor match
  case Media["image/png"](png) => t"a PNG"
  case Media["image/gif"](gif) => t"a GIF"
  case _                       => t"something else"
```

Each case reads as much of the stream as it needs and then returns the cursor to where it was
found, whether or not it matched: a later case, and the code which follows the match, see the
whole stream. A detector cannot break this promise, because it never holds the cursor. It reads
through a `Probe`, which can only go forwards, and the pattern rewinds the cursor when the
detector returns or throws.

What a pattern can detect is decided by the `Detectable` instances in scope, each imported by
name from the `detectables` family, as the two above are. The media type is checked against the
registry like any other, and a pattern for which no detector has been imported is a compile
error:

```scala fuel
compile
errors show
##
cursor match
  case Media["image/jpeg"](jpeg) => t"a JPEG"
  case _                         => t"something else"
```

#### How certain a detection is

A few opening bytes rarely prove what a stream is, and a `Detection` says in its type how much
was established. `png` above is a `Detection of "image/png" under Likely to (Raster in Png)`:
content detected as `image/png`, with a certainty of `Likely`, which is read as a `Raster in Png`.
The certainty is one of three, fixed by what the detector examined:

```tabular
column  Certainty
column  What was examined
column  Example
row  `Plausible`  something merely consistent with the format, which reserves no identifying structure or keeps it out of reach  a leading `{` for JSON; a leading `<` for XML
row  `Likely`  the structure which the format's specification designates as identifying, found where the specification puts it  the eight-byte PNG signature; the index at the end of a ZIP file
row  `Certain`  the whole of the content, validated  none is provided: it means reading the entire stream
```

A `Likely` match is wrong only by coincidence or forgery, and says nothing about the rest of the
content being well-formed. A `Plausible` match can be wrong by design: other formats legitimately
begin the same way, so the order of `Plausible` cases matters where the order of `Likely` ones
does not.

Each level is a subtype of the one before it — `Certain <: Likely <: Plausible` — so a demand for
at least a certainty is a bound. A second type argument to the pattern makes that demand of the
detector, and a method makes it of a `Detection` it is given:

```scala
def decode(detection: Detection of "image/png" under (? <: Likely)): Text = t"decoding"

cursor match
  case Media["image/png", Likely](png) => decode(png)
  case _                               => t"something else"
```

The JSON detector is only `Plausible`, so demanding more of it is rejected as the code compiles:

```scala fuel
compile
errors show
##
import detectables.jsonDetectable

cursor match
  case Media["application/json", Likely](json) => t"JSON"
  case _                                       => t"something else"
```

A detection is evidence, not a second handle on the stream: the cursor stays with the code that
matched it, and `png.read(cursor)` reads it as the type the detection names.

#### Formats a stream cannot identify

Some formats are identified by something a forward read cannot reach — a ZIP file by the index
at its end. The same patterns match content with random access, an `Expanse` or `Data` in memory,
and there a detector may look anywhere. A detector declares which access it needs, and one media
type may have both kinds, establishing different amounts. ZIP has: `zipDetectable` reads a stream,
and can only see whether it opens as most archives do, while `zipIndexDetectable` finds the index:

```scala
import detectables.{zipDetectable, zipIndexDetectable}

def identify(content: Data): Text = content match
  case Media["application/zip", Likely](zip) => t"a ZIP archive"
  case _                                     => t"something else"
```

A cursor can use only the first detector, so its `Media["application/zip"]` detection is
`Plausible`, and `Media["application/zip", Likely]` does not compile for it. `Data` and an
`Expanse` can use either, and the pattern takes whichever establishes more: there the same
detection is `Likely`, and an archive which follows a program, as a self-extracting one does, is
still found.

#### Writing a detector

A format which reserves a signature needs only its bytes, and the offset if it is not the start:

```scala
given sqlite: ("application/vnd.sqlite3" is Detectable under Likely over Probe to Data) =
  Detectable.signature(0x53, 0x51, 0x4c, 0x69, 0x74, 0x65)
```

Anything else is a function of a `Probe`, for a stream, or of an `Expanse`:

```scala
given logfile: ("text/vnd.example" is Detectable under Plausible over Probe to Text) =
  Detectable: probe =>
    probe.skipWhitespace()
    probe.next().lay(false)(_ == '['.toByte)

given trailed: ("application/vnd.example" is Detectable under Likely over Expanse to Data) =
  Detectable.positional: expanse =>
    expanse.size >= 4 && expanse.read(expanse.size - 4, 4).utf8 == t"DONE"
```

The type states the certainty, and the detector must earn it: `Likely` only for the structure the
format's specification designates as identifying, and `Certain` only for one which reads and
validates everything — which, on a stream, means holding all of it in memory until the detector
returns.

### Multipart

`multipart/form-data` is the format of file uploads. It is parsed into a `Multipart` of typed
`Part`s. Each part has its disposition, its name, an optional filename, its headers, and a
stream for its body. It is parsed incrementally, so a large upload streams, and does not
accumulate:

```scala
val body =
  t"--xyz\r\n" +
  t"Content-Disposition: form-data; name=\"avatar\"; filename=\"me.png\"\r\n" +
  t"\r\n" +
  t"pixels\r\n" +
  t"--xyz--\r\n"

val upload = Multipart.parse(Chain(body.in[Data]))
```

```scala fuel
run
##
Out.println(upload.at(t"avatar").let(_.filename).inspect)
```

A part's body is lent from the input, without copying. It must be read, with
`part.read[Data]`, or with `part.body()` for the stream, before moving on to the next part.
After that, whatever remained of it has been skipped, and it reads as empty.

### Related

- [Audio](audio.md), [images](images.md), [interpolation](interpolation.md) and [OpenAPI](openapi.md) name their formats by media type.
