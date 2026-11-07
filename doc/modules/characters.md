## Characters

### About

The boundary between text and bytes is a [character encoding](https://en.wikipedia.org/wiki/Character_encoding),
and Soundness makes the choice of encoding explicit and typed. A `Codepage` turns text into
bytes, a `Charset` turns bytes back into text, and each is a contextual value chosen by
import, so no conversion happens under an assumed default. What to do with bytes that cannot be
decoded is a separate, equally explicit choice.

Alongside encodings come the character-level facts a program sometimes needs: how many columns a
character occupies in a terminal — an "east Asian wide" character takes two — Unicode names and
properties, superscript and subscript forms, and the [grapheme-cluster](https://unicode.org/reports/tr29/)
boundaries that say where one user-perceived character ends and the next begins.

### On characters

Encodings fail, and most APIs hide it. Decoding bytes with the platform default charset silently
replaces anything malformed, so corrupt input turns into question marks somewhere downstream, and
nothing in the code records which encoding was assumed. Meanwhile "one character" is a slippery
idea: a flag emoji is two code points, an accented letter may be one or two, and a wide ideograph
occupies two terminal columns — details that matter the moment text is measured or split.

Naming the encoding as a given, rather than defaulting silently, is [declarative context](../philosophy/declarative-context.md) at the byte/text boundary.

Soundness separates the choices. The encoding is a given, named at the use site; the treatment of
undecodable bytes is another given, the *sanitizer*, so tolerating, substituting or rejecting bad
input is a decision the code states; and width is measured through a metric chosen for the
context. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Encoding and decoding

An encoding is brought into scope by import, and the conversion happens where text meets bytes —
explicitly, or implicitly wherever a [stream](streams.md) operation crosses the boundary:

```scala
import codepages.utf8Codepage
import charsets.utf8Charset
import textSanitizers.strictSanitizer

val bytes = t"café".in[Data]   // UTF-8 bytes
bytes.utf8                 // t"café"
```

UTF-8, UTF-16 (in both byte orders), ASCII and ISO-8859-1 are provided; any encoding the JVM
knows can be named with the `enc"…"` interpolator, which checks at compiletime that the encoding
exists — and, when an encoder is asked for, that the encoding can encode as well as decode:

```scala
enc"UTF-8".encoder
enc"ABCDEF"   // does not compile: no such encoding
```

### Bad input

A decoder consults the `TextSanitizer` in scope when it meets bytes that are not valid in its
encoding. Three sanitizers decide between failing, dropping and replacing; on the truncated
sequence below they give:

```tabular
column  Sanitizer
column  On bad bytes
column  Result on `badUtf8`
row  `strictSanitizer`  raises a `Charset.Error` naming the position of the fault  `Charset.Error(1, enc"UTF-8")`
row  `skipSanitizer`  drops them  `t"-10"`
row  `substituteSanitizer`  replaces them with `?`  `t"-?10"`
```

```scala
import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics

val badUtf8 = Data(45, -62, 49, 48)   // a truncated two-byte sequence

locally:
  import textSanitizers.skipSanitizer
  charsets.utf8Charset.decoded(badUtf8)   // t"-10"

locally:
  import textSanitizers.substituteSanitizer
  charsets.utf8Charset.decoded(badUtf8)   // t"-?10"

locally:
  import textSanitizers.strictSanitizer
  capture[Charset.Error](charsets.utf8Charset.decoded(badUtf8))
  // Charset.Error(1, enc"UTF-8")
```

Which behavior is right depends on the data: strictness for input that should be trusted
absolutely, tolerance for text recovered from a lossy source. The choice is visible at the
import.

A fourth choice keeps both: `accrueSanitizer` carries on decoding, as the skipping one does, but
records each fault so that the whole of a damaged input is recovered *and* every bad sequence is
reported, each with the position at which it occurred. The faults accrue into a value of the
caller's choosing — here, an [error](errors.md) that collects positions and faults — through
`validate`, whose first block says how each fault joins the accumulation and whose `protect`
block does the decoding:

```scala
import dysasymptotics.linearSize

case class DecodeIssues(items: List[(Int, Charset.Error)] = Nil)(using Diagnostics)
extends Error(m"${items.size} decoding issues"):
  def +(position: Int, error: Charset.Error): DecodeIssues =
    DecodeIssues(items :+ (position, error))

validate[Charset.Focus](DecodeIssues()):
  case error: Charset.Error => accrual + (prior.let(_.position).or(0), error)
. protect:
    import textSanitizers.accrueSanitizer
    charsets.utf8Charset.decoded(badUtf8)   // t"-10", and one recorded issue at position 1
```

This is what a tool importing a file of uncertain provenance wants: the text, plus a list of
where it was wrong, rather than a choice between the two.

### Encodings

`codepages` and `charsets` provide the encodings a program is likely to need, each as a named
given, so the encoding a piece of code uses is stated at its import rather than defaulted from the
platform:

```tabular
column  Encoding
column  Codepage, for encoding
column  Charset, for decoding
row  UTF-8  `utf8Codepage`  `utf8Charset`
row  UTF-16  `utf16Codepage`  `utf16Charset`
row  UTF-16, little-endian  `utf16LeCodepage`  `utf16LeCharset`
row  UTF-16, big-endian  `utf16BeCodepage`  `utf16BeCharset`
row  ASCII  `asciiCodepage`  `asciiCharset`
row  ISO-8859-1  `iso88591Codepage`  `iso88591Charset`, which always skips bad bytes
```

Every other charset takes the sanitizer in scope. An encoding named at runtime is looked up with the `enc"…"` interpolator, which checks
the name as the code compiles:

```scala
import codepages.utf8Codepage

enc"UTF-8".encoder
```

Encoding to bytes and decoding back also work as [stream](streams.md) stages, and a multi-byte
character split across two chunks is carried over the boundary rather than corrupted — including
a surrogate pair split between chunks, which is where naive implementations lose a character.

### Character widths

A character's width in a monospaced display is not always one column. The `metrics` extension
measures a character or a text through the `Measurable` metric in scope, one of three in
`textMetrics`:

```tabular
column  Metric
column  Two columns
column  No columns
column  One column
row  `uniformMetric`  none  none  every character
row  `eastAsianScriptsMetric`  wide and full-width characters  none  every other character
row  `wideCharacterWidthMetric`  wide and full-width characters  control characters, combining and enclosing marks, format characters  every other character
```

```scala
import textMetrics.eastAsianScriptsMetric

'a'.metrics    // 1
'身'.metrics   // 2
```

This metric is what text [padding and fitting](text.md) measure with, so a column of Japanese
text aligns correctly where counting characters would not.

The choice is also a performance one, which is why it is not simply always correct-by-default.
Measuring a text under `uniformMetric` is its length, in constant time; measuring it under
`eastAsianScriptsMetric` means inspecting every character and summing. Where the text is known to
hold nothing wide, the uniform metric gives the same answer for nothing.

### Unicode properties

A character answers questions about itself, reports its Unicode name, and gives its superscript
and subscript forms where Unicode defines them:

```tabular
column  Member
column  Returns
row  `whitespace`  whether it is whitespace
row  `control`  whether it is an ISO control character
row  `printable`  whether it is neither whitespace nor a control character
row  `letter`, `digit`, `alphanumeric`  whether it is a letter, a digit, or either
row  `majuscular`, `minuscular`  whether it is upper-case or lower-case
row  `majuscule`, `minuscule`  its upper-case or lower-case form
row  `description`, `designation`  its Unicode name, an `Optional[Text]`
row  `superscript`, `subscript`  its superscript or subscript form, an `Optional[Char]`
```

```scala
'é'.description     // t"Latin Small Letter E With Acute", an Optional
'\t'.control        // true
'2'.superscript     // '²'
```

### Grapheme clusters

What a reader perceives as one character may be several code points — a flag, an emoji sequence,
a combining accent. `GraphemeBreak.boundaries` finds the positions where one user-perceived
character ends and the next begins, following the Unicode segmentation rules:

```scala
GraphemeBreak.boundaries(t"🇬🇧🇫🇷")   // Array(0, 4, 8): two graphemes, four code points
```

Code that truncates or reverses text at grapheme boundaries, rather than at arbitrary code
points, never splits a character in half.
