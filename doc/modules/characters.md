## Characters

### About

The boundary between text and bytes is a [character encoding](https://en.wikipedia.org/wiki/Character_encoding),
and Soundness makes the choice of encoding explicit and typed. A `CharEncoder` turns text into
bytes, a `CharDecoder` turns bytes back into text, and each is a contextual value chosen by
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
import charEncoders.utf8Encoder
import charDecoders.utf8Decoder
import textSanitizers.strictSanitizer

val bytes = t"café".data   // UTF-8 bytes
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
encoding. The strict sanitizer raises a `CharDecodeError` naming the position of the fault; the
skip sanitizer drops the bad bytes; and the substitute sanitizer replaces them with `?`:

```scala
import strategies.throwUnsafely

val badUtf8 = Data(45, -62, 49, 48)   // a truncated two-byte sequence

locally:
  import textSanitizers.skipSanitizer
  charDecoders.utf8Decoder.decoded(badUtf8)   // t"-10"

locally:
  import textSanitizers.substituteSanitizer
  charDecoders.utf8Decoder.decoded(badUtf8)   // t"-?10"

locally:
  import textSanitizers.strictSanitizer
  capture[CharDecodeError](charDecoders.utf8Decoder.decoded(badUtf8))
  // CharDecodeError(1, enc"UTF-8")
```

Which behaviour is right depends on the data: strictness for input that should be trusted
absolutely, tolerance for text recovered from a lossy source. The choice is visible at the
import.

### Character widths

A character's width in a monospaced display is not always one column. The `metrics` extension
measures a character or a text through the `Measurable` metric in scope: `uniformMetric` counts
every character as one, while `eastAsianScriptsMetric` gives wide characters two columns:

```scala
import textMetrics.eastAsianScriptsMetric

'a'.metrics    // 1
'身'.metrics   // 2
```

This metric is what text [padding and fitting](text.md) measure with, so a column of Japanese
text aligns correctly where counting characters would not.

### Unicode properties

A character answers questions about itself — whether it is whitespace, a control character, or
printable — and reports its Unicode name; superscript and subscript forms are available where
Unicode defines them:

```scala
'é'.description     // its Unicode character name
'\t'.control        // true
'2'.superscript     // '²'
```

### Grapheme clusters

What a reader perceives as one character may be several code points — a flag, an emoji sequence,
a combining accent. `GraphemeBreak.boundaries` finds the positions where one user-perceived
character ends and the next begins, following the Unicode segmentation rules:

```scala
GraphemeBreak.boundaries(t"🇬🇧🇫🇷")   // two graphemes, four code points
```

Code that truncates or reverses text at grapheme boundaries, rather than at arbitrary code
points, never splits a character in half.
