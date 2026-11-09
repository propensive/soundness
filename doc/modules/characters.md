## Characters

### About

The boundary between text and bytes is a
[character encoding](https://en.wikipedia.org/wiki/Character_encoding), and here the choice of
encoding is explicit and typed. A `Codepage` turns text into bytes, and a `Charset` turns bytes
back into text. Each is a [contextual value], chosen by import, so no conversion ever happens
under an assumed default. What to do with bytes which cannot be decoded is a separate choice,
made just as explicitly.

A program sometimes needs facts about single characters too:

- how many columns a character occupies in a terminal, where an "East Asian wide" character
  takes two;
- a character's Unicode name and properties;
- its superscript and subscript forms;
- the [grapheme cluster](https://unicode.org/reports/tr29/) boundaries, which say where one
  character, as a reader perceives it, ends and the next begins.

### On characters

Encodings fail, and most APIs hide the failure. Decoding bytes with the platform's default
charset silently replaces anything malformed. Corrupt input turns into question marks somewhere
downstream, and nothing in the code records which encoding was assumed.

Meanwhile "one character" is a slippery idea. A flag emoji is two code points. An accented
letter may be one or two. A wide ideograph occupies two columns of a terminal. These details
matter as soon as text is measured or split.

Here the choices are separate, and each is stated:

```tabular
column  Choice
column  Made by
row  the encoding  a `Codepage` or `Charset` given, imported where it is used
row  what happens to undecodable bytes  a `TextSanitizer` given
row  how wide a character is  a metric, chosen for the display
```

Naming each as a [given], instead of defaulting silently, is
[declarative context](../philosophy/declarative-context.md) at the boundary between bytes and
text. Everything comes from the `soundness` package, and the second import lets the samples
print their results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Encoding and decoding

Text cannot become bytes until an encoding has been chosen. There is no default to fall back
on, so without a `Codepage` in scope the conversion does not compile:

```scala fuel
compile
errors show
##
t"café".in[Data]
```

An encoding is brought into scope by import. The conversion then happens where text meets
bytes, either explicitly, as here, or wherever a [stream](streams.md) crosses the boundary:

```scala
import codepages.utf8Codepage
import charsets.utf8Charset
import textSanitizers.strictSanitizer

val bytes = t"café".in[Data]
```

```scala fuel
run
##
Out.println(bytes.length.show)
Out.println(bytes.utf8)
```

The four characters are five bytes, since `é` takes two bytes in UTF-8.

UTF-8, UTF-16 in both byte orders, ASCII and ISO-8859-1 are provided. Any encoding the JVM
knows can be named with the `enc"…"` [interpolator], which checks at [compiletime] that the
encoding exists. When an encoder is asked for, it also checks that the encoding can encode as
well as decode:

```scala
val encoder = enc"UTF-8".encoder
```

```scala fuel
compile
errors show
##
enc"ABCDEF"
```

### Bad input

When a decoder meets bytes which are not valid in its encoding, it consults the `TextSanitizer`
in scope. Three sanitizers decide between failing, dropping and replacing:

```tabular
column  Sanitizer
column  On bad bytes
row  `strictSanitizer`  raises a `Charset.Error` which names the position of the fault
row  `skipSanitizer`  drops them
row  `substituteSanitizer`  replaces them with `?`
```

These four bytes are a hyphen, the first byte of a two-byte sequence whose second byte is
missing, and the digits `1` and `0`:

```scala
import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics

val badUtf8 = Data(45, -62, 49, 48)
```

```scala fuel
run
##
locally:
  import textSanitizers.skipSanitizer
  Out.println(charsets.utf8Charset.decoded(badUtf8))

locally:
  import textSanitizers.substituteSanitizer
  Out.println(charsets.utf8Charset.decoded(badUtf8))

locally:
  import textSanitizers.strictSanitizer
  Out.println(capture[Charset.Error](charsets.utf8Charset.decoded(badUtf8)).message.show)
```

Which behavior is right depends on the data. Strictness suits input which should be trusted
absolutely, and tolerance suits text recovered from a lossy source. Either way, the choice is
visible at the import.

A fourth sanitizer keeps both. `accrueSanitizer` carries on decoding, as the skipping one does,
but records each fault. The whole of a damaged input is recovered, *and* every bad sequence is
reported, with the position at which it occurred.

The faults [accumulate](errors.md#accumulating-failures) into a value of the caller's choosing,
here an error which collects positions and faults. `validate` does the collecting: its first
block says how each fault joins the accumulation, and its `protect` block does the decoding.

```scala
import dysasymptotics.linearSize

case class DecodeIssues(items: List[(Int, Charset.Error)] = Nil)(using Diagnostics)
extends Error(m"${items.size} decoding issues"):
  def +(position: Int, error: Charset.Error): DecodeIssues =
    DecodeIssues(items :+ (position, error))

val issues =
  validate[Charset.Focus](DecodeIssues()):
    case error: Charset.Error => accrual + (prior.let(_.position).or(0), error)
  . protect:
      import textSanitizers.accrueSanitizer
      charsets.utf8Charset.decoded(badUtf8)
```

The result of `validate` is the accumulation, with one entry for the one bad sequence:

```scala fuel
run
##
issues.items.each: (position, error) =>
  Out.println(t"at byte $position: ${error.message}")
```

This is what a tool wants when it imports a file of uncertain origin: the text, and a list of
where it was wrong, not a choice between the two.

### Encodings

The packages `codepages` and `charsets` provide the encodings a program is likely to need, each
as a named given:

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

Every other charset uses the sanitizer in scope.

Encoding and decoding also work as stages of a [stream](streams.md#composing-a-pipeline). A
multi-byte character which is split across two chunks is carried over the boundary, not
corrupted. That includes a surrogate pair split between chunks, which is where a naive
implementation loses a character.

### Character widths

A character is not always one column wide in a monospaced display. The `metrics` extension
measures a character, or a text, through the metric in scope. There are three, in the package
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
```

```scala fuel
run
##
Out.println('a'.metrics.show)
Out.println('身'.metrics.show)
```

Text [padding and fitting](text.md#padding-and-fitting) measure with the same metric, so a
column of Japanese text aligns correctly where counting characters would not.

The choice affects speed as well as correctness, which is why the most thorough metric is not
simply the default. Measuring a text under `uniformMetric` is reading its length, in constant
time. Measuring it under `eastAsianScriptsMetric` means inspecting every character and adding up
the widths. Where a text is known to hold nothing wide, the uniform metric gives the same answer
for nothing.

### Unicode properties

A character answers questions about itself. It reports its Unicode name, and gives its
superscript and subscript forms where Unicode defines them:

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

```scala fuel
run
##
Out.println('é'.description.inspect)
Out.println('\t'.control.inspect)
Out.println('2'.superscript.inspect)
Out.println('é'.superscript.inspect)
```

The last three are [optional](optional-values.md) values. `inspect` displays a present one
between corner brackets, and an absent one, such as the superscript of `é`, as `○`.

### Grapheme clusters

What a reader perceives as one character may be several code points: a flag, an emoji sequence,
a letter with a combining accent. `GraphemeBreak.boundaries` finds the positions where one such
character ends and the next begins, following Unicode's segmentation rules. Two flags are four
code points, and eight `Char`s, but two graphemes:

```scala fuel
run
##
Out.println(GraphemeBreak.boundaries(t"🇬🇧🇫🇷").to[List].inspect)
```

Code which truncates or reverses text at these boundaries never splits a character in half.
[Text](text.md#graphemes-and-width) counts and measures graphemes with `Writing`.

### Related

- [Filesystem](filesystem.md) describes the encodings which file names and contents are read in.
