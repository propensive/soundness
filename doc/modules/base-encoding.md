## Base Encoding

### About

Binary data often has to travel through a channel which carries only text: a URL, a JSON
string, an email header. A
[base encoding](https://en.wikipedia.org/wiki/Binary-to-text_encoding) maps bytes onto a safe
set of characters, so that they survive the trip.

Bytes are serialized to text, and back, in Base64, Base32, hexadecimal and the smaller bases.
The encoding is named as a type, and its set of characters, called its *alphabet*, is chosen
in scope.

### On base encoding

"Base64" is not one thing. The standard alphabet, the URL-safe alphabet, and the variants used
by MIME, by bcrypt and by IMAP all encode the same bytes into different characters, and mixing
them corrupts the result. Decoding raises a further question: what to do with a character
which is not in the alphabet at all. Most APIs bury these choices in flags, or fix them
silently.

Here the encoding is a type, and the alphabet is an explicit value:

- The base, which is how many bits each character carries, is a type parameter.
- The alphabet is a [given], brought into scope by import, so the standard and the URL-safe
  forms of Base64 differ by one line.
- Decoding validates each character, and reports one which is outside the alphabet as a typed
  error.

Naming the alphabet by import, instead of passing a flag, is the
[declarative context](../philosophy/declarative-context.md) style used throughout.

The word "encoding" is used for two different jobs, and it is worth being precise about which
this is. What happens here is *serialization*. Bytes become text, and nothing about the
meaning of the bytes is consulted or preserved: the same bytes serialize identically, whether
they were an image or a signature. Turning a *value* into JSON or XML is the other job. It is
[encoding](json.md), and it needs to know what the value is. The operations here are named
accordingly: `serialize` and `deserialize`.

Everything comes from the `soundness` package. The last import lets the samples print their
results:

```scala
import soundness.*
import codepages.utf8Codepage
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Serializing bytes

`serialize` writes bytes as text in a named encoding. The characters come from the alphabet in
scope:

```scala
import alphabets.base64Standard

val bytes = t"Hello".in[Data]
```

```scala fuel
run
##
Out.println(bytes.serialize[Base64])
```

`serialize` works on any value which encodes to bytes, not only on raw bytes. A value with an
`Encodable in Data` instance, such as a [digest](hashing.md), is serialized directly.

With no alphabet for the base in scope, there is nothing to serialize with:

```scala fuel
compile
errors show
##
bytes.serialize[Base32]
```

### Deserializing

`deserialize` reads text back to bytes, in the same encoding. A character outside the alphabet
raises a `Serialization.Error`, so deserializing needs a [strategy] in scope:

```scala
import strategies.throwUnsafely
```

```scala fuel
run
##
Out.println(t"SGVsbG8=".deserialize[Base64].utf8)
```

```scala fuel
run
errors show
##
t"SGVsbG8!".deserialize[Base64]
```

### Choosing an alphabet

The alphabet in scope decides the exact characters. `base64Standard` and `base64Url` are the
common choices. The URL-safe form is unpadded, and avoids `+` and `/`, which have a meaning in
a URL:

```scala fuel
run
##
locally:
  import alphabets.base64Url
  Out.println(bytes.serialize[Base64])

locally:
  import alphabets.hexLowerCase
  Out.println(bytes.serialize[Hex])
```

A *strict* alphabet accepts only its own characters when decoding. A tolerant one also accepts
recognized equivalents, such as hexadecimal in either case. The strictness of a decode is
therefore a choice between imports: `hexLowerCase` reads either case, and `hexStrictLowerCase`
only its own.

The full set is in the package `alphabets`:

```tabular
column  Base
column  Given
column  Variant
column  Padded
column  Also accepts
row  `Base64`  `base64Standard`  standard  yes
row  `Base64`  `base64Unpadded`  standard, unpadded  no
row  `Base64`  `base64Url`  URL-safe, with `-` and `_`  no
row  `Base64`  `base64Xml`  XML, with `+` and `-`, padded with `.`  yes
row  `Base64`  `base64Imap`  IMAP, with `+` and `,`  no
row  `Base64`  `base64Yui`  YUI, with `.` and `_`  no
row  `Base64`  `base64Radix64`  Radix-64  yes
row  `Base64`  `base64Bcrypt`  bcrypt, with `.` and `/` first  no
row  `Base64`  `base64Sasl`  SASL, with `+` and `,`  no
row  `Base64`  `base64Uuencoding`  uuencoding  no
row  `Base32`  `base32StrictUpperCase`  standard, upper-case  yes
row  `Base32`  `base32StrictLowerCase`  standard, lower-case  yes
row  `Base32`  `base32UpperCase`  standard, upper-case  yes  lower case
row  `Base32`  `base32LowerCase`  standard, lower-case  yes  upper case
row  `Base32`  `base32ExtendedHexUpperCase`  extended hex, upper-case  yes  standard lower-case Base32
row  `Base32`  `base32ExtendedHexLowerCase`  extended hex, lower-case  yes  standard upper-case Base32
row  `Base32`  `base32ZBase32`  z-base-32  yes
row  `Base32`  `base32ZBase32Unpadded`  z-base-32, unpadded  no
row  `Base32`  `base32Geohash`  Geohash  no
row  `Base32`  `base32WordSafe`  word-safe  no
row  `Base32`  `base32Crockford`  Crockford  no  lower case; `o` and `O` as `0`; `i`, `I` and `L` as `1`
row  `Hex`  `hexStrictUpperCase`  upper-case  no
row  `Hex`  `hexStrictLowerCase`  lower-case  no
row  `Hex`  `hexUpperCase`  upper-case  no  lower case
row  `Hex`  `hexLowerCase`  lower-case  no  upper case
row  `Hex`  `hexBioctal`  bioctal  no
row  `Octal`  `octalStandard`  standard  no
row  `Quaternary`  `quaternaryStandard`  `0123`  no
row  `Quaternary`  `quaternaryDnaNucleotide`  DNA nucleotides, `ATCG`  no
row  `Binary`  `binaryStandard`  `01`  no
```

### Other bases

The same `serialize` and `deserialize` handle every base, each with alphabets of its own. The
base is the number of bits which each character carries:

```tabular
column  Encoding
column  Bits per character
row  `Base64`  6
row  `Base32`  5
row  `Hex`  4
row  `Octal`  3
row  `Quaternary`  2
row  `Binary`  1
```

```scala
import alphabets.{hexLowerCase, binaryStandard, quaternaryDnaNucleotide}
```

```scala fuel
run
##
Out.println(bytes.serialize[Hex])
Out.println(bytes.serialize[Binary])
Out.println(bytes.serialize[Quaternary])
```

A hexadecimal literal for bytes is written with the `hex"…"` interpolator, as
[binary data](binary-data.md#reading) shows.

### Encoding a stream

An alphabet is also a stage of a [stream](streams.md#composing-a-pipeline). Bytes are encoded
as they flow, without being gathered first, which is what a base-encoded body written to a
socket requires, or a large file prepared for transport as text:

```scala
val encoded = bytes.stream.via(summon[Alphabet[Hex]])
```

The streaming form and the whole-value form agree byte for byte. That includes the boundaries
where a Base64 group of three bytes, or a Base32 group of five, straddles two chunks. Decoding
runs in the same way, so a stream may be encoded on one side and decoded on the other, with
neither side holding the whole of it.

### Related

- [CBOR](cbor.md), [cryptography](cryptography.md), [SSH](ssh.md), [geography](geography.md) and [text](text.md) encode bytes and coordinates in these alphabets.
