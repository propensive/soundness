## Base Encoding

### About

Binary data often has to travel through a channel that carries only text — a URL, a JSON string,
an email header. A [base encoding](https://en.wikipedia.org/wiki/Binary-to-text_encoding) maps
bytes onto a safe set of characters so they survive the trip. Soundness serializes bytes to text
and back in Base64, Base32, hexadecimal and the smaller bases, with the encoding named as a type
and the character set — its alphabet — chosen in scope.

### On base encoding

"Base64" is not one thing. The standard alphabet, the URL-safe alphabet, the variants used by MIME,
by bcrypt, by IMAP — all encode the same bytes into different characters, and mixing them corrupts
the result. Decoding raises the further question of what to do with a character that is not in the
alphabet at all. Most APIs bury these choices in flags or fix them silently.

It is worth being precise about what this is, since "encoding" is used for two different jobs.
What happens here is *serialization*: bytes become text, and nothing about the bytes' meaning is
consulted or preserved — the same bytes serialize identically whether they were an image or a
signature. Turning a *value* into JSON or XML is the other job, and it is
[encoding](json.md), which needs to know what the value is. The operations here are named
accordingly, `serialize` and `deserialize`.

Soundness makes the encoding a type and the alphabet an explicit value. The base — how many bits
each character carries — is a type parameter, and the concrete alphabet is a given brought into
scope by import, so the standard and URL-safe forms of Base64 are a one-line difference. Decoding
validates each character and reports an out-of-alphabet one as a typed error. Everything comes from
the `soundness` package:

```scala
import soundness.*
import codepages.utf8Codepage
```

Naming the alphabet as a type parameter, rather than passing a flag, is the [declarative context](../philosophy/declarative-context.md) style used throughout.

### Serializing bytes

`serialize` renders bytes as text in a named encoding, drawing the character set from the alphabet
in scope:

```scala
import alphabets.base64Standard

val bytes = t"Hello".in[Data]
bytes.serialize[Base64]   // t"SGVsbG8="
```

`serialize` works on any value that encodes to bytes, not only raw bytes, so a value with an
`Encodable in Data` instance serializes directly.

### Deserializing

`deserialize` reads text back to bytes in the same encoding. A character outside the alphabet
raises a `Serialization.Error`, so deserializing needs an error strategy in scope:

```scala
import strategies.throwUnsafely

t"SGVsbG8=".deserialize[Base64]   // the bytes of "Hello"
```

### Choosing an alphabet

The alphabet in scope decides the exact characters. `base64Standard` and `base64Url` are the common
choices — the URL-safe form avoids `+` and `/`, which have meaning in a URL:

```scala fuel
alternative Standard
  code
      locally:
        import alphabets.base64Standard
        bytes.serialize[Base64]   // t"SGVsbG8="
alternative URL-safe
  code
      locally:
        import alphabets.base64Url
        bytes.serialize[Base64]   // t"SGVsbG8", unpadded, with `-` and `_` where the standard has `+` and `/`
alternative Hex
  code
      locally:
        import alphabets.hexLowerCase
        bytes.serialize[Hex]      // t"48656c6c6f"
##
```

A *strict* alphabet accepts only its own characters when decoding, while a tolerant one also accepts
recognized equivalents — upper- and lower-case hexadecimal, say — so the strictness of a decode is a
choice between imports: `hexLowerCase` reads either case, `hexStrictLowerCase` only its own.

The full set is in the `alphabets` package:

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

The same `serialize` and `deserialize` handle every base, each with its own alphabets. The base
is the number of bits each character carries:

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
import alphabets.hexLowerCase

bytes.serialize[Hex]   // t"48656c6c6f"
```

### Encoding a stream

An alphabet is also a [stream](streams.md) stage, so bytes are encoded as they flow rather than
gathered first — which is what a base-encoded body written to a socket, or a large file armored
for transport, requires:

```scala
bytes.stream.via(summon[Alphabet[Hex]])
```

The streaming and whole-value forms agree byte for byte, including at the boundaries where a
base-64 group of three bytes or a base-32 group of five straddles two chunks. Decoding runs the
same way, so a stream may be encoded on one side and decoded on the other with neither side
holding the whole of it.
