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

Soundness makes the encoding a type and the alphabet an explicit value. The base — how many bits
each character carries — is a type parameter, and the concrete alphabet is a given brought into
scope by import, so the standard and URL-safe forms of Base64 are a one-line difference. Decoding
validates each character and reports an out-of-alphabet one as a typed error. Everything comes from
the `soundness` package:

```scala
import soundness.*
import charEncoders.utf8Encoder
```

### Serializing bytes

`serialize` renders bytes as text in a named encoding, drawing the character set from the alphabet
in scope:

```scala
import alphabets.base64Standard

val bytes = t"Hello".data
bytes.serialize[Base64]   // t"SGVsbG8="
```

`serialize` works on any value that encodes to bytes, not only raw bytes, so a value with an
`Encodable in Data` instance serializes directly.

### Deserializing

`deserialize` reads text back to bytes in the same encoding. A character outside the alphabet
raises a `SerializationError`, so deserializing needs an error strategy in scope:

```scala
import strategies.throwUnsafely

t"SGVsbG8=".deserialize[Base64]   // the bytes of "Hello"
```

### Choosing an alphabet

The alphabet in scope decides the exact characters. `base64Standard` and `base64Url` are the common
choices — the URL-safe form avoids `+` and `/`, which have meaning in a URL:

```scala
import alphabets.base64Url

bytes.serialize[Base64]   // URL-safe characters
```

A *strict* alphabet accepts only its own characters when decoding, while a tolerant one also accepts
recognised equivalents — upper- and lower-case hexadecimal, say — so the strictness of a decode is a
choice between imports.

### Other bases

The same `serialize` and `deserialize` handle hexadecimal, Base32, and the smaller octal, quaternary
and binary encodings, each with its own alphabets:

```scala
import alphabets.hexLowerCase

bytes.serialize[Hex]   // t"48656c6c6f"
```
