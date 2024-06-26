### Terminology

Usually the term _serialization_ can mean the transformation of one kind of data
into another sequential output format. And usually, it would be interchangeable
with _encoding_. Likewise, _deserialization_ and _decoding_.

In Soundness, we give more precise meanings to both terms, and profit from
the ability to disambiguate between them: _serialization_ is the conversion from
_bytes_ to _text_ (or a stream of bytes to a stream of text) in formats such as
BASE-64 or hexadecimal, where no semantic meaning is ascribed to the bytes.

Conversely, _encoding_ concerns conversion from values to some other format,
such as JSON or XML. Deserialization and decoding are their respective inverse
operations.

This distinction is made consistently throughout the documentation and naming in
APIs.

### Imports

All Monotonous terms and types are in the `monotonous` package, and exported to
the `soundness` package. If you are using Monotonous alone, without any other
Soundness modules then,
```scala
import monotonous.*
```
or if you are using it with other Soundness modules,
```scala
import soundness.*
```

### Usage

To serialize a `Bytes` value (an instance of `IArray[Byte]`) to BASE-64, we can
use:
```scala
import alphabets.base64.standard
val bytes: Bytes = ???
bytes.serialize[Base64]
```

In general, to convert from a `Bytes` instance—an `IArray[Byte]`—we can call
`serialize` on that value with an output serialization format; one of,
 - `Base64`
 - `Base32`
 - `Hex`
 - `Octal`
 - `Binary`
provided an appropriate contextual `Alphabet` instance is in-scope for that
format.

#### Alphabets

Each of these serializations formats has a number of possible representations. For
example,
- hexadecimal (`Hex`) may be upper-case or lower-case
- BASE-64 requires two or three non-alphanumeric characters to represent all 64
  different output characters, and certain characters are better for certain
  scenarios
- BASE-64 output may also be padded or unpadded (typically with additional `=`
  characters)

In general, these variations are called _alphabets_, since they primarily
specify the full set of valid characters for the serialization format. In the
case of BASE-64, they also configure the presence or absence of padding. (This
is not a feature of _alphabets_ in the real world, but it makes sense to include
this parameter in the `Alphabet` type.)

An `Alphabet` instance can also specify whether any other characters (which are
_not_ in the alphabet) should be considered "equivalent" to others that _are_ in
the alphabet. For example, `l` and `1` are easy to transcribe incorrectly. If an
alphabet contains, for example, `l`, then it might be helpful to specify that
`1` refers to the same index.

Monotonous provides a variety of different alphabets for different serializations.
While each of these has a unique, deterministic serialization (e.g. `hex.upperCase`
always uses the letters `A`-`F`), for deserialization the BASE-32 and BASE-64
alphabets are _tolerant_ by default.

That means that `a5bc29ae` can be deserialized successfully using the `hex.upperCase`
alphabet, even though that alphabet would serialize the same data as `A5BC29AE`.
Alphabets which are not tolerant are labelled `strict`, for example,
`base32.strictLowerCase`.

Monotonous provides implementations of most commonly-used alphabets for each
output format. These are:
- `base64`
  - `standard`
  - `unpadded`
  - `url`
  - `xml`
  - `imap`
  - `yui`
  - `radix64`
  - `bcrypt`
  - `sasl`
  - `uuencoding`
- `base32`
  - `strictUpperCase`
  - `strictLowerCase`
  - `upperCase`
  - `lowerCase`
  - `extendedHexUpperCase`
  - `extendedHexLowerCase`
  - `zBase32`
  - `zBase32Unpadded`
  - `geohash`
  - `wordSafe`
  - `crockford`
- `hex`
  - `strictUpperCase`
  - `strictLowerCase`
  - `upperCase`
  - `lowerCase`
  - `bioctal`
- `octal`
  - `standard`
- `quaternary`
  - `standard`
  - `dnaNucleotide`
- `binary`
  - `standard`

and are all available in the `alphabets` package.
