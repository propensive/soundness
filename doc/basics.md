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

To serialize a `Bytes` value (an instance of `IArray[Byte]`) to BASE-64, can
use:
```scala
import alphabets.base64.standard
val bytes: Bytes = ???
bytes.serialize[Base64]
```

In general, to convert from a `Bytes` instance—an `IArray[Byte]`—we can call
`serialize` on that value with an output serialization format; one of,
 - `Base32`
 - `Base64`
 - `Binary`
 - `Hex`
provided an appropriate contextual `Alphabet` instance is in-scope for that
format.

#### Alphabets

With the exception of `Binary` (which is always just a series of `0`s and `1`s),
each of these serializations formats has a number of representations. For
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
is not a feature of alphabets in the real world, but it makes sense to include
this parameter in the `Alphabet` type.)

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
  - `standard`
  - `zBase32`
  - `zBase32Unpadded`
- `hex`
  - `upperCase`
  - `lowerCase`
  - `bioctal`
and are all available in the `alphabets` package.
