## UUIDs

### About

A [UUID](https://en.wikipedia.org/wiki/Universally_unique_identifier) is a 128-bit identifier,
unique enough to assign without coordination. It is represented as a `Uuid` value. A fresh one
is generated, a known one is written as a literal which the compiler checks, and one is parsed
from text with a typed error on failure. Its bytes and bits are available when they are needed.

### On UUIDs

A UUID is almost always carried as a string, and a string is an unreliable home for it. A
literal with a wrong digit, or a value which is not a UUID at all, cannot be told from a valid
one until something parses it and fails. Nothing in the type marks a particular string as a
well-formed identifier.

A `Uuid` is a value which is known to be well-formed. A literal is validated at [compiletime].
Parsing at [runtime] reports a typed `Uuid.Error`, and never returns a broken value. A UUID
which cannot be constructed malformed is
[safety by construction](../philosophy/safety-by-construction.md) in its simplest form.

Everything comes from the `soundness` package. The second import lets the samples print their
results:

```scala
import soundness.*
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

The operations, each covered below:

```tabular
column  Operation
column  Gives
row  `Uuid()`  a fresh random identifier
row  `uuid"…"`  a literal identifier, checked as the code compiles
row  `Uuid.parse(text)`  the identifier, or a raised `Uuid.Error`
row  `Uuid.extract(text)`  an `Optional` identifier; `Uuid(id)` is the same as a pattern
row  `id.text`  the canonical text
row  `id.in[Data]`  the sixteen bytes, the most significant half first
row  `id.msb`, `id.lsb`  the most and least significant 64 bits
row  `a ^ b`, `~a`  the exclusive-or of two identifiers, and the inverse of one
```

### Generating

`Uuid()` produces a fresh random identifier, different each time:

```scala
val fresh = Uuid()
```

### Literals

The `uuid"…"` [interpolator] writes a known identifier, and checks it at compiletime:

```scala
val id = uuid"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c"
```

A malformed literal does not compile:

```scala fuel
compile
errors show
##
uuid"a0cb16f0-d41e-4c28-862f-bd6164bbccXY"
```

### Parsing

Text which arrives at runtime may not be a UUID, so `Uuid.parse` raises a `Uuid.Error` on
failure, which the [strategy] in scope handles:

```scala
import strategies.throwUnsafely
```

```scala fuel
run
##
Out.println(Uuid.parse(t"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c").text)
```

```scala fuel
run
errors show
##
Uuid.parse(t"not-a-uuid")
```

`Uuid.extract` gives an [optional](optional-values.md) value instead, and the same extractor
serves in a pattern:

```scala
def identify(text: Text): Uuid = text match
  case Uuid(id) => id
  case _        => uuid"00000000-0000-0000-0000-000000000000"
```

```scala fuel
run
##
Out.println(Uuid.extract(t"not-a-uuid").inspect)
Out.println(identify(t"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c").text)
Out.println(identify(t"not-a-uuid").text)
```

`as[Uuid]` [decodes](decoding.md#decoding-a-value) text in the same way, so a `Uuid` field in
a case class is read from [JSON](json.md) or a
[command-line parameter](cli.md) with no further code.

### Rendering and bytes

`text` gives a UUID's canonical text. Its sixteen bytes and its two 64-bit halves are also
available:

```scala fuel
run
##
Out.println(id.text)
Out.println(id.in[Data].length.show)
Out.println(id.msb.show)
Out.println(id.lsb.show)
```

### Combining

Two identifiers combine with exclusive-or, and one is inverted with `~`. These are for the
occasions when a UUID is treated as the bit pattern it is, such as deriving a stable identifier
from two others:

```scala
val other = uuid"11111111-2222-3333-4444-555555555555"
```

```scala fuel
run
##
Out.println((id ^ other).text)
Out.println((~id).text)
```

[Hashing](hashing.md) derives an identifier from content, and [Randomness](randomness.md)
controls the source which `Uuid()` draws from.
