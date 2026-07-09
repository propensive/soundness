## UUIDs

### About

A [UUID](https://en.wikipedia.org/wiki/Universally_unique_identifier) is a 128-bit identifier,
unique enough to assign without coordination. Soundness represents one as a `Uuid` value:
generate a fresh one, write one literally with the compiler checking it, parse one from text with
a typed error on failure, and reach its bytes or bits when needed.

### On UUIDs

A UUID is almost always carried as a string, and a string is an unreliable home for it. A literal
with a wrong digit, or a value that is not a UUID at all, is indistinguishable from a valid one
until something parses it and fails. Nothing marks, in the type, that a particular string is a
well-formed identifier.

A `Uuid` is a value that is known to be well-formed. A literal is validated as the code compiles,
runtime parsing reports a typed `UuidError` rather than returning a broken value, and the
identifier's 128 bits are available directly for the occasional need to inspect or combine them.
Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Generating

`Uuid()` produces a fresh random identifier:

```scala
val id = Uuid()
```

### Literals

The `uuid"…"` interpolator writes a known identifier and checks it as the code compiles, so a
malformed literal is a compile error rather than a runtime surprise:

```scala
uuid"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c"
```

### Parsing

Text parsed at runtime may not be a UUID, so `Uuid.parse` reports a `UuidError` on failure,
handled by the strategy in scope; `Uuid.extract` instead yields an `Optional`, and the same
extractor serves in a pattern:

```scala
import strategies.throwUnsafely

Uuid.parse(t"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c")   // a Uuid, or raises UuidError

t"not-a-uuid" match
  case Uuid(id) => id      // a well-formed identifier
  case _        => Uuid()  // fall back to a fresh one
```

### Rendering and bytes

A UUID renders to its canonical text with `text`, and exposes its sixteen bytes and its two
64-bit halves:

```scala
id.text    // t"a0cb16f0-d41e-4c28-862f-bd6164bbcc8c"
id.bytes   // a 16-byte Data value
id.msb     // the most-significant 64 bits
```

### Combining

Two identifiers combine with exclusive-or, and one inverts with `~`, for the occasions — deriving
a stable identifier from two others, say — where a UUID is treated as the bit pattern it is:

```scala
val a = Uuid()
val b = Uuid()

a ^ b   // the bitwise combination
~a      // the inverse
```
