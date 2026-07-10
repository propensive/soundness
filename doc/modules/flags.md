## Bit Flags

### About

A set of on-or-off options packs into the bits of a single `Long`, and `Flags` gives that packing a
typed surface: `Flags[Color]` is a set of flags over the cases of the `Color` enumeration, each
tested and set by its case rather than by a magic bit number. The value stays one machine word —
free to copy, cheap to store, natural in a binary format — while reading as a set.

### On flag sets

Bit flags are the oldest data structure in systems programming: `flags & 0x04` and `flags | 0x10`,
with the meaning of each bit recorded in a comment, if anywhere. The compact representation is
worth keeping — a `Set[Color]` costs an allocation and pointers for what one word can hold — but
the untyped access is not: nothing stops testing a bit that means something else, or a bit that
means nothing.

`Flags` keeps the word and types the bits by an enumeration. Each case owns a bit position by its
ordinal; testing and setting use the case itself; and the whole value converts to an ordinary `Set`
when set-like operations are wanted. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Using flags

An empty flag set is created for an enumeration, individual flags are set and cleared by
assignment, and tested by application:

```scala
enum Color:
  case Red, Orange, Yellow, Green, Blue, Indigo, Violet

import Color.*

var flags = Flags[Color]()
flags = flags(Orange) = true
flags = flags(Green) = true

flags(Orange)   // true
flags(Red)      // false

flags = flags(Green) = false
```

The value is immutable — each assignment yields a new `Flags` — so flag sets pass between threads
and into data structures as safely as the `Long` they are.

### As a set

`set` reads the flags out as an ordinary `Set` of the enumeration's cases, for iteration or
set-algebra:

```scala
flags.set   // Set(Orange)
```

Since the flags occupy single bits of a `Long`, an enumeration of up to 64 cases packs into one
word — the representation binary formats and foreign interfaces expect, gained without giving up
the types.
