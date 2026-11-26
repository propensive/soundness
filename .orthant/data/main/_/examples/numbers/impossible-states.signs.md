# Signed and unsigned, never by accident

A `U8` and an `S32` are different types, and neither is silently taken for the other. A
conversion is a short method named for the type it gives, so every change of signedness or
width is written where it happens.

```scala fuel
compile
frame
  label An unsigned value used as a signed one
  errors show
  code
      import scala.language.experimental.genericNumberLiterals

      val players: U8 = 200
      val count: S32 = players
frame
  label The conversion, written
  highlight s32  the wider type holds every U8
  code
      import scala.language.experimental.genericNumberLiterals

      val players: U8 = 200
      val count: S32 = players.s32
##
```

[Numeric types](../../soundness/modules/numbers.md#numeric-types) explains the families of
types and their names.
