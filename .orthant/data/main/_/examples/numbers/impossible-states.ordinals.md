# A position is not a number

An `Ordinal` is a position, and an `Int` is a count. Neither is accepted for the other, so a
number becomes a position only by saying where it was counted from, and only the arithmetic
which means something is defined.

```scala fuel
compile
frame
  label A number used as a position
  errors show
  code
      val third: Ordinal = 3
frame
  label A number counted from one
  highlight 3.u  the third, counting from one
  code
      val third: Ordinal = 3.u
frame
  label Two positions multiplied
  errors show
  code
      val third: Ordinal = 3.u
      val ninth = third*third
frame
  label A position moved, and a distance measured
  highlight Int  a distance is a number
    line 3
  code
      val third: Ordinal = 3.u
      val sixth: Ordinal = third + 3
      val distance: Int = sixth - third
##
```

[Ordinals](../../soundness/modules/numbers.md#ordinals) names the first seven, and
[impossible states](../../philosophy/impossible-states.md) has the principle.
