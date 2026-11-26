# Feet and inches, as a type

A height in feet and inches is one value in a cascade of units. The type states the cascade,
smallest unit first and the larger ones after `in`, and every unit in it must measure the same
dimension.

```scala fuel
compile
frame
  label A cascade of two units
  highlight Quanta[Inches[1]]..(Feet[1])  inches, carried into feet
  code
      type Height = Quanta[Inches[1]] in (Feet[1])

      val falstaff: Height = Quanta(5, 9)
frame
  label Carrying between them
  highlight falstaff..Quanta(4)  nine inches and four
    line 4
  code
      type Height = Quanta[Inches[1]] in (Feet[1])

      val falstaff: Height = Quanta(5, 9)
      val mounted: Height = falstaff + Quanta(4)
frame
  label A cascade of a length and a time
  errors show
  code
      type Height = Quanta[Inches[1]] in (Seconds[1])

      val falstaff: Height = Quanta(5, 9)
##
```

[Mixed and non-decimal bases](../../soundness/modules/quantities.md#mixed-and-non-decimal-bases)
converts a cascade to an ordinary quantity and back.
