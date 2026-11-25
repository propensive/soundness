# An angle is not a number

An `Angle` is made through its unit, so a bare number never stands for one. Whether `90.0` was
meant as degrees or as radians is a question which must be answered where it is written.

```scala fuel
compile
frame
  label A number, in no unit
  errors show
  code
      val heading: Angle = 90.0
frame
  label Ninety degrees
  highlight .deg  the unit, where the number is written
  code
      val heading: Angle = 90.0.deg
frame
  label The same angle, in radians
  highlight .rad  the other unit
  code
      val heading: Angle = 1.5708.rad
##
```

[Angles](../../soundness/modules/geography.md#angles) reads an angle back in either unit, and
[impossible states](../../philosophy/impossible-states.md) explains why the number is kept
out.
