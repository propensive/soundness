# A length is not an angle

The units of CSS are quantities, each with its dimension in its type. Two lengths add, in
whichever units they were written, and a length and an angle do not.

```scala fuel
compile
frame
  label Two lengths
  highlight +  both are lengths
  code
      val margin = 2.0*Rem
      val gutter = margin + 2.0*Rem
frame
  label A length and an angle
  errors show
  code
      val margin = 2.0*Rem
      val gutter = margin + 90.0*Deg
##
```

[Units](../../soundness/modules/css.md#units) lists the units, which are the
[quantities](../../soundness/modules/quantities.md) used everywhere else.
