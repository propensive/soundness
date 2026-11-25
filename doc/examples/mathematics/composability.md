# A vector of lengths

A vector is generic in its elements, and finds their arithmetic through the same typeclasses
as a quantity does. Neither library mentions the other, and yet the scalar product of two
vectors of lengths is an area.

```scala fuel
compile
frame
  label Two vectors of numbers
  code
      val stage = Vector(5, 2, 1)
      val trap = Vector(2, 3, 6)
      val product = stage.dot(trap)
frame
  label Two vectors of lengths
  highlight Inch  a length, from the library of quantities
    line 1
    occurrence 1
  code
      val stage = Vector(5*Inch, 2*Inch, Inch)
      val trap = Vector(2*Inch, 3*Inch, 6*Inch)
      val product = stage.dot(trap)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(3)

Out.println(product.show)
```

The result is in square inches.
[Elements that are not numbers](../../soundness/modules/mathematics.md#elements-that-are-not-numbers)
says what else may fill a vector, and [Quantities](../../soundness/modules/quantities.md)
describes the units.
