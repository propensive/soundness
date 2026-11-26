# A matrix product whose shapes agree

The rows and columns of a `Matrix` are in its type. A product is defined only where the
columns of the left matrix equal the rows of the right, so a product of the wrong shapes is
not a value which can be written.

```scala fuel
compile
frame
  label Two rows by three, times two rows by three
  errors show
  code
      val wide = Matrix[2, 3]((1, 2, 3), (4, 5, 6))
      val product = wide*wide
frame
  label Two rows by three, times three rows by two
  highlight Matrix[Int,..2]  the outer dimensions remain
  code
      val wide = Matrix[2, 3]((1, 2, 3), (4, 5, 6))
      val tall = Matrix[3, 2]((7, 8), (9, 10), (11, 12))
      val product: Matrix[Int, 2, 2] = wide*tall
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(product.inspect)
```

[Matrices](../../soundness/modules/mathematics.md#matrices) has the rest of the arithmetic, and
[impossible states](../../philosophy/impossible-states.md) the principle.
