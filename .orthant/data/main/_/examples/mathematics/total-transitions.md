# A determinant only for a square matrix

Some operations of a matrix mean something only when it is square. They are defined for the
types whose rows and columns are the same number, and for no others, so there is no check of
shapes to fail at runtime.

```scala fuel
compile
frame
  label Two rows and three columns
  errors show
  code
      val matrix = Matrix[2, 3]((2, -3, 1), (2, 0, -1))
      val determinant = matrix.determinant
frame
  label Three rows and three columns
  highlight Matrix[3,..3]  square, so it has a determinant
  code
      val matrix = Matrix[3, 3]((2, -3, 1), (2, 0, -1), (1, 4, 5))
      val determinant = matrix.determinant
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(determinant.inspect)
```

[Matrices](../../soundness/modules/mathematics.md#matrices) lists the operations of a square
matrix, and [total transitions](../../philosophy/total-transitions.md) explains the approach.
