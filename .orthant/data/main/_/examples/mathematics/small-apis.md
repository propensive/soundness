# One operator for every product

A matrix multiplies a matrix, a vector or a scalar, and each product is written with `*`.
The typeclass instance which is found decides what is computed, and the type of the result.

```scala fuel
compile
alternative By a matrix
  highlight Matrix[Int,..2]  the inner dimensions cancel
  code
      val wide = Matrix[2, 3]((1, 2, 3), (4, 5, 6))
      val tall = Matrix[3, 2]((7, 8), (9, 10), (11, 12))
      val product: Matrix[Int, 2, 2] = wide*tall
alternative By a vector
  highlight Vector[Int,..2]  one component for each row
  code
      val wide = Matrix[2, 3]((1, 2, 3), (4, 5, 6))
      val product: Vector[Int, 2] = wide*Vector(7, 8, 9)
alternative By a number
  highlight Matrix[Int,..3]  the shape is unchanged
  code
      val wide = Matrix[2, 3]((1, 2, 3), (4, 5, 6))
      val product: Matrix[Int, 2, 3] = wide*10
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(product.inspect)
```

[Matrices](../../soundness/modules/mathematics.md#matrices) has the other operations, and
[small APIs](../../philosophy/small-apis.md) the case for one name.
