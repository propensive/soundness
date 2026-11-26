# A cross product needs three dimensions

The scalar product of two vectors exists in any dimension, and the vector product does not.
`dot` is defined for every pair of vectors of one length, and `cross` only for the lengths
which have a vector product.

```scala fuel
compile
frame
  label The scalar product, in two dimensions
  highlight dot  defined in every dimension
  code
      val east = Vector(1, 0)
      val north = Vector(0, 1)
      val product = east.dot(north)
frame
  label The vector product, in two dimensions
  errors show
  code
      val east = Vector(1, 0)
      val north = Vector(0, 1)
      val product = east.cross(north)
frame
  label The vector product, in three
  highlight cross  defined in three
  code
      val east = Vector(1, 0, 0)
      val north = Vector(0, 1, 0)
      val product = east.cross(north)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(product.inspect)
```

[Vectors](../../soundness/modules/mathematics.md#vectors) has the arithmetic which every
dimension shares.
