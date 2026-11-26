# Vectors which know their length

The number of components of a `Vector` is part of its type, so the compiler checks the shapes
in a calculation before it runs.

```scala fuel
compile
frame
  label Two components added to three
  errors show
  code
      val position = Vector(1, 2, 3)
      val moved = position + Vector(4, 5)
frame
  label Three added to three
  highlight Vector[Int,..3]  three components, in the type
  code
      val position = Vector(1, 2, 3)
      val moved: Vector[Int, 3] = position + Vector(4, 5, 6)
##
```

[Mathematics](../../soundness/modules/mathematics.md) does the same for the rows and columns of
a matrix.
