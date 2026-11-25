# A transpose which swaps its type

The transpose of a matrix of two rows and three columns has three rows and two columns. The
compiler computes that from the type of the original, and a transpose assigned to the wrong
shape does not compile.

```scala fuel
compile
frame
  label A transpose, still claimed to be two by three
  errors show
  code
      val wide = Matrix[2, 3]((1, 2, 3), (4, 5, 6))
      val turned: Matrix[Int, 2, 3] = wide.transpose
frame
  label Three by two
  highlight Matrix[Int,..2]  rows and columns exchanged
  code
      val wide = Matrix[2, 3]((1, 2, 3), (4, 5, 6))
      val turned: Matrix[Int, 3, 2] = wide.transpose
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(turned.inspect)
```

[Matrices](../../soundness/modules/mathematics.md#matrices) lists the operations which are
typed this way.
