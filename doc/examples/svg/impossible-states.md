# Two positions cannot be added

A `Point` is a position in a drawing and a `Delta` is a displacement, and they are different
types. A point moved by a delta is a point; the sum of two points means nothing, and is not
an expression.

```scala fuel
compile
frame
  label A position added to a position
  errors show
  code
      val origin = Point(10, 20)
      val corner = origin + Point(30, 40)
frame
  label A position moved by a displacement
  highlight Delta  a displacement, which a point may be moved by
  code
      val origin = Point(10, 20)
      val corner = origin + Delta(30, 40)
frame
  label The displacement between two positions
  highlight -  a point less a point is a delta
  code
      val origin = Point(10, 20)
      val corner = origin + Delta(30, 40)
      val diagonal: Delta = corner - origin
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(corner.inspect)
Out.println(diagonal.inspect)
```

[Points and deltas](../../soundness/modules/svg.md#points-and-deltas) has the whole table of
the arithmetic, and [impossible states](../../philosophy/impossible-states.md) the reason for
two types.
