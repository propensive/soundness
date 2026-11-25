# A layout made of smaller layouts

A field whose type has a layout of its own contributes its whole width, so a record is composed
from smaller records and is never flattened by hand.

```scala fuel
compile
frame
  label A record of two fields
  code
      import byteOrders.bigEndianByteOrder

      case class Point(x: S16, y: S16)

      object Point:
        given structural: (Point is Structural) = Structural.derived
frame
  label A record of two records
  highlight from:..Point)  four bytes each
  code
      import byteOrders.bigEndianByteOrder

      case class Point(x: S16, y: S16)

      object Point:
        given structural: (Point is Structural) = Structural.derived

      case class Line(color: B32, from: Point, to: Point)

      object Line:
        given structural: (Line is Structural) = Structural.derived
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(Structural.width[Point].show)
Out.println(Structural.width[Line].show)
```

[Describing a layout](../../soundness/modules/binary-data.md#describing-a-layout) has more on
nesting.
