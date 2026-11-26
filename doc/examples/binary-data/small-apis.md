# One method reads any record

There is no `readHeader` and no `readPairs`. `unpack` takes the type which is wanted and the
offset it is at, and `pack` writes any value whose type has a layout.

```scala fuel
compile
frame
  label One record
  highlight unpack[Pair](0)  one record, at an offset
  code
      import byteOrders.bigEndianByteOrder, strategies.throwUnsafely

      case class Pair(left: U16, right: U16)

      object Pair:
        given structural: (Pair is Structural) = Structural.derived

      val data = hex"0001000200030004"
      val first = data.unpack[Pair](0)
frame
  label Two records, from an offset and a count
  highlight unpack[Pair](0,..2)  an offset and a count
  code
      import byteOrders.bigEndianByteOrder, strategies.throwUnsafely

      case class Pair(left: U16, right: U16)

      object Pair:
        given structural: (Pair is Structural) = Structural.derived

      val data = hex"0001000200030004"
      val first = data.unpack[Pair](0)
      val pairs = data.unpack[Pair](0, 2)
frame
  label The bytes again
  highlight pack  the same layout, written
  code
      import byteOrders.bigEndianByteOrder, strategies.throwUnsafely

      case class Pair(left: U16, right: U16)

      object Pair:
        given structural: (Pair is Structural) = Structural.derived

      val data = hex"0001000200030004"
      val first = data.unpack[Pair](0)
      val pairs = data.unpack[Pair](0, 2)
      val bytes = first.pack
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(first.right.inspect)
Out.println(pairs.length.show)
Out.println(bytes.length.show)
```

[Reading](../../soundness/modules/binary-data.md#reading) and
[writing](../../soundness/modules/binary-data.md#writing) use the one definition in both
directions.
