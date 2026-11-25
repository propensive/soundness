# A record which runs past the end of its bytes

`unpack` checks the whole record against the length of the data before it reads a field. That
the data may be too short is part of its type, so a caller must say what happens then.

```scala fuel
compile
frame
  label The failure is not accounted for
  errors show
  code
      import byteOrders.bigEndianByteOrder

      case class Pair(left: U16, right: U16)

      object Pair:
        given structural: (Pair is Structural) = Structural.derived

      def last(data: Data): Pair = data.unpack[Pair](data.length - 4)
frame
  label It is declared
  highlight raises..Structural.Error  the caller decides
  code
      import byteOrders.bigEndianByteOrder

      case class Pair(left: U16, right: U16)

      object Pair:
        given structural: (Pair is Structural) = Structural.derived

      def last(data: Data): Pair raises Structural.Error =
        data.unpack[Pair](data.length - 4)
frame
  label Or it becomes an absent value
  highlight safely  `Unset` if the data is too short
  code
      import byteOrders.bigEndianByteOrder

      case class Pair(left: U16, right: U16)

      object Pair:
        given structural: (Pair is Structural) = Structural.derived

      def last(data: Data): Optional[Pair] =
        safely(data.unpack[Pair](data.length - 4))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(last(hex"0001000200030004").inspect)
Out.println(last(hex"0001").inspect)
```

[Reading](../../soundness/modules/binary-data.md#reading) describes what the error holds.
