# A layout derived from a case class

A binary record is a case class whose fields have fixed widths. Derivation computes every
offset from the order of the fields, so the reader is never written, and no index appears.

```scala fuel
compile
frame
  label The fields, each with its width
  code
      import byteOrders.bigEndianByteOrder, strategies.throwUnsafely

      case class Header(magic: B32, version: U16, flags: B16, length: U32)
frame
  label The layout
  highlight Structural.derived  every offset, computed from the fields
  code
      import byteOrders.bigEndianByteOrder, strategies.throwUnsafely

      case class Header(magic: B32, version: U16, flags: B16, length: U32)

      object Header:
        given structural: (Header is Structural) = Structural.derived
frame
  label A record, read from twelve bytes
  highlight unpack[Header](0)  the type wanted, and its offset
  code
      import byteOrders.bigEndianByteOrder, strategies.throwUnsafely

      case class Header(magic: B32, version: U16, flags: B16, length: U32)

      object Header:
        given structural: (Header is Structural) = Structural.derived

      val header = hex"cafebabe0001000000000010".unpack[Header](0)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(Structural.width[Header].show)
Out.println(header.magic.hex)
Out.println(header.length.inspect)
```

[Describing a layout](../../soundness/modules/binary-data.md#describing-a-layout) explains how
the widths add up.
