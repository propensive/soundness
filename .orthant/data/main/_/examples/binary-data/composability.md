# A record, packed and written in hexadecimal

`pack` gives the bytes of a record, and bytes are what the base encodings write. A value goes
from a case class to the hexadecimal of its wire format in one expression, and comes back in
another.

```scala fuel
compile
frame
  label A record
  code
      import byteOrders.bigEndianByteOrder, alphabets.hexLowerCase
      import strategies.throwUnsafely

      case class Size(width: Short, height: Short)

      object Size:
        given structural: (Size is Structural) = Structural.derived

      val stage = Size(1920, 1080)
frame
  label Its bytes, in hexadecimal
  highlight pack.serialize[Hex]  two libraries, one expression
  code
      import byteOrders.bigEndianByteOrder, alphabets.hexLowerCase
      import strategies.throwUnsafely

      case class Size(width: Short, height: Short)

      object Size:
        given structural: (Size is Structural) = Structural.derived

      val stage = Size(1920, 1080)
      val wire: Text = stage.pack.serialize[Hex]
frame
  label And the record again, from a hexadecimal literal
  highlight hex"07800438"  checked as the code compiles
  code
      import byteOrders.bigEndianByteOrder, alphabets.hexLowerCase
      import strategies.throwUnsafely

      case class Size(width: Short, height: Short)

      object Size:
        given structural: (Size is Structural) = Structural.derived

      val stage = Size(1920, 1080)
      val wire: Text = stage.pack.serialize[Hex]
      val again = hex"07800438".unpack[Size](0)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(wire)
Out.println((again == stage).inspect)
```

[Writing](../../soundness/modules/binary-data.md#writing) describes `pack`, and
[base encoding](../../soundness/modules/base-encoding.md) the alphabets.
