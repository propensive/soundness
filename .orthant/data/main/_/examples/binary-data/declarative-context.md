# The byte order is chosen where the layout is declared

The order of the bytes within a field belongs to the format and not to each read, so it is an
import beside the layout. Without one, the layout cannot be derived.

```scala fuel
compile
frame
  label No byte order
  errors show
  code
      case class Version(major: U16, minor: U16)

      object Version:
        given structural: (Version is Structural) = Structural.derived
frame
  label Big-endian
  highlight bigEndianByteOrder  most significant byte first
  code
      import byteOrders.bigEndianByteOrder, strategies.throwUnsafely

      case class Version(major: U16, minor: U16)

      object Version:
        given structural: (Version is Structural) = Structural.derived

      val version = hex"00010200".unpack[Version](0)
frame
  label Little-endian
  highlight littleEndianByteOrder  least significant byte first
  code
      import byteOrders.littleEndianByteOrder, strategies.throwUnsafely

      case class Version(major: U16, minor: U16)

      object Version:
        given structural: (Version is Structural) = Structural.derived

      val version = hex"00010200".unpack[Version](0)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(version.major.inspect)
Out.println(version.minor.inspect)
```

The same four bytes are a different version under each import.
[Describing a layout](../../soundness/modules/binary-data.md#describing-a-layout) says how a
format which mixes the two is declared.
