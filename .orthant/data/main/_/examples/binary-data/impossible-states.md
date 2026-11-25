# Only a field of fixed width has a place in a layout

Every field of a binary record must occupy a known number of bytes. A field of text has no
fixed width, so a record which contains one has no layout, and the compiler says so.

```scala fuel
compile
frame
  label A name of any length
  errors show
  code
      import byteOrders.bigEndianByteOrder

      case class Entry(name: Text, size: U32)

      object Entry:
        given structural: (Entry is Structural) = Structural.derived
frame
  label A name of exactly eight bytes
  highlight B64  eight raw bytes
  code
      import byteOrders.bigEndianByteOrder

      case class Entry(name: B64, size: U32)

      object Entry:
        given structural: (Entry is Structural) = Structural.derived
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(Structural.width[Entry].show)
```

[The typeclass](../../soundness/modules/binary-data.md#the-typeclass) lists the types whose
widths are known.
