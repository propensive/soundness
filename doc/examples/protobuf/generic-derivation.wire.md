# The type of a field chooses its encoding

Proto3 has several ways to write an integer, and a schema names one for each field. Here the
Scala type of the field names it. An `Int` is an `int32`, which spends ten bytes on a negative
number, and an `S32` is a `sint32`, which spends one.

```scala fuel
compile
frame
  label A change, as an int32
  code
      case class Change(@field(1) amount: Int)

      val plain = Change(-1).in[Protobuf].encode.length
frame
  label The same change, as a sint32
  highlight S32  zig-zag encoded
  code
      case class Change(@field(1) amount: Int)
      case class Shift(@field(1) amount: S32)

      val plain = Change(-1).in[Protobuf].encode.length
      val zigzag = Shift(-1).in[Protobuf].encode.length
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(plain.show)
Out.println(zigzag.show)
```

Each length includes one byte for the field's number.
[Field types](../../soundness/modules/protobuf.md#field-types) has the whole table.
