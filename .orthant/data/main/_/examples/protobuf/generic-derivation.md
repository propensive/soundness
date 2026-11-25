# A case class is the definition of a message

There is no schema file and no generated code. The fields of a case class, each with its
number, are the message, and the compiler derives the wire format from them. The bytes are
those which `protoc` writes for the same message.

```scala fuel
compile
frame
  label The example message of the proto3 documentation
  highlight @field(1)  the field's number on the wire
  code
      import alphabets.hexLowerCase

      case class Sample(@field(1) value: Int)

      val bytes: Text = Sample(150).in[Protobuf].encode.serialize[Hex]
frame
  label A second field, numbered sparsely
  highlight @field(7)  numbers need not be consecutive
  code
      import alphabets.hexLowerCase

      case class Sample(@field(1) value: Int, @field(7) name: Text)

      val bytes: Text = Sample(150, t"Puck").in[Protobuf].encode.serialize[Hex]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(bytes)
```

The first three bytes are the documented `08 96 01`.
[Messages](../../soundness/modules/protobuf.md#messages) has the rest, and
[field types](../../soundness/modules/protobuf.md#field-types) the encoding of each type.
