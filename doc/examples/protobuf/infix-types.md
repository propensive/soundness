# From a value to bytes, and back

`in` and `read` take a type which says what is wanted and in what form. `Data over Protobuf`
is bytes which carry a message, and `Ticket in Protobuf` is a ticket held in one, so the wire
is crossed by one expression in each direction.

```scala fuel
compile
frame
  label A message, then its bytes
  code
      import strategies.throwUnsafely

      case class Ticket(@field(1) play: Text, @field(2) seat: Int)

      val message: Protobuf = Ticket(t"Pericles", 12).in[Protobuf]
      val bytes: Data = message.encode
frame
  label Bytes in one step
  highlight in[Data..Protobuf]  encoded and written
  code
      import strategies.throwUnsafely

      case class Ticket(@field(1) play: Text, @field(2) seat: Int)

      val bytes: Data = Ticket(t"Pericles", 12).in[Data over Protobuf]
frame
  label And a value again
  highlight read[Ticket..Protobuf]  parsed and decoded
  code
      import strategies.throwUnsafely

      case class Ticket(@field(1) play: Text, @field(2) seat: Int)

      val bytes: Data = Ticket(t"Pericles", 12).in[Data over Protobuf]
      val ticket: Ticket = bytes.read[Ticket in Protobuf]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import alphabets.hexLowerCase

Out.println(bytes.serialize[Hex])
Out.println(ticket.inspect)
```

[Printing directly](../../soundness/modules/protobuf.md#printing-directly) says which route the
bytes take, and [infix types](../../philosophy/infix-types.md) describes the vocabulary.
