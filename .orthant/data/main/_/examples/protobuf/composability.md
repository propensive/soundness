# One field of a message, replaced in passing

A message is immutable, and its fields are known by number. A lens from the optics library
selects a field by its position, `Prim` for field 1, and gives a new message with that field
replaced and every other byte as it was.

```scala fuel
compile
frame
  label A message, as it arrived
  code
      import strategies.throwUnsafely

      case class Mark(@field(1) row: Int, @field(2) seat: Int)
      case class Ticket(@field(1) mark: Mark, @field(2) play: Text)

      val ticket: Protobuf = Ticket(Mark(3, 4), t"Pericles").in[Protobuf]
frame
  label Its first field, replaced
  highlight _(Prim)..)  field 1, replaced
  code
      import strategies.throwUnsafely
      import conversions.encodableToProtobuf

      case class Mark(@field(1) row: Int, @field(2) seat: Int)
      case class Ticket(@field(1) mark: Mark, @field(2) play: Text)

      val ticket: Protobuf = Ticket(Mark(3, 4), t"Pericles").in[Protobuf]
      val moved: Protobuf = ticket.lens(_(Prim) = Mark(7, 8))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(moved.as[Ticket].inspect)
```

[Navigating a message](../../soundness/modules/protobuf.md#navigating-a-message) says when this
is the right tool, and [optics](../../soundness/modules/optics.md) describes lenses.
