# Where on the wire a message broke

A `Protobuf.Error` holds its reason as a value, and a reason about the bytes holds the offset
at which reading stopped. A handler takes the offset from a pattern, and a fault on the wire is
found without a hex dump.

```scala fuel
compile
frame
  label Any failure, handled alike
  code
      case class Ticket(@field(1) play: Text, @field(2) seat: Int)

      def seat(bytes: Data): Text =
        recover:
          case error: Protobuf.Error => t"unreadable"
        . protect(bytes.read[Ticket in Protobuf].seat.show)
frame
  label A message cut short, and where
  highlight Truncated(offset)  the offset, as an Int
  code
      import Protobuf.Error.Reason.Truncated

      case class Ticket(@field(1) play: Text, @field(2) seat: Int)

      def seat(bytes: Data): Text =
        recover:
          case Protobuf.Error(Truncated(offset)) => t"cut short at byte $offset"
          case error: Protobuf.Error             => t"unreadable"
        . protect(bytes.read[Ticket in Protobuf].seat.show)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(seat(hex"0a0850657269636c6573100c"))
Out.println(seat(hex"0a085065726963"))
```

[Errors](../../soundness/modules/protobuf.md#errors) lists the reasons, and
[expressive errors](../../philosophy/expressive-errors.md) says why they are values.
