# A message written straight into its bytes

Encoding ordinarily builds a `Protobuf` value for each field and each nested message, and then
writes it. A message which `derives Protobuf.Direct` has a printer and a parser composed for
its own shape as it compiles, which measure and write the fields with nothing between, and one
import makes every such message take that route.

```scala fuel
compile
frame
  label Through a Protobuf value
  code
      import strategies.throwUnsafely

      case class Ticket(play: Text, seat: Int)

      val bytes: Data = Ticket(t"Pericles", 12).in[Data over Protobuf]
frame
  label Directly, by a generated codec
  highlight derives..Direct  a parser and a printer for this shape
  highlight codecs.direct  used wherever one exists
  code
      import strategies.throwUnsafely
      import codecs.direct

      case class Ticket(play: Text, seat: Int) derives Protobuf.Direct

      val bytes: Data = Ticket(t"Pericles", 12).in[Data over Protobuf]
      val ticket: Ticket = bytes.read[Ticket in Protobuf]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(ticket.inspect)
```

The bytes are the same by either route.
[Choosing the route for every type](../../soundness/modules/protobuf.md#choosing-the-route-for-every-type)
compares the three modes.
