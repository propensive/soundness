# An enumeration is a oneof

Proto3 writes a choice between messages as a `oneof`, in which the number of the field says
which was chosen. An enumeration needs nothing more than its cases: the codec derived for it
writes the case as the field of its number, and reads it back by that number.

```scala fuel
compile
frame
  label Two kinds of cue
  code
      import strategies.throwUnsafely

      enum Cue:
        case Enter(player: Text)
        case Exit(player: Text, pursued: Boolean)
frame
  label One of them, across the wire
  highlight read[Cue..Protobuf]  the field's number chooses the case
  code
      import strategies.throwUnsafely

      enum Cue:
        case Enter(player: Text)
        case Exit(player: Text, pursued: Boolean)

      val bytes: Data = (Cue.Exit(t"Antigonus", true): Cue).in[Protobuf].encode
      val cue: Cue = bytes.read[Cue in Protobuf]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(cue.inspect)
```

[Field types](../../soundness/modules/protobuf.md#field-types) has the encoding of every type.
