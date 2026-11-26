# An enumeration, with its case named in the map

An enumeration encodes as a CBOR map with one more entry, which names the case. One given
says which key holds the name, and the codec derived for the enumeration writes it and reads
it.

```scala fuel
compile
frame
  label The key which names the case
  highlight Cbor.discriminatedUnion(t"kind")  declared once
  code
      import strategies.throwUnsafely

      enum Status:
        case Active(since: Int)
        case Retired

      given statusUnion: (Status is Discriminable in Cbor) = Cbor.discriminatedUnion(t"kind")
frame
  label A case, written and read back
  highlight as[Status]  the key chooses the case
  code
      import strategies.throwUnsafely

      enum Status:
        case Active(since: Int)
        case Retired

      given statusUnion: (Status is Discriminable in Cbor) = Cbor.discriminatedUnion(t"kind")

      val sent: Cbor = (Status.Active(1599): Status).in[Cbor]
      val received: Status = sent.as[Status]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(received.inspect)
```

[Encoding and decoding](../../soundness/modules/cbor.md#encoding-and-decoding) has the rest.
