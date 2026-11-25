# An unset field costs no bytes

A field of type `Optional` which is `Unset` is left out of the encoded map altogether, and a
map without the key reads back as `Unset`. A message pays for a field only when it carries it.

```scala fuel
compile
frame
  label A reading with its value
  code
      import strategies.throwUnsafely

      case class Reading(sensor: Text, value: Optional[Int])

      val full: Data = Reading(t"Dunsinane", 12).in[Data over Cbor]
frame
  label A reading without one
  highlight Unset  the key is not written
  code
      import strategies.throwUnsafely

      case class Reading(sensor: Text, value: Optional[Int])

      val full: Data = Reading(t"Dunsinane", 12).in[Data over Cbor]
      val bare: Data = Reading(t"Dunsinane", Unset).in[Data over Cbor]
      val read: Reading = bare.read[Reading in Cbor]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(full.length.show)
Out.println(bare.length.show)
Out.println(read.inspect)
```

[Optional fields](../../soundness/modules/cbor.md#optional-fields) has the policies for reading
one.
