# A message which gains a field

The codec of a case class is derived from its fields, so the type is the schema. A field with
a default, or an optional one, is not required of the bytes, and a reader with the new type
still reads what a writer with the old type wrote.

```scala fuel
compile
frame
  label A reading, written by an old sensor
  code
      import strategies.throwUnsafely

      case class Reading(sensor: Text, value: Int)

      val bytes: Data = Reading(t"Dunsinane", 12).in[Data over Cbor]
frame
  label The same bytes, read by a newer type
  highlight unit..celsius"  takes its default
  highlight wind..Optional[Int]  reads as Unset
  code
      import strategies.throwUnsafely

      case class Reading(sensor: Text, value: Int)
      case class Report(sensor: Text, value: Int, unit: Text = t"celsius", wind: Optional[Int])

      val bytes: Data = Reading(t"Dunsinane", 12).in[Data over Cbor]
      val report: Report = bytes.read[Report in Cbor]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(report.inspect)
```

[Optional fields](../../soundness/modules/cbor.md#optional-fields) has the policies for a
field which is absent, null or wrong.
