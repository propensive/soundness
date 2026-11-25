# A geo URI, read and written

A `geo:` URI is read with `as`, the method which reads a number or a date, and written back
with `encode`. No method is named for the format.

```scala fuel
compile
frame
  label Text, as a position
  highlight as[Geolocation]  the type which is wanted
  code
      import strategies.throwUnsafely

      val castle = t"geo:56.04,12.62,10".as[Geolocation]
frame
  label Its parts
  highlight altitude  optional in the format, and in the type
  code
      import strategies.throwUnsafely

      val castle = t"geo:56.04,12.62,10".as[Geolocation]
      val height: Optional[Double] = castle.altitude
frame
  label And text again
  highlight encode  the way back
  code
      import strategies.throwUnsafely

      val castle = t"geo:56.04,12.62,10".as[Geolocation]
      val height: Optional[Double] = castle.altitude
      val uri: Text = castle.encode
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(height.inspect)
Out.println(uri)
```

[Geo URIs](../../soundness/modules/geography.md#geo-uris) describes the format, and
[small APIs](../../philosophy/small-apis.md) the reasons for one name.
