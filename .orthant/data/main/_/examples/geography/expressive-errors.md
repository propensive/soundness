# What is wrong with a geo URI

The error for a malformed `geo:` URI holds the reason as a value of an enumeration, which a
program can match on, and its message puts the same reason into words.

```scala fuel
compile
frame
  label The error, captured as a value
  highlight capture[Geolocation.Error]  the error which the decoding raises
  code
      import strategies.throwUnsafely, errorDiagnostics.emptyDiagnostics

      val error: Geolocation.Error =
        capture[Geolocation.Error](t"geo:56.04,12.62,10?".as[Geolocation])
frame
  label Its reason
  highlight error.reason  a case of an enumeration
  code
      import strategies.throwUnsafely, errorDiagnostics.emptyDiagnostics

      val error: Geolocation.Error =
        capture[Geolocation.Error](t"geo:56.04,12.62,10?".as[Geolocation])

      val reason: Geolocation.Error.Reason = error.reason
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(reason.inspect)
Out.println(error.message.show)
```

[Geo URIs](../../soundness/modules/geography.md#geo-uris) has the format, and
[expressive errors](../../philosophy/expressive-errors.md) the case for a reason which is
both a value and a sentence.
