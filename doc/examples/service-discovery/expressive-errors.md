# A refusal which names the fault

A `Discovery.Error` holds its reason as a value: which of the things that can go wrong did,
and the name it went wrong for. Its message is a sentence made from that reason.

```scala fuel
compile
frame
  label A name over fifteen characters
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      val error: Discovery.Error =
        capture[Discovery.Error](Discovery.Service(t"a-midsummer-nights-dream", Tcp))
frame
  label The reason, taken apart
  highlight InvalidService(name)  the fault, and the name at fault
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics

      val error: Discovery.Error =
        capture[Discovery.Error](Discovery.Service(t"a-midsummer-nights-dream", Tcp))

      val length: Int = error.reason match
        case Discovery.Error.Reason.InvalidService(name) => name.length
        case _                                           => 0
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(error.message.show)
Out.println(length.show)
```

[Errors](../../soundness/modules/service-discovery.md#errors) lists the reasons, and
[expressive errors](../../philosophy/expressive-errors.md) says why they are values.
