# An error which keeps what it knows

An error is a case class, and its message is written with `m"…"`. The values which explain
the failure stay as typed fields, so a handler reads the port as a number and has no message
to parse.

```scala fuel
compile
frame
  label An error with a field and a message
  highlight m"the..available"  a message with a typed substitution
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")
frame
  label Raised, and captured
  highlight capture[PortError]  gives the error itself
  code
      import errorDiagnostics.stackTracesDiagnostics
      import strategies.throwUnsafely

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      val error: PortError = capture[PortError](connect(80))
      val retry: Int = error.port + 8000
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(error.message.show)
Out.println(retry.show)
```

[Defining an error](../../soundness/modules/errors.md#defining-an-error) describes messages,
and [expressive errors](../../philosophy/expressive-errors.md) what they are for.
