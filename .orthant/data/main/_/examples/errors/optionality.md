# A failure, as an absence

Where it does not matter why something failed, the failure is an absent value. `safely` runs
fallible code and gives an `Optional`, and from there the ordinary operations on optional
values say what happens next.

```scala fuel
compile
frame
  label A port, or nothing
  highlight safely  any failure becomes Unset
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics) extends Error(m"port $port is taken")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      val port: Optional[Int] = safely(connect(80))
frame
  label A port, or the usual one
  highlight or(8080)  the fallback
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics) extends Error(m"port $port is taken")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      val port: Int = safely(connect(80)).or(8080)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(port.show)
```

[Choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) sets `safely`
beside the other blocks, and [Optional values](../../soundness/modules/optional-values.md)
has the operations.
