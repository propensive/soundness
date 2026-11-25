# Two fallible steps, one after the other

A call which can fail gives its value, not a wrapper around it. The result of one fallible
call is passed to the next as a plain number, with no `Either`, no `flatMap` and no `for`
comprehension, and the possibility of failure is in the signature alone.

```scala fuel
compile
frame
  label One step which can fail
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))
frame
  label Two, in sequence
  focus pair..1))
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      def pair(port: Int): (Int, Int) raises PortError =
        val first = connect(port)
        (first, connect(first + 1))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(safely(pair(8080)).inspect)
Out.println(safely(pair(80)).inspect)
```

[Errors](../../soundness/modules/errors.md#on-errors) sets this against exceptions, and
[direct style](../../philosophy/direct-style.md) says where the effect went.
