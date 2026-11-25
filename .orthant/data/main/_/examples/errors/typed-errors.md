# A failure which a caller cannot overlook

A method which can fail names the error in its return type, with `raises`. Code which calls it
must handle the error or declare it in turn, and code which does neither does not compile.

```scala fuel
compile
frame
  label A call which ignores the error
  errors show
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      def start(): Int = connect(80)
frame
  label The error, declared in turn
  highlight raises..PortError  now the caller of start must answer
    line 9
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      def start(): Int raises PortError = connect(80)
##
```

[Declaring that code can fail](../../soundness/modules/errors.md#declaring-that-code-can-fail)
explains what `raises` stands for, and
[error handling](../../philosophy/error-handling.md) the design.
