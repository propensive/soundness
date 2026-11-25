# Whether an error records a stack trace

Constructing an error needs a `Diagnostics` in scope, which decides whether a stack trace is
recorded. The choice is an import, made once for all the errors a scope constructs, and none
is assumed.

```scala fuel
compile
frame
  label No choice has been made
  errors show
  code
      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      val error = PortError(80)
frame
  label A trace, for debugging
  highlight errorDiagnostics.stackTracesDiagnostics  every error records where it was made
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      val error = PortError(80)
frame
  label No trace, where errors are expected
  highlight errorDiagnostics.emptyDiagnostics  an error is only its fields
  code
      import errorDiagnostics.emptyDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      val error = PortError(80)
##
```

[Diagnostics](../../soundness/modules/errors.md#diagnostics) explains why an error without a
trace costs no more than any other value.
