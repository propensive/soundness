# One fallible method, four responses

`connect` raises a `PortError`, and does not say what happens next. Each caller decides, in
the block where the call is made, and the type of the result follows from the decision.

```scala fuel
compile
alternative Absent
  highlight safely  a failure is Unset
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      val port: Optional[Int] = safely(connect(80))
alternative Replaced
  highlight recover  a failure is answered with a value
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      val port: Int =
        recover:
          case PortError(port) => 8080
        . protect(connect(80))
alternative Inspected
  highlight attempt[PortError]  success or failure, as a value
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      val port: Text = attempt[PortError](connect(80)) match
        case Attempt.Success(port)  => t"listening on $port"
        case Attempt.Failure(error) => t"failed: ${error.message}"
alternative Thrown
  highlight strategies.throwUnsafely  a failure is thrown
  code
      import errorDiagnostics.stackTracesDiagnostics
      import strategies.throwUnsafely

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      def port: Int = connect(80)
##
```

[Choosing a strategy](../../soundness/modules/errors.md#choosing-a-strategy) compares all the
ways to run fallible code.
