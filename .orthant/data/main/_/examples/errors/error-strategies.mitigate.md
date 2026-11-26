# An error translated at the boundary

A caller of a service should not need to know that it failed for want of a port. `mitigate`
exchanges one error for another, so the low-level failure reaches the caller in the
vocabulary of the API it called.

```scala fuel
compile
frame
  label The low-level error escapes
  highlight raises..PortError  an implementation detail, in the signature
    line 9
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics) extends Error(m"port $port is taken")
      case class StageError()(using Diagnostics) extends Error(m"the stage is not ready")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      def open(port: Int): Int raises PortError = connect(port)
frame
  label Exchanged for the error of this API
  focus open..connect(port))
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics) extends Error(m"port $port is taken")
      case class StageError()(using Diagnostics) extends Error(m"the stage is not ready")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      def open(port: Int): Int raises StageError =
        mitigate:
          case PortError(port) => StageError()
        . protect(connect(port))
##
```

[Recovering](../../soundness/modules/errors.md#recovering) describes `mitigate`, and
[Translating and escalating](../../soundness/modules/errors.md#translating-and-escalating) a
way to apply a translation everywhere.
