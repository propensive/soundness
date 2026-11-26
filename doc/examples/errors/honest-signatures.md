# Every way to fail, and no more

A method which can fail in two ways names both, each with its own `raises`. When one is
handled it leaves the signature, and what remains is exactly what a caller still has to
answer.

```scala fuel
compile
frame
  label Two ways to fail
  highlight raises..CastError  both are part of the type
    line 7
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics) extends Error(m"port $port is taken")
      case class CastError(part: Text)(using Diagnostics) extends Error(m"nobody plays $part")

      def stage(port: Int, part: Text): Text raises PortError raises CastError =
        if port < 1024 then abort(PortError(port))
        if part == t"Yorick" then abort(CastError(part)) else part
frame
  label One of them answered
  focus rehearse..part))
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics) extends Error(m"port $port is taken")
      case class CastError(part: Text)(using Diagnostics) extends Error(m"nobody plays $part")

      def stage(port: Int, part: Text): Text raises PortError raises CastError =
        if port < 1024 then abort(PortError(port))
        if part == t"Yorick" then abort(CastError(part)) else part

      def rehearse(part: Text): Text raises CastError =
        recover:
          case PortError(port) => t"understudy"
        . protect(stage(80, part))
##
```

[Declaring that code can fail](../../soundness/modules/errors.md#declaring-that-code-can-fail)
describes `raises`, and [honest signatures](../../philosophy/honest-signatures.md) the
principle.
