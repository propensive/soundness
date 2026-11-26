# Three spellings of one type

`raises` is an alias, and hides nothing. A method which declares `raises PortError` takes the
way its error is handled as a contextual parameter, and each of these signatures is the same
method.

```scala fuel
compile
alternative A contextual parameter
  highlight (using..Tactic[PortError])  how the error is handled, as an argument
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int)(using Tactic[PortError]): Int =
        if port > 1024 then port else abort(PortError(port))
alternative A context function
  highlight Tactic[PortError]..Int  the same parameter, in the result type
    line 6
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int): Tactic[PortError] ?=> Int =
        if port > 1024 then port else abort(PortError(port))
alternative A phrase
  highlight raises..PortError  an alias of the context function
    line 6
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))
##
```

[Declaring that code can fail](../../soundness/modules/errors.md#declaring-that-code-can-fail)
describes the mechanism, and [infix types](../../philosophy/infix-types.md) the other words
which read this way.
