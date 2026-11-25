# A handled error leaves the type

`recover` supplies a value for each error it names. Once every error of a block has an answer
the block cannot fail, and the method around it no longer declares anything.

```scala fuel
compile
frame
  label A method which can fail
  highlight raises..PortError  passed on to the caller
    line 9
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      def listen(port: Int): Int raises PortError = connect(port)
frame
  label A method which cannot
  focus listen..connect(port))
  code
      import errorDiagnostics.stackTracesDiagnostics

      case class PortError(port: Int)(using Diagnostics)
      extends Error(m"the port $port is not available")

      def connect(port: Int): Int raises PortError =
        if port > 1024 then port else abort(PortError(port))

      def listen(port: Int): Int =
        recover:
          case PortError(port) => 8080
        . protect(connect(port))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(listen(80).show)
```

[Recovering](../../soundness/modules/errors.md#recovering) has `mitigate`, which exchanges one
error for another.
