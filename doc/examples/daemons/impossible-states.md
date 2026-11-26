# A bus which carries one type of message

The invocations of a daemon talk to each other over a bus, and the type of its messages is the
type argument of `cli`. Every invocation is compiled against that type, so a message which the
others would not understand cannot be sent.

```scala fuel
compile
frame
  label A message of the wrong type
  errors show
  code
      import backstops.stackTraceBackstop, executives.completionsExecutive
      import interpreters.posixInterpreter, threads.virtualThreads

      enum Notice:
        case CurtainUp, CurtainDown

      def stage(): Unit = cli[Notice]:
        execute:
          resident.broadcast(t"curtain up")
          Exit.Ok
frame
  label A notice
  highlight Notice.CurtainUp  the type every invocation agreed on
  code
      import backstops.stackTraceBackstop, executives.completionsExecutive
      import interpreters.posixInterpreter, threads.virtualThreads

      enum Notice:
        case CurtainUp, CurtainDown

      def stage(): Unit = cli[Notice]:
        execute:
          resident.broadcast(Notice.CurtainUp)
          Exit.Ok
##
```

The example is compiled, and is not run, since it defines a whole program.
[The message bus](../../soundness/modules/daemons.md#the-message-bus) shows the other side,
which observes the messages as a stream.
