# One word makes a program resident

A command-line program becomes a daemon by wrapping its body in `cli` and not in
`application`. The body does not change: `Out` was never the JVM's global output, but the
standard output of whichever invocation is in scope, and now there are many.

```scala fuel
compile
frame
  label A program which starts a JVM each time
  highlight application(args)  one process for each invocation
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem
      import strategies.throwUnsafely, threads.virtualThreads

      def herald(args: List[Text]): Unit = application(args):
        Out.println(t"Enter Viola")
        Exit.Ok
frame
  label A program which stays running
  highlight cli  each invocation is forwarded to the same process
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem
      import strategies.throwUnsafely, threads.virtualThreads

      def herald(): Unit = cli:
        Out.println(t"Enter Viola")
        Exit.Ok
##
```

The example is compiled, and is not run, since it defines a whole program.
[A daemon application](../../soundness/modules/daemons.md#a-daemon-application) describes what
the launcher forwards, and [declarative context](../../philosophy/declarative-context.md) why
nothing about the caller is global.
