# A program which forgets its exit status

A shell learns whether a command worked from its exit status, and a program which ends
without choosing one has chosen by accident. The body of `application` returns an `Exit`, so
a path through the program which gives none does not compile.

```scala fuel
compile
frame
  label A body which only prints
  errors show
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem, strategies.throwUnsafely
      def prologue(args: List[Text]): Unit = application(args):
        Out.println(t"Two households, both alike in dignity")
frame
  label A body which says how it ended
  highlight Exit.Ok  the status which the shell receives
  code
      import backstops.genericErrorMessageBackstop, executives.directExecutive
      import interpreters.posixInterpreter, systems.javaBaseSystem, strategies.throwUnsafely
      def prologue(args: List[Text]): Unit = application(args):
        Out.println(t"Two households, both alike in dignity")
        Exit.Ok
##
```

The example is compiled, and is not run, since it is a whole program.
[An entry point](../../soundness/modules/cli.md#an-entry-point) describes the body, and
[exit status and errors](../../soundness/modules/cli.md#exit-status-and-errors) the statuses
which have names.
