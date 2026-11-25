# An exit status is an answer

`grep` exits with a nonzero status when it finds nothing, which is an answer and not a
failure. Asking a command for its `Exit` gives the status as a value with two cases, and
nothing is thrown for either.

```scala fuel
compile
frame
  label The status of the last command
  highlight Exit
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      def status(name: Text): Exit =
        (sh"echo 'Alas, poor Yorick'" | sh"grep -q $name").exec[Exit]()
frame
  label Both of its cases
  highlight Exit.Fail(_)  an ordinary answer
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      def status(name: Text): Exit =
        (sh"echo 'Alas, poor Yorick'" | sh"grep -q $name").exec[Exit]()

      def mourned(name: Text): Boolean = status(name) match
        case Exit.Ok      => true
        case Exit.Fail(_) => false
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(mourned(t"Yorick").inspect)
Out.println(mourned(t"Falstaff").inspect)
```

[Failure](../../soundness/modules/processes.md#failure) explains why a nonzero status is the
caller's to interpret, and [total transitions](../../philosophy/total-transitions.md) why it
is not an exception.
