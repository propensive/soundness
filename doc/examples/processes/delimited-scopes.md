# A terminal for the length of a block

Some programs behave differently when their output is a terminal. `pty` describes a command on
a pseudo-terminal, and `session` allocates one, lends the running command to a block, and
hangs up on it when the block ends.

```scala fuel
compile
frame
  label A command, described on a terminal of a given size
  highlight pty(100,  nothing is run yet
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely
      import errorDiagnostics.stackTracesDiagnostics

      val shell = sh"sh -c 'read line; stty size'".pty(100, 30)
frame
  label A session with it
  highlight session:  the terminal exists until the block ends
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely
      import errorDiagnostics.stackTracesDiagnostics

      val shell = sh"sh -c 'read line; stty size'".pty(100, 30)

      def size(): Text = shell.session: terminal ?=>
        terminal.resize(132, 50)
        t"\r".writeTo(terminal)
        terminal.text().trim
##
```

The example is compiled, and is not run, since it would start a process on a terminal.
[On a terminal](../../soundness/modules/processes.md#on-a-terminal) describes sessions, and
[delimited scopes](../../philosophy/delimited-scopes.md) the reason for lending.
