# A terminal is typed at only while its program runs

A command run on a pseudo-terminal is a session: the terminal is allocated for a block, and
released when the block ends. A function which would read from the terminal afterwards depends on
something which has been released, and the compiler says so. The text which the program printed
is a value.

```scala fuel
compile
feature experimental.captureChecking
frame
  label A function which would read a released terminal
  errors show
  highlight terminal.text()  would read after the terminal was released
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely
      import errorDiagnostics.stackTracesDiagnostics

      def size() = sh"stty size".pty().session: terminal ?=>
        terminal.resize(132, 50)
        () => terminal.text()
frame
  label Its output, read inside the session
  highlight terminal.text().trim
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely
      import errorDiagnostics.stackTracesDiagnostics

      def size(): Text = sh"stty size".pty().session: terminal ?=>
        terminal.resize(132, 50)
        terminal.text().trim
##
```

The example is compiled, and is not run, since it would start a process.
[On a terminal](../../soundness/modules/processes.md#on-a-terminal) describes pseudo-terminals.
