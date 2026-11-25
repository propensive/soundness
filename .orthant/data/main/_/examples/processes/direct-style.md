# The output of one command, in the arguments of the next

Running a command is an expression, and its output is a value. A second command takes that
value as an argument on the next line, with no callback and no stream to drain between them.

```scala fuel
compile
frame
  label The output of a command
  highlight author  a value, once the command has finished
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      val author = sh"echo Shakespeare"().trim
frame
  label Used as an argument
  highlight $author  the output of the first command
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      val author = sh"echo Shakespeare"().trim
      val credit = sh"echo 'Written by' $author"().trim
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(credit)
```

[Running a command](../../soundness/modules/processes.md#running-a-command) says when each
kind of result returns, and [direct style](../../philosophy/direct-style.md) why waiting is
written as a plain call.
