# A command which knows its own output

Applied without a type, a command gives the output which its name implies. The name is read
from the literal as the program compiles, so `echo` is text and `cat` is a stream of lines,
and the compiler holds each to it.

```scala fuel
compile
frame
  label The output of echo is text
  highlight ()  no type is named
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      val herald: Text = sh"echo Illyria"()
frame
  label The output of cat is not
  errors show
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      val herald: Text = sh"echo Illyria"()
      val prologue: Text = sh"cat prologue.txt"()
frame
  label It is a stream of lines
  highlight Chain[Text]
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      val herald: Text = sh"echo Illyria"()
      val prologue: Chain[Text] = sh"cat prologue.txt"()
##
```

The example is compiled, and is not run, since it reads a file.
[An implied output type](../../soundness/modules/processes.md#an-implied-output-type) says
which commands are known.
