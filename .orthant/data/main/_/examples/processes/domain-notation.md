# A pipeline, written as a shell writes it

Commands are joined with `|`, as they are in a shell, and the result is a value: a pipeline
which is run and read exactly as a single command is. Each stage is still a checked literal.

```scala fuel
compile
frame
  label A command
  code
      val words = sh"echo 'to be or not to be'"
frame
  label Piped into another
  highlight |  the output of one is the input of the next
  code
      val words = sh"echo 'to be or not to be'" | sh"tr a-z A-Z"
frame
  label And a third
  highlight A-Z"..sh  the pipeline grows, and does not nest
  code
      val words = sh"echo 'to be or not to be'" | sh"tr a-z A-Z" | sh"tr -d ' '"
##
```

```scala fuel
run
##
import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
import logging.silentLogging, strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(words.exec[Text]().trim)
Out.println(words.commands.length.show)
```

[Piping](../../soundness/modules/processes.md#piping) has the rules by which pipelines
combine.
