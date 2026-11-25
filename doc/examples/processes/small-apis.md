# One method reads every kind of output

A command is run with `exec`, and the type named decides what is read from it: the whole of
its output, its lines, or its exit status. There is no method for each.

```scala fuel
compile
frame
  label The output, as text
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      val cast = sh"printf 'Viola\nFeste\n'"
      val text = cast.exec[Text]()
frame
  label Its lines
  highlight List[Text]
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      val cast = sh"printf 'Viola\nFeste\n'"
      val text = cast.exec[Text]()
      val lines = cast.exec[List[Text]]()
frame
  label Its exit status
  highlight Exit
  code
      import environments.javaBaseEnvironment, workingDirectories.javaBaseWorkingDirectory
      import logging.silentLogging, strategies.throwUnsafely

      val cast = sh"printf 'Viola\nFeste\n'"
      val text = cast.exec[Text]()
      val lines = cast.exec[List[Text]]()
      val status = cast.exec[Exit]()
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(lines.inspect)
Out.println(status.inspect)
```

[Running a command](../../soundness/modules/processes.md#running-a-command) lists the types
which can be asked for, and [small APIs](../../philosophy/small-apis.md) explains the single
name.
