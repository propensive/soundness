# A path in a shell command

A path is substituted into a command as it is, with no conversion to text. However its names
are spelled, it is one argument, so a space in a name does not split it in two.

```scala fuel
compile
frame
  label A path with a space in a name
  code
      val sonnet: Path on Linux = % / "home" / "viola" / "sonnet 18.txt"
frame
  label The path, as an argument
  highlight $sonnet  one argument, whatever it contains
  code
      val sonnet: Path on Linux = % / "home" / "viola" / "sonnet 18.txt"
      val command = sh"wc -l $sonnet"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(command.inspect)
```

The command is built, and is not run.
[Processes](../../soundness/modules/processes.md) describes how arguments are substituted,
and [composability](../../philosophy/composability.md) why no adapter is needed.
