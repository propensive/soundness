# A filename cannot split a command

A value substituted into a command is one argument, whatever characters it contains. A command
is a list of arguments and never a string for a shell to split again, so a space or a
semicolon in a filename has nowhere to take effect.

```scala fuel
compile
frame
  label A name with a space in it
  highlight $file  one argument
  code
      val file = t"twelfth night.txt"
      val command = sh"cat $file"
frame
  label A name which would end the command
  highlight $file  still one argument
  code
      val file = t"lear.txt; rm -rf /"
      val command = sh"cat $file"
frame
  label A list of values
  highlight $files  an argument for each element
  code
      val files = List(t"twelfth night.txt", t"lear.txt; rm -rf /")
      val command = sh"cat $files"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(command.inspect)
Out.println(command.arguments.length.show)
```

[Substitution and escaping](../../soundness/modules/processes.md#substitution-and-escaping)
has the rule for each kind of value, and
[impossible states](../../philosophy/impossible-states.md) the principle.
