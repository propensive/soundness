# A path, as an argument

A path is substituted into a command as it is, without being turned into text first. It
becomes one argument, so a directory with a space in its name needs no quoting.

```scala fuel
compile
frame
  label A path
  code
      val folio: Path on Linux = (% / "srv" / "first folio" / "hamlet.txt").on[Linux]
frame
  label Placed in a command
  highlight $folio  one argument
  code
      val folio: Path on Linux = (% / "srv" / "first folio" / "hamlet.txt").on[Linux]
      val count = sh"wc -l $folio"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(count.inspect)
```

[Substitution and escaping](../../soundness/modules/processes.md#substitution-and-escaping)
says what else can be substituted, and [paths](../../soundness/modules/paths.md) how a path is
built.
