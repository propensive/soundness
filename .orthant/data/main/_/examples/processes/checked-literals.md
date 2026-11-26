# A quote which is never closed

A command written with `sh"…"` is split into its arguments as the program compiles, by the
rules a shell would use. A quote which is opened and not closed is found then, and not by
whichever shell would have received it.

```scala fuel
compile
frame
  label An unbalanced quote
  errors show
  code
      val herald = sh"echo 'Enter Viola"
frame
  label Two arguments
  highlight 'Enter..Viola'  one argument, with a space in it
  code
      val herald = sh"echo 'Enter Viola'"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(herald.inspect)
```

[Defining a command](../../soundness/modules/processes.md#defining-a-command) describes the
parsing, and [safety by construction](../../philosophy/safety-by-construction.md) the reason
for doing it early.
