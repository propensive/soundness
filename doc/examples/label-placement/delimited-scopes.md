# A label is placed only within an arrangement

Where a label goes depends on every other label, so one cannot be placed alone. `position`
needs the arrangement which `arrange` opens, and outside that block there is nothing for it
to ask.

```scala fuel
compile
frame
  label A label with no arrangement
  errors show
  code
      val verona = position(60.0, 12.0, 100.0, 100.0)
frame
  label A label within one
  highlight arrange:  the labels asked for in its body are arranged together
  code
      val verona = arrange:
        position(60.0, 12.0, 100.0, 100.0)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(verona.attachment.inspect)
```

[On placement](../../soundness/modules/label-placement.md#on-placement) describes the two
runs of the block, and [delimited scopes](../../philosophy/delimited-scopes.md) the pattern.
