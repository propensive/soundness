# Two labels which want the same place

No label can be placed until all of them are known, yet the code asks for each position as
though it were ready, one line after another. `arrange` runs its body once to learn the
labels, and again to answer.

```scala fuel
compile
frame
  label One label
  code
      val towns = arrange:
        val verona = position(60.0, 12.0, 100.0, 100.0)
        verona.attachment
frame
  label A second, at the same point
  highlight mantua  asked for after the first, and arranged with it
  code
      val towns = arrange:
        val verona = position(60.0, 12.0, 100.0, 100.0)
        val mantua = position(60.0, 12.0, 100.0, 100.0)
        (verona.attachment, mantua.attachment)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(towns.inspect)
```

The first label lies to the east of the point, as labels do by preference, and the second
gives way to the west. [On placement](../../soundness/modules/label-placement.md#on-placement)
explains what the body may and may not do, and
[direct style](../../philosophy/direct-style.md) why it has no callback.
