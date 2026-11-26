# The width of text depends on who measures it

Padding text to a width needs a way to measure it, and a terminal shows some characters two
columns wide. The metric is chosen by an import, and the same call pads differently under
each.

```scala fuel
compile
alternative As a terminal shows it
  highlight textMetrics.wideCharacterWidthMetric  some characters fill two columns
  code
      import textMetrics.wideCharacterWidthMetric

      val title = t"ハムレット".pad(12, Rtl, '.')
alternative Every character is one column
  highlight textMetrics.uniformMetric  every character fills one
  code
      import textMetrics.uniformMetric

      val title = t"ハムレット".pad(12, Rtl, '.')
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(title)
```

Each of the five characters of *Hamlet* in Japanese fills two columns of a terminal, so two
dots make up the twelve. Under the uniform metric there would be seven.
[Padding and fitting](../../soundness/modules/text.md#padding-and-fitting) and
[Graphemes and width](../../soundness/modules/text.md#graphemes-and-width) have the details.
