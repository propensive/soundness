# A transfer measured in bytes and seconds

A `Transfer` is not given bare numbers. The bytes moved are a quantity of bytes, and the time
taken is a duration, so the rate and the estimate which the gauge shows are computed from
values whose units the compiler has checked.

```scala fuel
compile
frame
  label Bytes moved, of a total
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.Transfer
      import counters.transferCounter

      val download = Transfer(96*Mebi(Byte), 512*Mebi(Byte))
frame
  label And the time it took
  highlight 32*Second  gives the gauge a rate, and an estimate
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.Transfer
      import counters.transferCounter

      val download = Transfer(96*Mebi(Byte), 512*Mebi(Byte), 32*Second)
frame
  label A length is not a size
  errors show
  code
      import textMetrics.uniformMetric, palettes.solarizedDarkGaugePalette
      import Tui.Transfer
      import counters.transferCounter

      val download = Transfer(96*Mebi(Metre), 512*Mebi(Byte), 32*Second)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xtermTrueColorTermcap

Out.println(gaugeLine(download, 60))
```

[Quantities](../../soundness/modules/quantities.md) has the units, and
[Statuses](../../soundness/modules/gauges.md#statuses) the status types.
