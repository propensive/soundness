# Kilometers, milliseconds and kibibytes

A metric prefix is applied to a unit as it is in writing, and gives a larger or smaller unit
of the same dimension. The decimal prefixes scale by powers of ten, and the binary ones by
powers of two.

```scala fuel
compile
frame
  label A distance
  highlight Kilo(Metre)  a thousand meters
  code
      val march = 15*Kilo(Metre)
frame
  label A time
  highlight Milli(Second)  a thousandth of a second
  code
      val march = 15*Kilo(Metre)
      val blink = 300*Milli(Second)
frame
  label Their quotient, in base units
  highlight march/blink  the prefixes cancel
  code
      val march = 15*Kilo(Metre)
      val blink = 300*Milli(Second)
      val haste = march/blink
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(3)

Out.println(march.show)
Out.println(blink.show)
Out.println(haste.show)
```

[Metric prefixes](../../soundness/modules/quantities.md#metric-prefixes) has the full set.
