# Mixed units, converted by the compiler

Feet and meters measure the same dimension, so they add, in either order, and the result is in
the SI unit. The ratio between them is a type, known as the code compiles, and a quantity is
a bare `Double` when it runs.

```scala fuel
compile
frame
  label Meters and feet
  highlight 2*Metre..2*Foot  converted, and in meters
  code
      val stage = 2*Metre + 2*Foot
frame
  label Feet and meters, the same length
  highlight 2*Foot..2*Metre  in meters too
    line 2
  code
      val stage = 2*Metre + 2*Foot
      val apron = 2*Foot + 2*Metre
frame
  label An area, from the two
  highlight stage*apron  converted to one unit, squared
  code
      val stage = 2*Metre + 2*Foot
      val apron = 2*Foot + 2*Metre
      val floor = stage*apron
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(3)

Out.println(stage.show)
Out.println(apron.show)
Out.println(floor.show)
```

[Conversion ratios](../../soundness/modules/quantities.md#conversion-ratios) explains why the
rate is a type, and [zero cost](../../philosophy/zero-cost.md) what is left in the bytecode.
