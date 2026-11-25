# A remainder which is never negative

The `%` of the JVM takes the sign of its left operand, so the hour before midnight comes out
as `-1`. `%%` is the modulus of mathematics, which lies between zero and the divisor, and
`**` is a power.

```scala fuel
compile
frame
  label The remainder of the hardware
  highlight %  takes the sign of the dividend
  code
      val hour = -1.0 % 24.0
frame
  label The modulus of mathematics
  highlight %%  always from zero up to the divisor
  code
      val hour = -1.0 %% 24.0
frame
  label And a power
  highlight **  two to the tenth
  code
      val hour = -1.0 %% 24.0
      val kilo = 2.0 ** 10.0
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(hour.inspect)
Out.println(kilo.inspect)
```

[Mathematics](../../soundness/modules/numbers.md#mathematics) lists the other operations.
