# A complex number, written as a sum

A complex number is a real part added to a multiple of `i`. It is written that way, as an
ordinary expression, and its arithmetic is the arithmetic of its parts.

```scala fuel
compile
frame
  label Two components
  highlight Complex(3.0,..4.0)  real, then imaginary
  code
      val impedance = Complex(3.0, 4.0)
frame
  label A real part, and a multiple of i
  highlight i*4.0  the imaginary unit, scaled
  code
      val impedance = Complex(3.0, 0.0) + i*4.0
frame
  label Its magnitude
  highlight modulus  the distance from zero
  code
      val impedance = Complex(3.0, 0.0) + i*4.0
      val magnitude = impedance.modulus
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(magnitude.inspect)
```

[Complex numbers](../../soundness/modules/mathematics.md#complex-numbers) has the polar form,
and components which carry units.
