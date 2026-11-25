# A formula, written as a physicist writes it

A quantity is a number multiplied by a unit, and units multiply and divide like the numbers
beside them. The displacement of a falling body, `s = ut + ½at²`, is written as it stands.

```scala fuel
compile
frame
  label A velocity, a time and an acceleration
  code
      val u = 3*Metre/Second
      val t = 2*Second
      val a = 9.8*Metre/(Second*Second)
frame
  label The first term, a distance
  highlight u*t  velocity by time
  code
      val u = 3*Metre/Second
      val t = 2*Second
      val a = 9.8*Metre/(Second*Second)

      val s = u*t
frame
  label The second term, a distance too
  highlight 0.5*a*t*t  acceleration by time squared
  code
      val u = 3*Metre/Second
      val t = 2*Second
      val a = 9.8*Metre/(Second*Second)

      val s = u*t + 0.5*a*t*t
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(3)

Out.println(s.show)
```

The sum compiles because both terms are lengths.
[Calculations](../../soundness/modules/quantities.md#calculations) writes the formula as a
method, and [elegant prose](../../philosophy/elegant-prose.md) says why notation matters.
