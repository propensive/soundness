# A length is not a time

A quantity's units are part of its type, so the compiler checks the dimensions of every
calculation. When they disagree, it says so as a physicist would, without a type in sight.

```scala fuel
compile
frame
  label A distance added to a time
  errors show
  code
      val sprint = 100*Metre + 9.58*Second
frame
  label A distance divided by a time
  highlight /  a speed, in metres per second
  code
      val sprint = 100*Metre/(9.58*Second)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

given decimalizer: Decimalizer = Decimalizer(3)

Out.println(sprint.show)
```

[Quantities](../../soundness/modules/quantities.md#arithmetic-and-dimensions) has the rest of
the arithmetic, and [expressive errors](../../philosophy/expressive-errors.md) the reasons for
wording an error this way.
