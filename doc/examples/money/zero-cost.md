# An amount is a whole number of cents

Binary floating point cannot hold a tenth, so sums of decimal fractions drift. An amount of
money is a `Long` of the currency's smallest unit, with the currency only in its type, so its
arithmetic is exact and costs what integer arithmetic costs.

```scala fuel
compile
frame
  label A sum of fractions
  code
      val exact: Boolean = 0.1 + 0.2 == 0.3
frame
  label A sum of money
  highlight Eur(0.10)..Eur(0.30)  integer arithmetic, on cents
  code
      import currencies.Eur

      val exact: Boolean = Eur(0.10) + Eur(0.20) == Eur(0.30)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println((0.1 + 0.2 == 0.3).inspect)
Out.println(exact.inspect)
```

[Arithmetic](../../soundness/modules/money.md#arithmetic) says what an amount is underneath,
and [zero cost](../../philosophy/zero-cost.md) why that matters.
