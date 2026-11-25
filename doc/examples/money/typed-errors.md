# An identifier which arrives as text

An ISIN written in the source is checked by the compiler. One which arrives as the program
runs is checked then, by the same rules, and that the check can fail is in the type.

```scala fuel
compile
frame
  label The failure is not accounted for
  errors show
  code
      def security(code: Text): Isin = Isin(code)
frame
  label It is declared
  highlight raises..Isin.Error  the caller decides
  code
      def security(code: Text): Isin raises Isin.Error = Isin(code)
frame
  label Or an invalid code is no security at all
  highlight safely  `Unset` if a rule is broken
  code
      def security(code: Text): Optional[Isin] = safely(Isin(code))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(security(t"GB00BH4HKS39").present.inspect)
Out.println(security(t"GB00BH4HKS38").present.inspect)
```

[Identifiers](../../soundness/modules/money.md#identifiers) lists the reasons an `Isin.Error`
gives.
