# A currency defined by its type

A currency which is not among those provided is one line. Its type is a phrase which gives the
ISO code, the name, the symbol and the number of minor units in a major one.

```scala fuel
compile
frame
  label The Danish krone
  highlight "DKK"..100  code, name, symbol, minor units
  code
      import currencyStyles.genericCurrencyStyle

      given Dkk: ("DKK" is Currency of "Danish Krone" in "kr" over 100) = Currency()
frame
  label An amount of it
  highlight Money.."DKK"  the code is the currency's type
  code
      import currencyStyles.genericCurrencyStyle

      given Dkk: ("DKK" is Currency of "Danish Krone" in "kr" over 100) = Currency()

      val ransom: Money in "DKK" = Dkk(1500.50)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(ransom.show)
Out.println((ransom/2).show)
```

[Other currencies](../../soundness/modules/money.md#other-currencies) explains each position,
and [infix types](../../philosophy/infix-types.md) the words between them.
