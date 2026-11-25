# The minor unit is part of the currency

Not every currency has a hundred minor units to the major one. The number is in the currency's
type, after `over`, and rounding and display follow from it with nothing further said.

```scala fuel
compile
frame
  label A dinar of a thousand fils
  highlight 1000  three decimal places
  code
      import currencyStyles.genericCurrencyStyle

      given Kwd: ("KWD" is Currency of "Kuwaiti Dinar" in "KD" over 1000) = Currency()

      val price = Kwd(1.2345)
frame
  label A yen, which has no minor unit
  highlight 1  none at all
    line 3
    occurrence 1
  code
      import currencyStyles.genericCurrencyStyle

      given Jpy: ("JPY" is Currency of "Japanese Yen" in "¥" over 1) = Currency()

      val price = Jpy(1.2345)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(price.show)
```

[Other currencies](../../soundness/modules/money.md#other-currencies) explains each position
of the type.
