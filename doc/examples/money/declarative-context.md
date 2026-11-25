# The symbol or the code, chosen once

How an amount is written is not a property of the amount. A currency style in scope decides
between the currency's symbol and its ISO code, for every amount which is shown there.

```scala fuel
compile
alternative The local style
  highlight localCurrencyStyle  the currency's symbol
  code
      import currencies.Gbp
      import currencyStyles.localCurrencyStyle

      val receipt: Text = t"Received ${Gbp(3.01)} from Shylock"
alternative The generic style
  highlight genericCurrencyStyle  the ISO code
  code
      import currencies.Gbp
      import currencyStyles.genericCurrencyStyle

      val receipt: Text = t"Received ${Gbp(3.01)} from Shylock"
##
```

[Rendering](../../soundness/modules/money.md#rendering) shows both results, and
[declarative context](../../philosophy/declarative-context.md) why the style is not an
argument.
