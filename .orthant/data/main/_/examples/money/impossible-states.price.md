# A price which keeps its tax apart

The seller, the buyer and the tax authority see three different numbers in one price, and each
is a valid amount of money. A `Price` holds the principal and the tax separately, and has no
form of its own to show, so the code must say which number it means.

```scala fuel
compile
frame
  label A price, shown as though it were one number
  errors show
  code
      import currencies.Gbp, currencyStyles.localCurrencyStyle

      val ticket = Gbp(2.30).tax(0.2)
      val label: Text = ticket.show
frame
  label The amount the buyer pays
  highlight inclusive  principal and tax together
  code
      import currencies.Gbp, currencyStyles.localCurrencyStyle

      val ticket = Gbp(2.30).tax(0.2)
      val label: Text = ticket.inclusive.show
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(ticket.principal.show)
Out.println(ticket.tax.show)
Out.println(label)
```

[Prices and tax](../../soundness/modules/money.md#prices-and-tax) describes the rounding.
