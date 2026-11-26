# A split which loses nothing

Three equal shares of €100.00 do not exist. Division rounds each share, and the shares no
longer make the whole. `share` distributes the remainder a cent at a time, so the parts always
sum to the amount which was split.

```scala fuel
compile
frame
  label Division, which rounds
  code
      import currencies.Eur, currencyStyles.localCurrencyStyle

      val purse = Eur(100.00)
      val third = purse/3
frame
  label Shares, which sum to the whole
  highlight share(3)  three parts, no cent lost
  code
      import currencies.Eur, currencyStyles.localCurrencyStyle

      val purse = Eur(100.00)
      val third = purse/3
      val shares = purse.share(3)
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println((third*3.0).show)
Out.println(shares.map(_.show).join(t", "))
```

[Splitting](../../soundness/modules/money.md#splitting) describes the allocation.
