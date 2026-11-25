# Euros and pounds do not add

An amount of money has its currency in its type. A sum of two currencies has no meaning
without an exchange rate, so it has no type either.

```scala fuel
compile
frame
  label A sum in one currency
  code
      import currencies.{Eur, Gbp}

      val fare = Eur(3.01) + Eur(0.02)
frame
  label A sum across currencies
  errors show
  code
      import currencies.{Eur, Gbp}

      val fare = Eur(3.01) + Gbp(0.02)
##
```

[Money](../../soundness/modules/money.md#arithmetic) explains why no rate is built in.
