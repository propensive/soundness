## Money

### About

Money is a fixed-point amount of a particular currency, and the currency is part of its type. A
`Money in "EUR"` and a `Money in "GBP"` cannot be added together, because adding euros to
pounds is not a sum but a mistake. Making the currency a type parameter is how that
[impossible state](../philosophy/impossible-states.md) is ruled out.

Within one currency, amounts add, subtract, scale, divide and compare. An amount splits into
equal shares without losing a penny, and a price carries its tax beside its principal.

Thirty-seven currencies are defined, each as a value which constructs amounts. An amount is
written with either its symbol or its ISO code, as a style in scope decides.

### On money

The two classic mistakes with money are of representation and of confusion.

Representing an amount as a floating-point number invites rounding errors: `0.1 + 0.2` is not
`0.3` in binary floating point. Money belongs in whole numbers of the currency's smallest unit.

An amount without its currency is a number pretending to be money. A program which adds a
figure in dollars to a figure in euros produces a result which looks plausible, and means
nothing.

Here an amount is stored as a whole number of *minor units*, such as cents or pence, and the
currency is part of the type, so an operation across currencies does not compile. Division and
allocation are done as accounting requires: a split produces shares which differ by at most one
minor unit, and which sum exactly to the original.

Everything comes from the `soundness` package. The currencies in use are imported by name, and
the last import lets the samples print their results:

```scala
import soundness.*
import scala.language.experimental.into
import currencies.{Eur, Gbp}
import currencyStyles.localCurrencyStyle
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Amounts

A currency constructs an amount from a decimal number, rounding to the nearest minor unit. The
type records the currency:

```scala
val price: Money in "EUR" = Eur(3.01)
```

```scala fuel
run
##
Out.println(price.show)
```

### Arithmetic

Amounts of one currency combine with the usual operators. Multiplying or dividing by a number
scales an amount:

```scala fuel
run
##
Out.println((Eur(3.01) + Eur(0.02)).show)
Out.println((Eur(3.01)*3.0).show)
Out.println((Eur(3.01)/3).show)
Out.println((-Eur(1.99)).show)
Out.println((Eur(1.01) >= Eur(1.01)).inspect)
```

Mixing currencies does not compile:

```scala fuel
compile
errors show
##
Eur(1.00) + Gbp(1.00)
```

There is no built-in exchange rate. Converting between currencies is a business decision, with
a time and a source, and not an arithmetic identity, so it is left to the program to define.

Underneath, an amount is a `Long` of minor units, so its `toString` is a `Long`'s: the raw
count, with no currency and no decimal point. An amount is written out with `show` and the
[currency style](#rendering).

The operators come from the [typeclasses](numbers.md#overloading-the-operators) which make `+`
and `*` available to any type.

### Other currencies

A currency which is not among those provided is defined in one line, which gives its ISO code,
its name, its symbol and the number of minor units in a major one:

```scala
given Nok: ("NOK" is Currency of "Norwegian Krone" in "kr" over 100) = Currency()
```

Each part of the type has its place:

```tabular
column  Position
column  Gives
column  Here
row  before `is`  the ISO code  `"NOK"`
row  after `of`  the name  `"Norwegian Krone"`
row  after `in`  the symbol  `"kr"`
row  after `over`  the minor units in a major one  `100`
```

### Splitting

`share` divides an amount into a number of parts which sum exactly to the whole. Any remainder
is distributed one minor unit at a time. This is the allocation problem which naive division
gets wrong: three equal shares of €3.01 would lose a cent.

```scala fuel
run
##
Out.println(Eur(3.01).share(3).map(_.show).join(t", "))
```

### Prices and tax

Three parties look at the same price, and see different numbers:

```tabular
column  Party
column  Cares about
column  Member of a `Price`
row  the seller  the amount excluding tax  `principal`
row  the buyer  the amount including tax  `inclusive`
row  the tax authority  the difference  `tax`
```

Hold all three in one type, and it is easy to add tax to a figure which already had it, or to
report the wrong one. Arithmetic cannot catch that mistake, because every one of those numbers
is a valid amount of money.

A `Price` is therefore a principal amount, and the tax on it, kept apart, so that either is
available exactly and need not be reconstructed by division. `tax` builds a price from a rate,
and `inclusive` gives the total:

```scala
val priced = Gbp(2.30).tax(0.2)
```

```scala fuel
run
##
Out.println(priced.principal.show)
Out.println(priced.tax.show)
Out.println(priced.inclusive.show)
```

Tax is rounded *up* to the minor unit. That is what tax authorities require, and what naive
rounding gets wrong in exactly the cases which matter. Twenty per cent of £2.94 is £0.588,
which is charged as £0.59:

```scala fuel
run
##
Out.println(Gbp(2.94).tax(0.2).tax.show)
```

A `Price` deliberately has no `show`. Whether to display the amount with or without tax is a
decision the application must make explicitly, by showing the member it means.

### Rendering

An amount is shown through the currency style in scope. The local style uses the currency's
symbol, and the generic style its ISO code:

```scala fuel
run
##
Out.println(t"Received ${Eur(3.01)}")

locally:
  import currencyStyles.genericCurrencyStyle
  Out.println(t"Received ${Eur(3.01)}")
```

[Internationalization](internationalization.md) covers the wider question of presenting values
for a particular locale.

### Identifiers

The identifying numbers of finance are checked here too. An
[ISIN](https://en.wikipedia.org/wiki/International_Securities_Identification_Number) is the
international identifier of a security. The `isin"…"` [interpolator] validates one at
[compiletime], including its checksum, and `Isin(…)` does the same at runtime:

```scala
val security = isin"GB00BH4HKS39"
```

```scala fuel
compile
errors show
##
isin"GB00BH4HKS3"
```

An `Isin.Error` says which rule was broken, so a rejected identifier can be reported
precisely, and not merely as "invalid":

```tabular
column  Reason
column  Rule
row  `WrongLength`  an ISIN is 12 characters long
row  `BadCountryCode`  it begins with a valid country code
row  `InvalidCharacter`  every character is a digit or an uppercase letter
row  `LuhnCheck`  its last digit passes the Luhn check
```

An `Isin` is an [opaque type] over a `Long`. A validated identifier costs nothing to hold or
compare, and cannot be confused with an unvalidated string.

`Luhn` checks the [Luhn digit](https://en.wikipedia.org/wiki/Luhn_algorithm) which card
numbers use:

```scala fuel
run
##
Out.println(Luhn.check(17893729974L).inspect)
Out.println(Luhn.check(17893729975L).inspect)
```
