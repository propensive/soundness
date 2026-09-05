## Money

### About

Money is a fixed-point amount of a particular currency, and Soundness keeps the currency in the
type: a `Money in "EUR"` and a `Money in "GBP"` cannot be added together, because adding euros to
pounds is not a sum but a mistake. Parameterizing by the currency is how an
[impossible state](../philosophy/impossible-states.md) is ruled out by the type.

Within a currency, amounts add, subtract, scale, divide and compare; an amount splits into
equal shares without losing a penny; and a price carries its tax alongside its principal.

Thirty-seven currencies come defined, each a value that constructs amounts, and rendering an
amount uses either its symbol or its ISO code, chosen by a style in scope.

### On money

The two classic mistakes with money are representation and confusion. Representing an amount as a
floating-point number invites rounding errors — `0.1 + 0.2` is not `0.3` in binary floating point —
so money belongs in fixed-point integers of the currency's smallest unit. And an amount without
its currency is a number pretending to be money: a program that adds a dollar figure to a euro
figure produces a plausible-looking result that means nothing.

Soundness stores an amount as an integer count of minor units, and makes the currency part of the
type, so a cross-currency operation does not compile. Division and allocation are handled the way
accounting requires — a split produces shares that differ by at most a minor unit and sum exactly
to the original. Everything comes from the `soundness` package, with the currencies used imported
by name:

```scala
import soundness.*
import currencies.{Eur, Gbp}
import currencyStyles.localCurrencyStyle
```

### Amounts

A currency constructs an amount from a decimal number, rounding to the nearest minor unit; the
type records the currency:

```scala
val price: Money in "EUR" = Eur(3.01)
```

### Arithmetic

Amounts of one currency combine with the usual operators. Multiplying and dividing by a number
scales an amount, and dividing one amount by another yields their ratio:

```scala
Eur(3.01) + Eur(0.02)   // Eur(3.03)
Eur(3.01)*3.0           // Eur(9.03)
Eur(3.01)/3             // Eur(1.00)
-Eur(1.99)              // Eur(-1.99)

Eur(1.01) >= Eur(1.01)  // true
```

Mixing currencies does not compile:

```scala
Eur(1.00) + Gbp(1.00)   // does not compile: different currencies
```

There is no built-in exchange rate — converting between currencies is a business decision with a
time and a source, not an arithmetic identity, so it is left to the program to define.

An amount is a `Long` of minor units underneath, which means its `toString` is a `Long`'s: the raw
count, with no currency and no decimal point. Rendering goes through `show` and the currency style,
as below.

A currency that is not among those provided is one line, giving its ISO code, name, symbol and the
number of minor units in a major one:

```scala
given Nok: ("NOK" is Currency of "Norwegian Krone" in "kr" over 100) = Currency()
```

### Splitting

`share` divides an amount into a number of parts that sum exactly to the whole, distributing any
remainder one minor unit at a time — the allocation problem that naive division gets wrong:

```scala
Eur(3.01).share(3)   // three shares, together exactly €3.01
```

### Prices and tax

Three parties look at the same price and see different numbers: the seller cares about the amount
excluding tax, the buyer about the amount including it, and the tax authority about the difference.
Hold all three in one type and it is easy to add tax to a figure that already had it, or to report
the wrong one — a mistake that arithmetic cannot catch, because every one of those numbers is a
valid amount of money.

A `Price` is therefore a principal amount and the tax on it, kept apart so that either is available
exactly rather than reconstructed by division. `tax` builds one from a rate, and `inclusive` gives
the total:

```scala
val priced = Gbp(2.30).tax(0.2)   // Price(Gbp(2.30), Gbp(0.46))
priced.inclusive                  // Gbp(2.76)
```

Tax rounds *up* to the minor unit, which is what tax authorities require and what a naive
rounding gets wrong in exactly the cases that matter: twenty per cent of £2.94 is £0.588, charged
as £0.59 rather than £0.58.

A `Price` deliberately has no `show`: whether to display the inclusive or exclusive amount is a
decision the application must make explicitly, by showing the member it means.

### Rendering

An amount shows through the currency style in scope: the local style uses the currency's symbol,
and the generic style its ISO code, for contexts where `€3.01` and `3.01 EUR` are each
appropriate:

```scala
t"Received ${Eur(3.01)}"   // t"Received €3.01"

locally:
  import currencyStyles.genericCurrencyStyle
  t"Received ${Eur(3.01)}"   // t"Received 3.01 EUR"
```

### Identifiers

Alongside amounts, the module checks the identifying numbers of finance. An
[ISIN](https://en.wikipedia.org/wiki/International_Securities_Identification_Number) — the
international identifier of a security — validates at compiletime with the `isin"…"` interpolator
and at runtime with `Isin(…)`, including its checksum; and `Luhn` checks the
[Luhn digit](https://en.wikipedia.org/wiki/Luhn_algorithm) used by card numbers:

```scala
isin"GB00BH4HKS39"          // a valid ISIN
isin"GB00BH4HKS3"           // does not compile: wrong length
Luhn.check(17893729974L)    // true
```

An `Isin.Error` says which rule was broken — the length, the country prefix, the permitted
characters, or the Luhn check on the final digit — so a rejected identifier can be reported
precisely rather than as "invalid".

An `Isin` is an opaque type over a `Long`, so a validated identifier costs nothing to hold or
compare, and cannot be confused with an unvalidated string.
