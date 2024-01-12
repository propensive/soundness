### Constructing monetary values

`Money` values can be constructed by applying a `Double` value to a currency object, such as,
`Eur`, `Usd` or `Jpy`:
```scala
import plutocracy.*
val fivePounds = Gbp(5.00)
val sixCents = Usd(0.06)
```

Each value will have the type `Money`, parameterized on the singleton type of its currency, thus:
```scala
val fivePounds: Money[Gbp.type] = Gbp(5.00)
val sixCents: Money[Usd.type] = Usd(0.06)
```

Values are represented internally as `Long`s, with a fixed decimal point.

#### Currencies

A currency is trivial to define, as an object extending `Currency` which has four parameters:
 - a three-letter currency code, such as `t"USD"`
 - a currency symbol, often (but not always) a single character, such as `t"£"` or `t"kr"`
 - the currency name
 - an integer number of currency subunits in each currency unit, e.g. cents in a euro; usually `100`

For example,
```scala
object Nok extends Currency(t"NOK", t"kr", t"Norwegian Krone", 100)
```

### Monetary arithmetic

Addition and subtraction of values of `Money` is possible, provided they have the same currency.
If not, a compile error will be produced.

Multiplication and division are also possible, either by another `Money` value of the same currency,
or by a `Double` value, yielding a `Double` or a `Money` respectively.

#### Rounding

When a multiplication or division by a double value results in an inexact monetary value, rounding
must occur. Since `Money` values are represented as integers, this happens eagerly. Rounding is
"half away from zero", which is not (currently) configurable.

In addition to division, a `split` method is provided on `Money` values which will divide a `Money`
value into an integer number of parts, with the property that their sum is equal to the original
amount, while sacrificing the invariant that every part is equal, if the division is not exact: in
order to maintain the total amount, some parts may be rounded up, while others may be rounded down.
Each part will differ by at most 1 currency subunit.

### Displaying values

As monetary values are opaque `Long` instances, they share `Long`'s `toString` implementation,
which will just display the raw number, without a currency or a decimal point.

Instead, to display a `Money`, the `show` method should be used. This will use the information in
the `Money`'s `Currency` value to print the value correctly, however it requires a contextual
`CurrencyStyle` value, which will format the currency appropriately. A choice of two is provided:
- `plutocrat.currencyStyles.generic`, for currencies in the style, `3.01 EUR`
- `plutocrat.currencyStyles.local`, for currencies in the style, `€3.01`

Different implementations may be provided, if necessary.

### Taxed prices

It's common to need to work with _prices_ which represent an amount which has some tax applied to
it. For example, in many countries a sales tax of about 20% is applied to items bought by
consumers.

For the seller, the untaxed amount is ususally the most important; for the buyer, the tax-inclusive
amount is the only relevant one; and the government is probably most interested in the tax amount.
If all values are represented in code by the same type, it's possible to inadvertently mix them up,
for example by applying a sales tax twice.

There are multiple different ways to protect against this sort of mistake, so Plutocracy provides
one possible solution: a `Price` type which comprises of a `principal` amount (the tax-exclusive
amount) and a `tax` amount, both of which are `Money` instances. `Price`, like `Money` is
parameterized with a currency type.

A `Price` may be constructed from a `Money` with the `tax` method, which takes a tax rate, a
`Double`, as its parameter. For example,
```scala
val price: Price[Eur.type] = Eur(2.99).tax(0.18)
```
would apply an 18% tax to an amount costing `€2.99`.

`Price`s may be added, subtracted and negated, with the principal and tax amounts combined
independently of each other.

However, there is, quite deliberately, no `Show` instance for `Price`. In order to show a price,
its `principal` and `tax` `Money` amounts, or the `inclusive` `Money` amount should be accessed
explicitly, and can be shown appropriately for the context.




