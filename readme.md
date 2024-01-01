[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/plutocrat/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/plutocrat/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Plutocrat

__Typesafe representations of monetary values in Scala__

Representing monetary values, which are usually fixed-point decimal values, is a common requirement
for many commercial projects, but can involve a variety of small challenges involving different
currencies, rounding errors, showing monetary amounts, and handling sales taxes. Plutocracy exists
to make these mundane tasks easier.

## Features

- provides simple representations of currencies and monetary values
- monetary values are represented precisely as fixed-point decimals
- currencies are stored in memory as `Long`s using opaque types
- new currencies are easy to add
- implementations of the 16 most traded currencies are included
- introduces another type representing taxed prices
- distinct `Money` and `Price` types avoids common mistakes relating to taxation

## Availability Plan

Plutocrat has not yet been published. The medium-term plan is to build Plutocrat
with [Fury](/propensive/fury) and to publish it as a source build on
[Vent](/propensive/vent). This will enable ordinary users to write and build
software which depends on Plutocrat.

Subsequently, Plutocrat will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

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
 - a currency symbol, often (but not always) a single character, such as `t"
�"` or `t"kr"`
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





## Status

Plutocrat is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Plutocrat is designed to be _small_. Its entire source code currently consists
of 165 lines of code.

## Building

Plutocrat will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Plutocrat?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Plutocrat's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Plutocrat and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `plutocrat`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Plutocrat's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Plutocrat are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/plutocrat/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Plutocrat
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Plutocrat was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

_Plutocracy_ is power through money, which this library provides; at least a representation thereof.

In general, Scala One project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows an imagined currency symbol based on a lambda (λ).

## License

Plutocrat is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

