## Decoding

### About

A running program receives much of its data as loose, untyped text: command-line
arguments, environment variables, configuration files, the fields captured by a
regular expression, a line typed at a prompt. Soundness turns that text into
typed values — an `Int`, a `Uuid`, an email address — and reflects in the types
the one thing every such conversion shares: it might not succeed.

### On conversion

Converting a value from one representation to another can fail in more than one
way, and the differences matter. Reading an integer from arbitrary text can fail,
because the text might not be a number. Narrowing a `Long` to an `Int` can fail,
because the value might not fit. Widening a `Byte` to an `Int`, on the other hand,
can never fail. Treating these alike — returning `null`, throwing from a getter,
or quietly truncating — is where bugs begin.

Soundness keeps them apart by giving each its own typeclass, so the way a
conversion can fail is visible in the code that calls it. A `Decodable`
conversion raises a typed error when it fails. An `Extractable` conversion yields
no result rather than raising, which is exactly what pattern matching wants. An
`Irrefutable` conversion cannot fail at all. The names read as English and state
the direction of the conversion: a value `is Decodable in Text` reads _out of_
text, and text `is Extractable to Int` reads _toward_ an integer.

The everyday surface is small — the `decode` method and the `As` extractor — and
the sections below start there before turning to the typeclasses underneath and
how to write instances for your own types. Everything comes from the `soundness`
package:

```scala
import soundness.*
```

### Decoding a value

The `decode` method reads a typed value out of text. The target type is supplied
explicitly, and a matching `Decodable` instance does the work:

```scala
t"123".decode[Int]   // 123
```

Decoding can fail — the text might not be a number — so it needs an error-handling
strategy in scope. The `throwUnsafely` strategy raises an exception when a decode
fails:

```scala
import strategies.throwUnsafely

t"123".decode[Int]    // 123
t"hello".decode[Int]  // raises a NumberError
```

A `NumberError` reports both the offending text and why it failed: text that is
not a number at all is _unparseable_, while a number outside the target type's
range is _out of range_. Decoding to a `Byte` distinguishes the two:

```scala
t"300".decode[Byte]   // raises a NumberError: 300 is out of range for a Byte
t"abc".decode[Byte]   // raises a NumberError: abc is unparseable
```

Instances come built in for the primitive number types, for `Char`, and for types
defined elsewhere in Soundness such as `Uuid` and `Fqcn`. Other modules add their
own, which is why the same `decode` reads a date, a hostname or a URL once the
relevant module is imported:

```scala
t"2024-01-15".decode[Date]
t"example.com".decode[Hostname]
```

Because the error capability is an ordinary given, the strategy that handles a
failed decode is chosen at the call site, not fixed by the decoder. Recovering
with a default, rather than raising, is a matter of changing the strategy:

```scala
safely(t"hello".decode[Int]).or(0)   // 0
```

### Extracting in patterns

// "raising one" should be "raising an exception".

Pattern matching needs a conversion that declines to match instead of raising one.
That is the `As` extractor: `As[Int]` succeeds when the scrutinee yields an `Int`
and falls through to the next case when it does not.

```scala
t"123" match
  case As[Int](number) => number
  case _               => 0
// 123
```

`As` is an ordinary [extractor](https://docs.scala-lang.org/tour/extractor-objects.html),
so it composes with every pattern Scala offers. It binds the groups of a
[regular expression](https://en.wikipedia.org/wiki/Regular_expression), turning
captured substrings into typed values in one step:

```scala
t"12:13:14" match
  case r"${As[Int](hours)}(\d+):${As[Int](minutes)}(\d+):${As[Int](seconds)}(\d+)" =>
    List(hours, minutes, seconds)
  case _ =>
    List(0, 0, 0)
// List(12, 13, 14)
```

It nests inside larger patterns just as freely — here, deconstructing a list and
decoding each element at once:

```scala
List(t"12", t"13", t"14") match
  case As[Int](first) :: As[Int](second) :: As[Int](third) :: Nil => (first, second, third)
  case _                                                          => (0, 0, 0)
// (12, 13, 14)
```

The extractor works for any type with a suitable instance, not only numbers. An
email address parses when it is well-formed and declines otherwise, so a single
match both validates and converts:

```scala
t"foo@bar.com" match
  case As[EmailAddress](email) => email
  case _                       => email"nobody@example.com"
// email"foo@bar.com"

t"foobar.com" match
  case As[EmailAddress](email) => email
  case _                       => email"nobody@example.com"
// email"nobody@example.com" — no "@", so no match
```

### Narrowing and widening

Numbers convert between widths, and the direction decides whether the conversion
can fail. Widening always succeeds: every `Byte` is an `Int`. Narrowing succeeds
only when the value fits, so it belongs to `As`, which simply declines when it
does not:

```scala
100 match
  case As[Byte](byte) => byte
  case _              => -1
// 100 — fits in a Byte

300 match
  case As[Byte](byte) => byte
  case _              => -1
// -1 — 300 does not fit in a Byte
```

Text narrows to a `Boolean` the same way, matching only the two words that name
one:

```scala
t"true" match
  case As[Boolean](flag) => flag
  case _                 => false
// true
```

An [enumeration](https://en.wikipedia.org/wiki/Enumerated_type) extracts by the
name of its cases, with no instance to write — the cases are read from the type:

```scala
enum Direction:
  case North, South, East, West

t"North" match
  case As[Direction](direction) => direction
  case _                        => Direction.North
// Direction.North
```

### Requiring a value

Some types accept an empty input and some do not, and a form or a configuration
often needs to know which before it asks for a value. A `Requirable` instance
answers that: a type is _required_ when an empty input cannot produce one. The
judgement follows from the type's decoder, so no separate declaration is needed.

### Defining your own

The four typeclasses are the seams along which Soundness extends. Writing an
instance is writing the single conversion function; the framework supplies the
`decode` method, the `As` extractor and the rest.

A `Decodable` instance maps an input to a value, mapping each recognized form to
its result:

```scala
enum Direction:
  case North, South, East, West

object Direction:
  given (Direction is Decodable in Text) =
    case t"N" => North
    case t"S" => South
    case t"E" => East
    case t"W" => West
```

An `Extractable` instance returns an `Optional` result — `Unset` for the inputs it
declines — and it can be built on top of an existing one. A bounded type that
borrows the `Int` extractor and then keeps only the values in its range needs just
a few lines:

```scala
case class Percent(value: Int)

given (Text is Extractable to Int) => Text is Extractable to Percent =
  case As[Int](number) if number >= 0 && number <= 100 => Percent(number)
  case _                                               => Unset
```

The conversions in the standard library follow these same shapes, so a type
defined this way is decoded with `decode`, matched with `As`, and composed into
the patterns of every other type without any further wiring — the payoff of
naming the conversion once and letting its fallibility live in the type.
