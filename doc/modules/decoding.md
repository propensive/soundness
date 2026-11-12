## Decoding

### About

A running program receives much of its data as loose, untyped text: command-line arguments,
environment variables, configuration files, the groups captured by a regular expression, a line
typed at a prompt. Decoding turns that text into typed values, such as an `Int`, a `Uuid` or an
email address.

Every such conversion shares one fact: it might not succeed. The types say so. A conversion
never returns a plausible value for input it could not read, which is what
[total transitions](../philosophy/total-transitions.md) demands.

### On conversion

A conversion from one representation to another can fail in more than one way, and the
differences matter:

- Reading an integer from arbitrary text can fail, because the text might not be a number.
- Narrowing a `Long` to an `Int` can fail, because the value might not fit.
- Widening a `Byte` to an `Int` can never fail.

Treating these alike, by returning `null`, throwing from a getter or quietly truncating, is
where bugs begin. Here each has its own [typeclass], so the way a conversion can fail is visible
in the code which calls it:

```tabular
column  Typeclass
column  Written
column  When the conversion fails
row  `Decodable`  `Int is Decodable in Text`  raises a typed error
row  `Extractable`  `Text is Extractable to Int`  yields `Unset`
row  `Irrefutable`  `Byte is Irrefutable to Int`  cannot fail
row  `Identifiable`  `Identifiable[Column]`  cannot fail; it maps a name both ways
```

The names read as English, and state the direction of the conversion. A value which
`is Decodable in Text` is read *out of* text. Text which `is Extractable to Int` is read
*toward* an integer. An `Extractable` gives no result, instead of raising an error, which is
exactly what pattern matching wants.

The everyday surface is small: the `as` method and the `As` extractor. The sections below begin
there, and then turn to the typeclasses underneath and to writing instances for new types.

Everything comes from the `soundness` package. Decoding can fail, so it needs a [strategy] in
scope, and `throwUnsafely` throws an exception when a decode fails. The last import lets the
samples print their results:

```scala
import soundness.*
import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Decoding a value

The `as` method reads a typed value out of text. The target type is named, and a `Decodable`
instance for it does the work:

```scala fuel
run
##
Out.println(t"123".as[Int].show)
```

Text which is not a number raises a `Number.Error`:

```scala fuel
run
errors show
##
t"hello".as[Int]
```

A `Number.Error` has a reason. Text which is not a number at all is *unparseable*, while a
number outside the target type's range is *out of range*. Decoding to a `Byte` shows both:

```scala fuel
run
##
Out.println(capture[Number.Error](t"300".as[Byte]).reason.inspect)
Out.println(capture[Number.Error](t"abc".as[Byte]).reason.inspect)
```

The strategy which handles a failed decode is chosen by the caller, not fixed by the decoder.
[`safely`](errors.md#choosing-a-strategy) turns a failure into `Unset`, so falling back to a
default is one more method call:

```scala fuel
run
##
Out.println(safely(t"hello".as[Int]).or(0).show)
```

A type with no `Decodable` instance cannot be asked for, and the compiler says so:

```scala
class Widget()
```

```scala fuel
compile
errors show
##
t"sprocket".as[Widget]
```

Instances are built in for the primitive number types, for `Char`, and for types defined
elsewhere in Soundness, such as `Uuid` and `Fqcn`. Other parts of Soundness add their own, which
is why the same `as` reads a [date](time.md), a
[hostname or a URL](network-addresses.md#parsing-at-runtime):

```scala
val date = t"2024-01-15".as[Date]
val host = t"example.com".as[Hostname]
```

#### Booleans

A `Boolean` decodes from `true` or `false`, in any case, with or without space around it.
Anything else raises a `Truth.Error`. The other spellings a tool might accept are each a `Truth`
in the package `truths`:

```tabular
column  Given
column  True
column  False
row  `yesNoTruth`  `yes`  `no`
row  `onOffTruth`  `on`  `off`
row  `oneZeroTruth`  `1`  `0`
```

Every `Truth` in scope is accepted beside the default. Importing one adds spellings, and takes
none away:

```scala
import truths.yesNoTruth
```

```scala fuel
run
##
Out.println(t"yes".as[Boolean].inspect)
Out.println(t"True".as[Boolean].inspect)
```

### Policies for optional fields

Every data format which decodes an `Optional` field answers the same three questions, and by
default answers them the same way. That covers [JSON](json.md#optional-fields-and-defaults),
[CBOR](cbor.md), [YAML](yaml.md), [XML](xml.md), [TEL](tel.md) and
[delimited text](csv.md). Taking JSON as the example:

```tabular
column  A field which is
column  Reads as
column  Unless
row  omitted from the document  `Unset`  `strictJsonAbsence` makes it an error
row  an explicit null  `Unset`  `strictJsonNulls` hands it to the inner decoder
row  present but rejected by the inner decoder  an error  `lenientJsonFaults` reads it as `Unset`
```

Each answer is a policy, indexed by the format, with a default in its companion:

```scala fuel
run
##
Out.println(summon[Decodable.Absence in Text].strict.inspect)
Out.println(summon[Decodable.Nullity in Text].strict.inspect)
Out.println(summon[Decodable.Fault in Text].strict.inspect)
```

A format's `optionalityOptions` package offers the other answer to each question, named for the
format. One answer can therefore be changed without touching the others, and JSON can be strict
where, in the same scope, CSV stays lenient. These are the givens which change a default. A
format with no null has no policy for nulls:

```tabular
column  Format
column  Omitted is an error
column  Null goes to the decoder
column  Rejected is `Unset`
row  JSON  `strictJsonAbsence`  `strictJsonNulls`  `lenientJsonFaults`
row  YAML  `strictYamlAbsence`  `strictYamlNulls`  `lenientYamlFaults`
row  CBOR  `strictCborAbsence`  `strictCborNulls`  `lenientCborFaults`
row  TEL
  cell  `strictTelAbsence`
  cell
  cell  `lenientTelFaults`
row  XML
  cell  `strictXmlAbsence`
  cell
  cell  `lenientXmlFaults`
row  Delimited text
  cell  `strictDsvAbsence`
  cell
  cell  `lenientDsvFaults`
```

Each package also names the default answers, `lenientJsonAbsence`, `strictJsonFaults` and so on,
for a scope which must restore one.

A lenient *fault* decodes the value under the ambient tactic's
[`tolerate`](errors.md#tolerating-a-failure), so it records nothing even where errors are being
accumulated. `Option` fields follow the same policies, and give `None`. The tutorial for each
format shows the policies in use.

### Extracting in patterns

Pattern matching needs a conversion which declines to match, instead of raising an error. That
is the `As` extractor. `As[Int]` succeeds when the value being matched yields an `Int`, and
falls through to the next case when it does not:

```scala
def number(text: Text): Int = text match
  case As[Int](number) => number
  case _               => 0
```

```scala fuel
run
##
Out.println(number(t"123").show)
Out.println(number(t"abc").show)
```

`As` is an ordinary [extractor](https://docs.scala-lang.org/tour/extractor-objects.html), so it
composes with every pattern Scala offers. It binds the groups of a
[regular expression](patterns.md), turning captured text into typed values in one step:

```scala
val time = t"12:13:14" match
  case r"${As[Int](hours)}(\d+):${As[Int](minutes)}(\d+):${As[Int](seconds)}(\d+)" =>
    List(hours, minutes, seconds)
  case _ =>
    List(0, 0, 0)
```

It nests inside larger patterns just as freely. This match takes a list apart and decodes each
element at once:

```scala
val triple = List(t"12", t"13", t"14") match
  case As[Int](first) :: As[Int](second) :: As[Int](third) :: Nil => (first, second, third)
  case _                                                          => (0, 0, 0)
```

```scala fuel
run
##
Out.println(time.inspect)
Out.println(triple.inspect)
```

The extractor works for any type with a suitable instance, not only numbers. An email address
is extracted when it is well-formed, and declined otherwise, so a single match both validates
and converts:

```scala
def recipient(text: Text): EmailAddress = text match
  case As[EmailAddress](email) => email
  case _                       => email"nobody@example.com"
```

```scala fuel
run
##
Out.println(recipient(t"hamlet@example.com").show)
Out.println(recipient(t"hamlet.example.com").show)
```

### Narrowing and widening

Numbers convert between widths, and the direction decides whether the conversion can fail.
Widening always succeeds: every `Byte` is an `Int`. Narrowing succeeds only when the value fits,
so it belongs to `As`, which simply declines when it does not:

```scala
def narrow(value: Int): Byte = value match
  case As[Byte](byte) => byte
  case _              => -1
```

```scala fuel
run
##
Out.println(narrow(100).show)
Out.println(narrow(300).show)
```

An [enumeration] is extracted by the name of its cases. There is no instance to write, since
the cases are read from the type:

```scala
enum Direction:
  case North, South, East, West

def direction(text: Text): Optional[Direction] = text match
  case As[Direction](direction) => direction
  case _                        => Unset
```

```scala fuel
run
##
Out.println(direction(t"North").inspect)
Out.println(direction(t"Up").inspect)
```

### Requiring a value

Some types accept an empty input and some do not, and a form or a configuration often needs to
know which before it asks for a value. A type is *required* when an empty input cannot produce
one. That follows from the type's decoder, so no separate declaration is needed. The decoder is
simply run against the empty text, and whether it succeeds is the answer:

```scala fuel
run
##
Out.println(safely(t"".as[Int]).present.inspect)
Out.println(safely(t"".as[Text]).present.inspect)
```

An `Int` is required, since the empty text is not a number. A `Text` is not, since the empty
text is a `Text`. A [form](forms.md) marks its mandatory fields this way, without a second
declaration which could disagree with the decoder.

### Enumerations by name and position

An enumeration's cases are known to the compiler, so its `Enumerable` instance is
[derived][derivation] from the type, not written. The instance gives the enumeration's cases in
the order they were declared, and looks a case up by name or by [ordinal](numbers.md#ordinals).
Each lookup gives an `Optional`, since neither a name nor a position is guaranteed to correspond
to a case:

```scala
val enumerable = summon[Direction is Enumerable]
```

```scala fuel
run
##
Out.println(enumerable.values.inspect)
Out.println(enumerable.value(t"North").inspect)
Out.println(enumerable.value(t"Up").inspect)
Out.println(enumerable.value(Prim).inspect)
Out.println(enumerable.name(Direction.South))
Out.println(enumerable.index(Direction.East).show)
```

This is the machinery beneath `As[Direction]`, beneath [bit flags](flags.md), and beneath the
handling of enumerations in every format's derived codecs, so all of them agree about what a
case is called.

### Round-tripping an identifier

Some conversions are not to a Scala type at all, but from one textual form to another: a
database column named `first_name` and a field named `firstName`, or a header written
`Content-Type` and read as `contentType`. An `Identifiable` states both directions of such a
mapping, as a pair of functions on text:

```scala
case class Column()

val snakeCase = Identifiable[Column](_.uncamel.snake, _.cut(t"_").camel)
```

The two functions convert a name into the convention and back, using the
[case conventions](text.md#case-conventions) of text.

`Identifiable` is a `Typeclass.Pure`, so the compiler knows that the conversion captures
nothing. It may be shared freely, and applied wherever the naming convention is needed, without
being passed along as an argument.

### Defining your own

The four typeclasses are where decoding is extended. Writing an instance means writing the one
conversion function. The `as` method, the `As` extractor and the rest follow from it.

A `Decodable` instance maps each form of input it recognizes to a value. This one reads a
compass heading from a word:

```scala
enum Heading:
  case N, S, E, W

object Heading:
  given (Heading is Decodable in Text) =
    case t"north" => N
    case t"south" => S
    case t"east"  => E
    case t"west"  => W
```

```scala fuel
run
##
Out.println(t"north".as[Heading].inspect)
```

An `Extractable` instance gives an `Optional` result, which is `Unset` for the inputs it
declines. It can be built on an existing instance. This bounded type borrows the extractor for
`Int`, and keeps only the values in its range:

```scala
case class Percent(value: Int)

given (Text is Extractable to Int) => Text is Extractable to Percent =
  case As[Int](number) if number >= 0 && number <= 100 => Percent(number)
  case _                                               => Unset
```

The conversions built into Soundness follow these same shapes. A type defined this way is
decoded with `as`, matched with `As`, and composed into the patterns of every other type, with
no further wiring. The conversion is named once, and its fallibility lives in its type.

### Related

- [JSON](json.md#case-classes-and-enumerations) and the other formats decode a whole document
  into a case class, using a `Decodable` instance for each text field.
- [Command-line parameters](cli.md) and [environment variables](environment.md) are decoded
  with `as`.
- [Patterns](patterns.md) captures the groups which `As` converts.
- [Errors](errors.md) covers the strategies which decide what a failed decode does.
- [Internationalization](internationalization.md), [interpolation](interpolation.md), [paths](paths.md), [sockets](sockets.md), [UUIDs](uuid.md) and [versioning](versioning.md) decode their values from text.
