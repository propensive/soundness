## JSON

### About

[JSON](https://en.wikipedia.org/wiki/JSON) text parses into a `Json` value, and a `Json` value
converts to and from ordinary Scala types. A case class needs no annotation and no hand-written
codec: its encoder and decoder are [derived][derivation] from its fields.

Every conversion is checked. Asking a number for a string, or a document for a field it lacks,
raises a typed error which says what was wrong and where. It never yields a `null`, a default,
or a wrong answer.

A document can also be validated against a schema derived from a Scala type. Once it is
validated, its fields can be navigated with the compiler checking each step.

### On JSON

Most libraries treat JSON loosely. A parsed document is an untyped tree, and fields are fished
out of it by string keys. When the data does not hold what the code expects, the mismatch
surfaces as a `null`, a failed cast or a silent default, often far from where it began.

Here the structure and the types stay together:

- Parsing gives a `Json` value, which is [immutable]: every operation on it returns a new value,
  in keeping with [immutability](../philosophy/immutability.md).
- Converting between `Json` and a Scala type is done by an encoder or decoder which the compiler
  derives from the type's shape. A case class becomes an object, and an enumeration becomes a
  tagged union, so there is nothing to write and nothing to keep in step by hand.
- A conversion that cannot be made raises a `Json.Error`. The error names its reason and, where
  it can, the position in the document.
- The same derivation produces a schema. A document checked against the schema can then be read
  with the compiler rejecting any path the schema does not allow.

The sections below begin with the everyday work of parsing, decoding and encoding. Schemas,
validation, streaming and the faster direct codecs come later, and each can be read on its own.

Everything comes from the `soundness` package. Working with JSON also depends on a few choices,
each made by importing a [given] from a [choice package]:

```scala
import soundness.*

import codepages.utf8Codepage
import errorDiagnostics.stackTracesDiagnostics
import formatting.compactJsonFormatting
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

```tabular
column  Import
column  Decides
row  `codepages.utf8Codepage`  how JSON text is written as bytes
row  `errorDiagnostics.stackTracesDiagnostics`  that an error records a stack trace
row  `formatting.compactJsonFormatting`  that JSON is written without whitespace; [Formatting](#formatting) has the alternative
row  `strategies.throwUnsafely`  that an error is thrown as an exception; [errors](errors.md#choosing-a-strategy) has the other strategies
row  `stdios.javaLangSystemStdio`  where the samples print their results
```

### Parsing

`read[Json]` parses any source of text into a `Json` value:

```scala
val document = t"""{"name": "Alice", "age": 30}""".read[Json]
```

`show` writes a `Json` value back out as text, in the formatting in scope:

```scala fuel
run
##
Out.println(document.show)
```

Parsing fails on malformed input, and the failure goes to the [strategy] in scope. Under
`throwUnsafely`, that means an exception which says what the parser found and where:

```scala fuel
run
errors show
##
t"""{"name": "Alice", "age": }""".read[Json]
```

### Reading values

`as` converts a `Json` value to a Scala type. Primitive types, collections and maps are all read
directly:

```scala fuel
run
##
Out.println(t"42".read[Json].as[Int].show)
Out.println(t"3.14".read[Json].as[Double].inspect)
Out.println(t""""hello"""".read[Json].as[Text])
Out.println(t"true".read[Json].as[Boolean].inspect)
Out.println(t"[1, 2, 3]".read[Json].as[List[Int]].inspect)
Out.println(t"""{"a": 1, "b": 2}""".read[Json].as[Map[Text, Int]].inspect)
```

JSON's `null` reads as `Unit`, and as the absent value of an [optional] type.

Asking for the wrong type is an error, not a conversion. A string is never quietly turned into a
number:

```scala fuel
run
errors show
##
t""""hello"""".read[Json].as[Int]
```

### Case classes and enumerations

A case class takes part in JSON as it stands. Its decoder is derived from its fields, so an
object reads into a value:

```scala
case class Person(name: Text, age: Int)

val bob = t"""{"name": "Bob", "age": 40}""".read[Json].as[Person]
```

```scala fuel
run
##
Out.println(bob.inspect)
```

Its encoder is derived in the same way. `in[Json]` encodes a value as `Json`:

```scala fuel
run
##
Out.println(Person(t"Alice", 30).in[Json].show)
```

Nested case classes, and collections of them, derive in turn, so a whole structure converts in
one step:

```scala
case class Crew(rowers: List[Person], cox: Person, reserve: Option[Person])

val crew =
  t"""{"rowers":[{"name":"Bob","age":40}],"cox":{"name":"Carol","age":25}}""".read[Crew in Json]
```

```scala fuel
run
##
Out.println(crew.inspect)
```

`read[Crew in Json]` parses and decodes together. `in`, `as` and `read` are the same vocabulary
for every format, and between them they give each route between text, `Json` and a Scala type:

```tabular
column  Expression
column  Does
column  Yields
row  `value.in[Json]`  encodes a value  `Json`
row  `json.as[Type]`  decodes a `Json` value  `Type`
row  `text.read[Json]`  parses text  `Json`
row  `text.read[Type in Json]`  parses and decodes in one step  `Type`
row  `value.in[Text over Json]`  encodes and renders  `Text`
row  `value.in[Data over Json]`  encodes and renders as bytes  `Data`
```

`in[Text over Json]` writes in the `Json.Formatting` in scope, exactly as `show` does, and
`in[Data over Json]` writes the same text as bytes in the `Codepage` in scope:

```scala
val written: Text = Person(t"Bob", 40).in[Text over Json]
```

```scala fuel
run
##
Out.println(written)
```

[Derivation](derivation.md#when-derivation-fails) explains what happens when a field's type has
no decoder, and
[Decoding](decoding.md) describes the decoders that turn a single text value, such as a JSON
string, into a type of one's own.

### Optional fields and defaults

A field typed as `Optional` is left out of the output when it is absent, and reads as `Unset`
when the document omits it. `Option` behaves the same way, with `None`:

```scala
case class Profile(name: Text, age: Optional[Int])
```

```scala fuel
run
##
Out.println(Profile(t"Eve", Unset).in[Json].show)
Out.println(t"""{"name": "Eve"}""".read[Json].as[Profile].inspect)
```

A field with a default value takes that default when the document omits it:

```scala
case class Account(name: Text, age: Int = 18)
```

```scala fuel
run
##
Out.println(t"""{"name": "Eve"}""".read[Json].as[Account].inspect)
```

A field which is neither optional nor defaulted is required. A document without it does not
decode:

```scala fuel
run
errors show
##
t"""{"age": 40}""".read[Json].as[Person]
```

[Optional values](optional-values.md) describes `Optional` and `Unset` in full.

#### Controlling optionality

An `Optional` field answers three questions. Each answer has a default, which a given from the
`optionalityOptions` package overrides:

```tabular
column  A key which is
column  Reads as
column  Unless
row  omitted from the document  `Unset`  `strictJsonAbsence` makes it an error
row  an explicit null  `Unset`  `strictJsonNulls` hands it to the inner decoder
row  present but rejected by the inner decoder  an error  `lenientJsonFaults` reads it as `Unset`
```

A strict absence raises the reason `Absent`. A strict null is rejected by the inner decoder, as
it would be for any other field. A lenient fault is read as `Unset` and reports nothing. Each is
a separate given, so one can be varied alone:

```scala
val strict =
  import optionalityOptions.strictJsonAbsence
  capture[Json.Error](t"""{"name": "Eve"}""".read[Json].as[Profile]).reason

val lenient =
  import optionalityOptions.lenientJsonFaults
  t"""{"name": "Eve", "age": "old"}""".read[Json].as[Profile].age
```

```scala fuel
run
##
Out.println(strict.inspect)
Out.println(lenient.inspect)
```

`Option` fields follow the same three policies, and give `None`. A declared default of `Unset`
cannot be told apart from no default, so it does not satisfy a strict absence.

The [direct parsers](#skipping-the-tree) honor the policies too, with one limit. They tolerate
a lenient fault only when the value in the document is of the wrong *kind*, such as a string
where a number was expected, because the parser sees the kind before reading the value and can
skip it whole. A value of the right kind which fails part-way, such as a number too large for an
`Int`, has already been partly consumed, so it still raises.

### Renaming fields

A field's name in Scala need not match its key in JSON. The `@name` annotation gives the key to
use. `@name[Json]` confines the rename to JSON, where another format may want a different key:

```scala
case class Record(@name[Json](t"first_name") firstName: Text, @name(t"yob") year: Int)
```

```scala fuel
run
##
Out.println(Record(t"Ann", 1984).in[Json].show)
```

### Tagged unions

An enumeration or sealed hierarchy encodes as an object with an extra *discriminator* field,
which names the case. The key that holds the discriminator is chosen by import, and is `kind`
here:

```scala
import discriminables.jsonByKindDiscriminable

enum Shape:
  case Circle(radius: Double)
  case Square(side: Double)

val circle: Shape = Shape.Circle(1.0)
```

```scala fuel
run
##
Out.println(circle.in[Json].show)
```

Decoding reads the discriminator and builds the right case:

```scala fuel
run
##
Out.println(t"""{"side":2.0,"kind":"Square"}""".read[Json].as[Shape].inspect)
```

`@name` renames a case's discriminator value just as it renames a field, so the name in the
document and the name in Scala can differ.

A discriminator beside the case's own fields is only one convention in use. The others are also
chosen by a `Discriminable` given, not by writing a codec. A *wrapper* puts the whole variant
inside an object with a single key, named for the case. An *envelope* names one field for the
tag and another for the payload:

```scala fuel
alternative Discriminator field
  code
      circle.in[Json].show
alternative Wrapper
  code
      given Shape is Discriminable in Json = Json.DiscriminantWrapper()

      circle.in[Json].show
alternative Envelope
  code
      given Shape is Discriminable in Json = Json.DiscriminantEnvelope(t"type", t"value")

      circle.in[Json].show
##
```

```tabular
column  Convention
column  `Shape.Circle(1.0)` is written as
row  Discriminator field  `{"radius":1.0,"kind":"Circle"}`
row  Wrapper  `{"Circle":{"radius":1.0}}`
row  Envelope  `{"type":"Circle","value":{"radius":1.0}}`
```

Each convention works through the tree and through the [direct parsers](#skipping-the-tree). A
direct parser does not need the tag to come first: a document whose `type` field follows its
`value` reads just as well. A wrapper object with more than one key is not a valid variant, and
raises `Json.Error.Reason.Absent` instead of guessing which key was meant.

Anything more unusual is a `Discriminable` written by hand. It has three methods, which say how
to attach a tag, how to read one, and how to reach the variant. Codecs derived from it work
unchanged.

### Building JSON

A `Json` value can also be assembled directly, without a case class. `Json.make` builds an
object from named arguments, each of them a `Json`:

```scala fuel
run
##
Out.println(Json.make(a = 1.in[Json], b = t"two".in[Json], c = true.in[Json]).show)
```

The `j"…"` [interpolator] writes JSON literally, and its syntax is checked at [compiletime]. A
substitution in a value's place encodes that value. One followed by `*` splices a collection
into an array, and one in a field's place merges a map of fields into an object:

```scala
val x = 42
val xs = List(2, 3, 4)
val rest: Map[Text, Json] = Map(t"b" -> 2.in[Json], t"c" -> 3.in[Json])
```

```scala fuel
run
##
Out.println(j"""{"a": $x}""".show)
Out.println(j"""[1, $xs*]""".show)
Out.println(j"""{"a": 1, $rest}""".show)
```

A malformed literal is rejected where it is written. The error points at the character at
fault, not at the whole expression:

```scala fuel
compile
errors show
##
j""" {"a": 1, } """
```

### Matching JSON

The same `j"…"` interpolator is a pattern. In a `match` it takes a document apart, binding each
substitution to the piece found in its place. A document of another shape falls through to the
next case:

```scala
val answer = t"""{"a": 42}""".read[Json] match
  case j"""{"a": $a}""" => a.as[Int]
  case _                => -1
```

An array pattern matches by length, and a trailing `*` captures the remaining elements:

```scala
val split = t"""[1, 2, 3, 4]""".read[Json] match
  case j"""[$head, $tail*]""" => (head.as[Int], tail.as[List[Int]])
  case _                      => (0, Nil)
```

```scala fuel
run
##
Out.println(answer.show)
Out.println(split.inspect)
```

Each kind of substitution has a meaning as an expression and as a pattern:

```tabular
column  Substitution
column  In an expression
column  In a pattern
row  `$x` in a value's place  the value, encoded as `Json`  binds the value there, as `Json`
row  `$xs*` at the end of an array  splices a collection's elements  binds the remaining elements, as an array
row  `$rest` in a field's place  merges a `Map[Text, Json]` of fields  binds the other fields, as an object
row  `$x` inside a string  the value, encoded as `Text`  not supported
```

[Interpolation](interpolation.md) describes how interpolators like `j"…"` are built.

### Navigating

Applying a `Json` value to a key reaches a field, and applying it to an index reaches an array
element. The steps chain:

```scala
val data = t"""{"a": {"b": {"c": 42}}}""".read[Json]
```

```scala fuel
run
##
Out.println(data(t"a")(t"b")(t"c").as[Int].show)
Out.println(t"[10, 20, 30]".read[Json](1).as[Int].show)
```

Reaching for a field that is not there is not itself an error. It gives an absent `Json`, which
decodes to `Unset` when an optional type is asked for, and raises only when a value is required:

```scala
val person = t"""{"name": "Bob"}""".read[Json]
```

```scala fuel
run
##
Out.println(person(t"missing").as[Optional[Int]].inspect)
```

```scala fuel
run
errors show
##
person(t"missing").as[Int]
```

#### Fields as members

The compiler knows nothing about which fields a `Json` value has, or even whether it is an
object. Often the programmer knows more, and would like to write `person.name`. That syntax
gives up checking, since a misspelled field is found only at [runtime], so it is not available
by default:

```scala fuel
compile
errors show
##
person.name
```

Importing `dynamicAccess.dynamicJson` permits it for the rest of the scope:

```scala
import dynamicAccess.dynamicJson
```

```scala fuel
run
##
Out.println(person.name.as[Text])
```

The import grants a permission, `Json is Dynamical`. The same permission can be confined to one
block with `dynamically`, which names the formats it covers:

```tabular
column  Form
column  Grants dynamic access to
row  `dynamically[Json]`  JSON alone
row  `dynamically[Json | Yaml]`  each format in the union
row  `dynamically`  every format with dynamic access
```

```scala
val config = t"""{"server": {"port": 8080}}""".read[Json]

val port = dynamically[Json]:
  config.server.port.as[Int]
```

A document whose shape is known in advance is better [validated](#validation) once. Its fields
are then members which the compiler does check.

### Updating

Assigning to a field through dynamic access gives a new document. The original is unchanged,
since a `Json` value is immutable. Assigning a value adds or replaces a field, and assigning
`Unset` removes one:

```scala
val base = t"""{"x": 1}""".read[Json]
```

```scala fuel
run
##
Out.println((base.y = 2).show)
Out.println((base.x = Unset).show)
Out.println(base.show)
```

A deeper update is written with a [lens](optics.md). A lens reaches through several levels, and
may carry optics such as `Each` or `Filter` to change many elements at once:

```scala
case class Role(name: Text)
case class Entity(name: Text, age: Int, roles: List[Role])
case class Org(name: Text, leader: Entity)

val org = Org(t"The Beatles", Entity(t"John", 40, List(Role(t"Leader")))).in[Json]

val older =
  import conversions.encodableToJson
  import optics.jsonLens

  org.lens(_.leader.age = 41.in[Json])
```

```scala fuel
run
##
Out.println(older.show)
```

### Formatting

The output format is a given in scope. Compact formatting leaves out all whitespace, and
indented formatting adds newlines and indentation for a human reader:

```scala fuel
run
##
locally:
  import formatting.indentedJsonFormatting

  Out.println(crew.in[Json].show)
```

`show` renders the whole document into one `Text`. A large document need not be held in memory
before it is sent, and the other forms of output deliver it a block at a time. They differ in
where the work of serializing happens, and in what they hand out:

```tabular
column  Form
column  Serializes on
column  Hands out
row  `json.show`  the caller's thread  one `Text` of the whole document
row  `Json.emit[Text](json, deliver)`  the caller's thread  each block as `Text`, to a function
row  `Json.emit[Data](json, deliver)`  the caller's thread  each block as UTF-8 `Data`, to a function
row  `Json.lend(json)`  the caller's thread  the writer's own buffer, lent block by block
row  `Json.emit(json)`  a fiber  an iterator of `Text`
row  `Json` as `Streamable` by `Text`  a fiber  a stream of `Text`
```

The push form of `Json.emit` hands each block to a function as it fills. With `emit[Data]` the
blocks are UTF-8 bytes, encoded straight from the serializer, which suits writing to a file, a
[socket](sockets.md) or an `OutputStream`:

```scala fuel
typecheck
##
val json = Person(t"Bob", 40).in[Json]

Json.emit[Data](json, chunk => socket.write(chunk))
```

`Json.lend` avoids even the copy into each `Data` block. It lends the consumer the writer's own
buffer as a `Region`, together with an interval which proves that every index in it is in range.
Both are valid only for the duration of the call, the same rule as a
[stream's](streams.md) `lend`:

```scala fuel
typecheck
##
Json.lend(json): region =>
  interval => region.visit(interval)(index => sink.put(region(index)))
```

Where the consumer pulls instead, `Json.emit(json)` serializes on a fiber and hands out the text
through an iterator. A `Json` is `Streamable` by `Text` on the same terms. Both need a
[`supervise`](concurrency.md) block:

```scala fuel
typecheck
##
import threads.virtualThreads
import probates.cancelProbate

supervise:
  Json.emit(json).each(chunk => out.write(chunk))
```

### Errors

A conversion that cannot be made raises a `Json.Error`, whose `reason` says what went wrong:

```tabular
column  Reason
column  Raised for
row  `NotType(found, primitive)`  a value of the wrong type
row  `Absent`  a required field that is absent
row  `OutOfRange`  a number outside the target's range
```

`capture` runs an expression that is expected to fail, and gives its error, so that the reason
can be inspected:

```scala fuel
run
##
Out.println(capture[Json.Error](t""""abc"""".read[Json].as[Int]).reason.inspect)
Out.println(capture[Json.Error](t"""{}""".read[Json].as[Person]).reason.inspect)
```

Throwing at the first error is one [strategy] among several. A method that should not throw
declares `raises Json.Error` instead, and its caller chooses what a failure does;
[Errors](errors.md) covers the choices.

#### Collecting every error

Under a strategy that [accumulates failures](errors.md#accumulating-failures), decoding does
not stop at the first error. Every error is collected, and each is tagged with a
[JSON Pointer](https://en.wikipedia.org/wiki/JSON_Pointer) to the field that failed, so all of a
document's problems are reported at once.

The pointer comes from a *focus*, which the decoder maintains as it descends, and which the
strategy reads through `prior`. This contact has a person and an address which are each wrong in
two places:

```scala
import dysasymptotics.linearSize

case class Address(street: Text, postcode: Text)
case class Contact(person: Person, address: Address)

case class Issues(items: List[(Text, Json.Error)] = Nil)(using Diagnostics)
extends Error(m"${items.size} problems"):
  def +(pointer: Text, error: Json.Error): Issues = Issues(items :+ (pointer, error))

val damaged = t"""{"person": {"name": 1}, "address": {"street": 2}}""".read[Json]

val problems =
  Validate[Issues, [r] =>> r raises Json.Error, Json.Focus]
    ( Issues(),
      { case error: Json.Error => accrual + (prior.let(_.pointer.encode).or(t"#"), error) } )
  . protect(damaged.as[Contact])
  . items
```

Decoding yields four errors, one for each fault:

```scala fuel
run
##
problems.each: (pointer, error) =>
  Out.println(t"$pointer: ${error.reason.inspect}")
```

Missing fields and fields of the wrong type accumulate together. An absent field contributes
exactly one error, however many times the decoder tries to read it.

### Equality and structure

Two documents are equal when they mean the same thing. The order of an object's keys does not
matter, and a whole number equals the same value written with a decimal point:

```scala fuel
run
##
Out.println((t"""{"x": 1, "y": 2}""".read[Json] == t"""{"y": 2, "x": 1}""".read[Json]).inspect)
Out.println((t"5".read[Json] == t"5.0".read[Json]).inspect)
```

`Json.unseal` exposes the underlying tree where it is needed directly. The tree has a predicate
for each kind of value, and a `primitive` which names the kind:

```scala fuel
run
##
Out.println(Json.unseal(t"42".read[Json]).isLong.inspect)
Out.println(Json.unseal(t""""x"""".read[Json]).primitive.inspect)
```

The predicates are finer than the primitives. Code which cares how a number is represented can
ask, and code which does not need not:

```tabular
column  Predicate
column  Primitive
column  True for
row  `isLong`  `Number`  a number held as a `Long`
row  `isDouble`  `Number`  a number held as a `Double`
row  `isNumber`  `Number`  any number
row  `isString`  `String`  a string
row  `isBoolean`  `Boolean`  `true` or `false`
row  `isNull`  `Null`  `null`
row  `isObject`  `Object`  an object
row  `isArray`  `Array`  an array
```

### Numbers and precision

A JSON number has no limit on its precision, and most parsers quietly impose one. Here the
*number mode* in scope decides what happens to a number too long for the parser's fast path,
which holds about fifteen digits. A shorter number is decoded identically in every mode:

```scala
import numberModes.fullNumberMode
```

```tabular
column  Mode
column  With a number too long for the fast path
row  `fullNumberMode`  keeps every digit as a binary-coded decimal, at the cost of an allocation
row  `doubleNumberMode`  drops the excess digits and computes a `Double` from the leading ones
row  `bcdNumberMode`  drops the excess digits and yields the raw packed accumulator, with no allocation
```

`fullNumberMode` is the default, because losing digits silently is the wrong thing to do
unasked. `doubleNumberMode` does what Jawn, Circe, Jsoniter and Jackson do. It is the mode for
a like-for-like comparison of speed, or for data known to fit in a `Double`. The `Long` which
`bcdNumberMode` yields holds binary-coded decimal digits, not the number's value, so it buys
speed at the price of treating long numbers as opaque. [Numbers](numbers.md) covers the numeric
types a value can be decoded to.

### NDJSON

[Newline-delimited JSON](https://en.wikipedia.org/wiki/JSON_streaming) is a stream of
independent documents, one on each line. It needs no type of its own. `delineate` splits a
[source](streams.md#records) into records at its line boundaries, and each record is read as
`Json`:

```scala
import lineSeparation.adaptiveLinefeedLineSeparation

val numbers = t"1\n2\n3".source[Text].delineate.records.map(_.read[Json].as[Int]).to[List]
```

```scala fuel
run
##
Out.println(numbers.inspect)
```

Each line is read independently, so the documents need not share a shape. A log of mixed
records reads as well as a uniform one.

### JSON Pointers

A [JSON Pointer](https://datatracker.ietf.org/doc/html/rfc6901) names a location within a
document. The `jp"…"` interpolator writes one, and checks it at compiletime. A pointer can also
be built a segment at a time:

```scala fuel
run
##
Out.println(jp"#/foo/bar".encode)
Out.println(JsonPointer()(t"a")(t"b").encode)
```

Within a segment, the special characters `~` and `/` are escaped as `~0` and `~1`. A malformed
pointer does not compile, and the error points at the character at fault:

```scala fuel
compile
errors show
##
jp"#/foo~2bar"
```

### Source positions

Ordinary parsing keeps no record of where each value sat in the text, since most programs never
ask. A program which does ask, perhaps to underline the source of an error, brings
`parsing.trackPositions` into scope. `read` then records positions, and `locate` gives the line
and column of the value a pointer names:

```scala
import parsing.trackPositions

val tracked = t"{\n  \"a\": 42\n}".read[Json]
```

```scala fuel
run
##
Out.println(tracked.locate(JsonPointer()(t"a")).let(_.line).inspect)
```

A position covers the whole of the value it names. A negative number's extent includes its
minus sign, and an object or array runs from its opening bracket to its closing one. `locateKey`
gives the position of the *key* matching the pointer's last segment, not of its value, which is
what underlining a misspelled field name needs.

A pointer which does not resolve gives `Unset`, whether it names a key the document lacks or an
index past the end of an array. Probing a document for a position is therefore always safe.
Under ordinary parsing, without tracking, every `locate` gives `Unset`, and a program which does
not want positions pays nothing for them.

### Schemas

A [JSON Schema](https://en.wikipedia.org/wiki/JSON_Schema) describes the shape a document is
allowed to have. A schema is derived from a Scala type, and a schema value renders as a standard
JSON Schema document:

```scala fuel
run
##
Out.println((JsonSchema.Integer(): JsonSchema).in[Json].show)
```

The `@memo` annotation on a field gives it a description in the generated schema. String
formats such as `date-time` and `email` are represented by `JsonSchema.Format`. The derived
schema is what [validation](#validation) rests on, and [OpenAPI](openapi.md) uses the same
schemas to describe a service.

### Validation

`verify` checks a document against the shape a type expects. On success it gives the same
document, now carrying that type:

```scala
case class Employee(name: Text, age: Int, email: Text)

val alice = t"""{"name": "Alice", "age": 30, "email": "a@b.c"}""".read[Json]
val employee = alice.verify[Employee]
```

A document which does not conform raises a `Json.Error`:

```scala fuel
run
errors show
##
t"""{"name": "Bob"}""".read[Json].verify[Employee]
```

The verified document carries the schema's structure in its type, so its fields are reached as
members, with full checking and without the `dynamicAccess.dynamicJson` import:

```scala fuel
run
##
Out.println(employee.name.as[Text])
Out.println(employee.as[Employee].inspect)
```

Anything the schema does not permit is a compiletime error. That includes a field which does
not exist, an index into something which is not an array, and a field of a scalar:

```scala fuel
compile
errors show
##
employee.nope
```

```scala fuel
compile
errors show
##
employee.name(0)
```

```scala fuel
compile
errors show
##
employee.name.deeper
```

Navigation runs to any depth, through nested objects and into arrays, and each step is checked:

```scala
case class Workplace(city: Text)
case class Posting(title: Text, workplace: Workplace)
case class Squad(members: List[Person])

val posting = t"""{"title": "Engineer", "workplace": {"city": "Tallinn"}}""".read[Json]
val squad = t"""{"members": [{"name": "Ada", "age": 36}, {"name": "Bob", "age": 41}]}""".read[Json]
```

```scala fuel
run
##
Out.println(posting.verify[Posting].workplace.city.as[Text])
Out.println(squad.verify[Squad].members(1).name.as[Text])
```

It is the schema which licenses this access, not a blanket permission. Reaching a field of an
*unverified* document still needs `dynamicJson`, so the two kinds of access cannot be confused.
A path through a document is thereby checked against the type the document is supposed to have,
before the program runs.

### Overriding one field's encoder

A derived encoder uses, for each field, whichever instance is in scope for that field's type.
A different given in scope would change every field of that type at once, which is rarely what
one awkward field calls for. A `Specific` names a path instead, and its override applies along
that path and nowhere else:

```scala
case class Staffer(name: Text, age: Int)
case class Firm(boss: Staffer, deputy: Staffer)

val shout: Text is Json.Encodable = Json.Encodable(() => Morphology.Str): text => Json(text.upper)
```

With the `Specific` given in scope, only the deputy's name is changed:

```scala fuel
run
##
locally:
  given (Firm is Specific over Json.Encodable) =
    specifically:
      case root.deputy.name() => shout

  Out.println(Firm(Staffer(t"ann", 30), Staffer(t"bob", 40)).in[Json].show)
```

A path which ends at a collection takes an encoder for the collection. Deriving that encoder
again, with a local given in scope, changes how the members of that one list are written without
changing their type's encoding everywhere:

```scala
val nameOnly: Person is Json.Encodable =
  Json.Encodable(() => Morphology.Str): person => Json(person.name)
```

```scala fuel
run
##
locally:
  given (Crew is Specific over Json.Encodable) =
    specifically:
      case root.rowers() =>
        given Person is Json.Encodable = nameOnly
        summon[List[Person] is Json.Encodable]

  Out.println(crew.in[Json].show)
```

### Skipping the tree

`read[Person in Json]` ordinarily parses the text into a `Json` tree, and then decodes the tree.
A type with a `Json.Parsable` instance is instead read *directly* from the source. Its parser is
composed for that type at compiletime, and takes the fields from the text as they arrive, so no
tree is built. The saving is the whole intermediate structure: its allocation, its boxing and a
second traversal.

```scala
given personParsable: (Person is Json.Parsable) = Json.Parsable.derived
```

In ordinary code the instance belongs in the type's companion object, where it is found without
an import.

Writing has a matching instance, `Json.Printable`. A type which has one is written directly,
with no tree, wherever the formatting is compact. Indented formatting still renders through the
tree. Both routes write the same text:

```scala fuel
alternative Tree
  code
      Person(t"Bob", 40).in[Text over Json]
alternative Direct
  code
      val direct =
        given Person is Json.Printable = Json.Direct.derived[Person].printable
        Person(t"Bob", 40).in[Text over Json]
##
```

`Json.Printable.emit` writes the text a block at a time, handing each block to a function as it
fills.

A record can generate both instances at once, into its companion, with `derives Json.Direct`.
A single import then selects the route for every record in scope:

```tabular
column  Import
column  For a record
column  For any other type
row  `codecs.direct`  a generated parser and printer  its own direct instance, else the tree
row  `codecs.derived`  the derived codecs, even over a direct instance  the derived codecs, even over a direct instance
row  `codecs.strict`  a generated parser and printer  its own direct instance, else a compile error naming the type
```

Under `codecs.strict` the error is reported wherever `read[Foo in Json]` or `in[Text over Json]`
would otherwise fall back to the tree. With none of the three imported, a type takes the direct
route exactly when it has a direct instance of its own. The modes are alternatives, so import
one at a time.

The tree remains the right route for a document whose shape is not known in advance, and
wherever the tree itself is what is wanted.

### Typed records from a schema

Sometimes the schema arrives as a JSON Schema document, not as a Scala type. A `Json.Provider`
reads such a schema at compiletime, and produces typed records from JSON which matches it. A
provider object, here `Catalogue`, holds the schema, and its `record` method turns a `Json`
value into a record. Each field of the record has the type the schema declares, and a field the
schema does not describe is a compiletime error:

```scala fuel
typecheck
##
object Catalogue extends Json.Provider(t"""{
  "type": "object",
  "required": ["name", "children"],
  "properties": {
    "name": { "type": "string" },
    "email": { "type": "string", "format": "email" },
    "children": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["weight"],
        "properties": { "weight": { "type": "number", "minimum": 0 } }
      }
    }
  }
}""".read[Json])

val input = t"""{"name": "Bicycle", "children": [{"weight": 9.5}]}""".read[Json]

val record = Catalogue.record(input)
val name: Text = record.name
val weight: Optional[Double] = record.children.head.let(_.weight)
```

The provider takes its schema as anything readable as JSON. A schema kept as a
[classpath](classpath.md) resource is bound directly, as
`object Catalogue extends Json.Provider(cp"/schemas/catalogue.json")`, with a `Classloader` such
as `classloaders.threadContextClassloader` in scope, and is read as the code compiles.

Each construct in the schema has a reading in Scala:

```tabular
column  Schema construct
column  Reads as
row  `"type": "string"`  `Text`
row  `"type": "integer"`  `Int`, or `Long` with `"format": "int64"`
row  `"type": "number"`  `Double`
row  `"type": "boolean"`  `Boolean`
row  a property in the `required` list  its type, with a `Json.Provider.Error` where the document omits it
row  any other property, or one whose type admits `null`  an `Optional`, `Unset` where omitted or `null`
row  an `array` with an `items` schema  a `List` of the items' type
row  an `object` with `properties`  a nested record
row  `additionalProperties`, or `patternProperties` all of one type  a `Map[Text, T]`, empty where absent
row  a `$ref` into the same document  the definition it names
row  an `allOf` of objects  one record with all their properties
row  an `enum` (or `const`) without a `type`  the type of its values
row  a `oneOf` or `anyOf` whose alternatives are all strings  `Text`, an `enum` of all their values where each alternative is one
row  a property admitting several types  the union of their readings, chosen by the kind of value found
```

A `$ref` may take any JSON pointer, `#/definitions/…` and `#/$defs/…` alike. A union arises
from the alternatives of an `anyOf` or `oneOf`, and from a `type` which names several. So
`"exec": string | object` reads as `Text | Record { … }`, and `"ignore": string | array` as
`Text | List[Text]`, and each alternative keeps its own checks. A dictionary nests freely: a
dictionary of lists, a list of dictionaries, an array of arrays. A property named with a hyphen
or a keyword is reached through a backticked name, such as ``record.`content-type` ``.

The schema's constraints are checked as each field is read, as is the value's JSON type:

```tabular
column  Applies to
column  Keywords checked
row  a string  `enum` (or `const`), `pattern`, `minLength`, `maxLength`
row  a number  `minimum`, `maximum`, `exclusiveMinimum`, `exclusiveMaximum`
```

The exclusive bounds are accepted in draft 4's boolean form and in the later numeric form. A
failed check raises a `Json.Provider.Error`. Every error is raised through the [tactic] of the
code which reads the field, so `validate`, with a `venture` for each field,
[collects every problem](errors.md#ventures-and-guards) in a document.

Some constructs cannot be given a type, and remain raw `Json` to be read by hand:

- alternatives which the kind of value cannot tell apart, such as two object shapes in a
  `oneOf`, or an alternative typed as anything;
- a `not` or an `if`;
- an object with neither `properties` nor a value schema;
- an array with no `items` schema, or with a tuple-form `items` list;
- `patternProperties` of differing types;
- a reference to another document;
- a recursive reference, at the point of recursion, since a record type cannot contain itself.

The schema's root must be an object with properties. A root which is an array, a bare string or
a reference to another document is a compiletime error. Where the root offers alternatives, and
one of them is such an object, the provider reads that one.

A property whose name is a method of every JVM object, such as `notify`, `wait`, `toString` or
`hashCode`, cannot be selected by name. It is read through `record.selectDynamic("notify")`,
whose result is the field's reading: cast it to `Record.Field[T, Json.Provider.Error]`, and it
is applied to the `Tactic` in scope.

A format which names a network or identity type reads as whichever type represents that domain
where the record is used, chosen by importing its interface:

```tabular
column  Format
column  Interface
column  Reads as
row  `email`, `idn-email`  `emailAddressInterfaces.soundnessEmailAddress`  `EmailAddress`
row  `hostname`  `hostnameInterfaces.soundnessHostname`  `Hostname`
row  `ipv4`  `ipAddressInterfaces.soundnessIpv4`  `Ipv4`
row  `ipv6`  `ipAddressInterfaces.soundnessIpv6`  `Ipv6`
row  `uri`, `iri`  `urlInterfaces.soundnessUrl`  `HttpUrl`
row  `uuid`  `uuidInterfaces.soundnessUuid`  `Uuid`
```

So with `emailAddressInterfaces.soundnessEmailAddress` imported, `record.email` is an
`EmailAddress`. [Network addresses](network-addresses.md) and [UUIDs](uuid.md) describe these
types.

A provider can equally produce a named tuple, `(name: Text, children: List[(weight: Double)])`,
whose elements are all read when it is built, by declaring a `tuple` method beside `record`.
[Records](records.md) describes both forms and how they differ.

### Dates and times

The temporal types of [Time](time.md) are not encoded in JSON until an import says how. An
`Instant` and a `Duration` each travel as a whole number of milliseconds:

```scala
import chronometries.unixChronometry
import encodables.instantJsonEncodable
import decodables.durationJsonDecodable
```

```scala fuel
run
##
Out.println(Instant(1700000000000L).in[Json].show)
Out.println(t"5000".read[Json].as[Duration].value.inspect)
```

`encodables` and `decodables` each have an instance for both types, and `parsables` has the
instances the [direct parsers](#skipping-the-tree) use.

### JSON over HTTP

A `Json` value is a request body and a response body, sent as `application/json`. The
[HTTP client](http-client.md#posting-a-body) posts one once `postables.jsonPostable` is
imported, and the response's text is read back as `Json`:

```scala fuel
typecheck
##
import charsets.utf8Charset
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import postables.jsonPostable

val reply: Json =
  url"https://example.com/people".submit(Http.Post)(Person(t"Bob", 40).in[Json]).receive[Text].read[Json]
```

With `servables.jsonServable` imported, an [HTTP server](http-server.md#responses) answers a
request with a `Json` value in the same way. No glue is needed between the two layers: the
encoders and decoders described above carry the values across.

### Related

- [YAML](yaml.md), [XML](xml.md), [CBOR](cbor.md), [TEL](tel.md) and [CSV](csv.md) use the same
  `read`, `as` and `in` vocabulary, so a case class which works with one works with the others.
- [Derivation](derivation.md) explains how the codecs for a case class are generated.
- [Decoding](decoding.md) covers reading a single text value as a type.
- [Errors](errors.md) covers the strategies that decide what a `Json.Error` does.
- [Optics](optics.md) covers the lenses used for deep updates.
- [Records](records.md) covers the typed records a schema provides.
