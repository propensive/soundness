## JSON

### About

Soundness reads and writes [JSON](https://en.wikipedia.org/wiki/JSON). Text parses
into a `Json` value; a `Json` value converts to and from ordinary Scala types
without boilerplate; and the conversion is checked as it happens, so asking a
number for a string, or a document for a field it lacks, produces a typed error
rather than a wrong answer. Additionally, a document can be validated against a JSON schema
derived from a Scala type, and its fields can be navigated with the compiler checking each step.

### On JSON

JSON is everywhere, and most libraries treat it loosely: a parsed document is an
untyped tree, fields are fished out by string keys, and a mismatch between what the
code expects and what the data holds surfaces as a `null`, a cast failure, or a
silent default, often far from where it began.

Soundness keeps the structure and the types together. Parsing yields a `Json` value
that remembers what it is. Converting a `Json` to a Scala value, or a Scala value to
`Json`, is done by an encoder or decoder that the compiler derives from the type's
own shape — a case class becomes an object, an enumeration becomes a tagged union —
so there is nothing to write and nothing to keep in step by hand. When a conversion
cannot be made, it raises a `Json.Error` that names the reason and, where it can, the
position in the source.

The same derivation produces a schema, and from a schema two further guarantees
follow: a document can be checked against the shape a type expects, and the fields
of a verified document can be read with the compiler rejecting any path the schema
does not allow. The sections below start with parsing and the everyday conversions,
then build toward schemas, validation, and the integrations with other types.

A JSON value is immutable and every operation on it returns a new one, in keeping with [immutability](../philosophy/immutability.md).

These examples assume a few imports — the package, an error strategy, a text
encoding, and a choice of output formatting:

```scala
import soundness.*

import charEncoders.utf8Encoder
import errorDiagnostics.stackTracesDiagnostics
import formatting.compactJsonFormatting
import strategies.throwUnsafely
```

### Parsing

Any textual source can become a `Json` value with `read`:

```scala
val document = t"""{"name": "Alice", "age": 30}""".read[Json]
```

Parsing can fail on malformed input, so it draws on the error strategy in scope; for now, the
`throwUnsafely` strategy raises an exception on failure.

### Reading values

A `Json` value converts to a Scala type with `as`. The primitive types, collections,
and `null` all read directly:

```scala
t"42".read[Json].as[Int]            // 42
t"3.14".read[Json].as[Double]       // 3.14
t""""hello"""".read[Json].as[Text]  // t"hello"
t"true".read[Json].as[Boolean]      // true
t"null".read[Json].as[Unit]         // ()
t"[1, 2, 3]".read[Json].as[List[Int]]              // List(1, 2, 3)
t"""{"a": 1, "b": 2}""".read[Json].as[Map[Text, Int]]  // Map(t"a" -> 1, t"b" -> 2)
```

### Case classes and enumerations

A case class needs no annotation to take part in JSON. Soundness derives its encoder
and decoder from the fields, so a value converts to an object and back:

```scala
case class Person(name: Text, age: Int)

Person(t"Alice", 30).in[Json].show   // t"""{"name":"Alice","age":30}"""

t"""{"name": "Bob", "age": 40}""".read[Json].as[Person]
// Person(t"Bob", 40)
```

The `in[Json]` method encodes any value for which an encoder is derived, and `show`
renders the result. (`in` is the general codec vocabulary: `value.in[Format]` encodes
and `value.as[Type]` decodes, whatever the format.) Nested case classes and
collections of them derive in turn, so a
whole structure converts in one step. Decoding straight from text to a type combines
the two:

```scala
case class Crew(rowers: List[Person], cox: Person, reserve: Option[Person])

t"""{"rowers":[{"name":"Bob","age":40}],"cox":{"name":"Carol","age":25}}"""
. read[Crew in Json]
// Crew(List(Person(t"Bob", 40)), Person(t"Carol", 25), None)
```

This is not a shorthand for parsing and then decoding. A type with a `Json.Parsable` instance is
read *directly* from the source: the parser is composed for that type as the code compiles, and
fields are taken from the token stream as they arrive, so no `Json` tree is ever built. What is
saved is the whole intermediate structure — allocation, boxing, and a second traversal:

```scala
given Person is Json.Parsable = Json.Parsable.derived
```

The instance belongs in the type's companion object in ordinary code, where it is found without
an import.

The tree route remains for documents whose shape is not known in advance, or where the tree is
itself the point.

### Optional fields and defaults

A field typed as `Optional` (or `Option`) is omitted from the output when it is
absent, and supplied as `Unset` (or `None`) when missing on the way in:

```scala
case class Profile(name: Text, age: Optional[Int])

Profile(t"Eve", Unset).in[Json].show   // t"""{"name":"Eve"}"""
```

A field with a default takes that default when the document omits it:

```scala
case class Account(name: Text, age: Int = 18)

t"""{"name": "Eve"}""".read[Json].as[Account]   // Account(t"Eve", 18)
```

### Renaming fields

A field name in Scala need not match the key in JSON. The `@name` annotation gives
the key to use; `@name[Json]` confines the rename to JSON, where another format might
want a different one:

```scala
case class Record(@name[Json](t"first_name") firstName: Text, @name(t"yob") year: Int)

Record(t"Ann", 1984).in[Json].show   // t"""{"first_name":"Ann","yob":1984}"""
```

### Tagged unions

An enumeration or sealed hierarchy encodes as an object with a discriminator field
naming the case. Which key holds the discriminator is chosen by importing a strategy
— `kind` or `type`:

```scala
import discriminables.jsonByKindDiscriminable

enum Shape:
  case Circle(radius: Double)
  case Square(side: Double)

(Shape.Circle(1.0): Shape).in[Json].show
// t"""{"radius":1.0,"kind":"Circle"}"""
```

Decoding reads the discriminator and reconstructs the right case, either as the whole
enumeration or as a known case:

```scala
t"""{"radius":1.0,"kind":"Circle"}""".read[Json].as[Shape]   // Shape.Circle(1.0)
```

`@name` renames a case's discriminator value just as it renames a field, so the wire
form and the Scala name can differ.

A discriminator field alongside the case's own fields is only one of the conventions in use, and
the others are chosen by a `Discriminable` given rather than by writing a codec. A *wrapper*
encoding puts the whole variant inside a single-key object named for the case:

```scala
given Shape is Discriminable in Json = Json.DiscriminantWrapper()

(Shape.Circle(1.0): Shape).in[Json].show
// t"""{"Circle":{"radius":1.0}}"""
```

An *envelope* encoding names the tag field and the payload field:

```scala
given Shape is Discriminable in Json = Json.DiscriminantEnvelope(t"type", t"value")

(Shape.Square(2.0): Shape).in[Json].show
// t"""{"type":"Square","value":{"side":2.0}}"""
```

Each works both through the tree and through [direct parsing](#case-classes-and-enumerations),
and direct parsing does not require the tag to come first: a document whose `type` field trails
its `value` reads just as one whose tag leads. A wrapper object with more than one key is not a
valid variant, and raises `Json.Error.Reason.Absent` rather than guessing which key was meant.

Anything more exotic is a `Discriminable` written by hand — three methods saying how to attach a
tag, how to read one, and how to reach the variant — and codecs derived from it work unchanged.

### Building JSON

Beyond encoding a typed value, a `Json` value can be assembled directly. `Json.make`
builds an object from named arguments, each itself a `Json`:

```scala
Json.make(a = 1.in[Json], b = t"two".in[Json], c = true.in[Json]).show
// t"""{"a":1,"b":"two","c":true}"""
```

The `j"…"` interpolator writes JSON literally, and checks the syntax as the code
compiles. Holes substitute encoded values, splice a collection into an array with
`*`, or merge a map of fields into an object:

```scala
val x = 42
j"""{"a": $x}"""        // an object with a from a value

val xs = List(2, 3, 4)
j"""[1, $xs*]"""        // [1, 2, 3, 4]

val rest: Map[Text, Json] = Map(t"b" -> 2.in[Json], t"c" -> 3.in[Json])
j"""{"a": 1, $rest}"""  // {"a": 1, "b": 2, "c": 3}
```

A malformed literal is rejected where it is written, with the error focused on the
offending character rather than the whole expression:

```scala
j""" {"a": 1, } """   // does not compile: a trailing comma
```

### Matching JSON

The same `j"…"` interpolator serves as a pattern. In a `match` it deconstructs a
document, binding holes to the pieces it captures, and declining when the shape does
not fit:

```scala
t"""{"a": 42}""".read[Json] match
  case j"""{"a": $a}""" => a.as[Int]
  case _                => -1
// 42
```

Array patterns match by length, and a trailing `*` captures the rest:

```scala
t"""[1, 2, 3, 4]""".read[Json] match
  case j"""[$head, $tail*]""" => (head.as[Int], tail.as[List[Int]])
  case _                      => (0, Nil)
// (1, List(2, 3, 4))
```

### Navigating

A field or an array element is reached by applying the `Json` value to a key or an
index, and the steps chain:

```scala
val data = t"""{"a": {"b": {"c": 42}}}""".read[Json]
data(t"a")(t"b")(t"c").as[Int]   // 42

t"[10, 20, 30]".read[Json](1).as[Int]   // 20
```

A `Json` value is dynamically typed, and the compiler knows nothing about what fields it has —
indeed nothing about whether it is an object, an array or a scalar at all. Often the programmer
knows more, or is content to proceed as though they did, and the syntax should be allowed to say
so. But it is a real loss of checking, so it is not on by default: with dynamic access enabled, a
field reads as though it were a member, and a nonexistent one is discovered at runtime rather than
where it is written.

```scala
import dynamicAccess.dynamicJson

val person = t"""{"name": "Bob"}""".read[Json]
person.name.as[Text]   // t"Bob"
```

Reaching for an absent field is not an error; it yields an absent `Json` that decodes
to `Unset`:

```scala
person(t"missing").as[Optional[Int]]   // Unset
```

### Updating

A field assigned through dynamic access produces a new document — JSON values are
immutable, so an update returns a copy. Assigning a value adds or replaces a field,
and assigning `Unset` removes one:

```scala
import dynamicAccess.dynamicJson

val base = t"""{"x": 1}""".read[Json]
(base.y = 2).show        // t"""{"x":1,"y":2}"""
(base.x = Unset).show    // t"""{}"""
```

Deeper updates are written with a lens, which reaches through several levels and may
carry optics such as `Each` or `Filter` to touch many elements at once:

```scala
import dynamicAccess.dynamicJson, conversions.encodableToJson

case class Role(name: Text)
case class Entity(name: Text, age: Int, roles: List[Role])
case class Org(name: Text, leader: Entity)

val org = Org(t"The Beatles", Entity(t"John", 40, List(Role(t"Leader")))).in[Json]
org.lens(_.leader.age = 41.in[Json]).as[Org]
// the leader's age updated to 41
```

### Formatting

The output format is a given in scope. Compact formatting omits all whitespace;
indented formatting adds newlines and indentation for reading:

```scala
import formatting.indentedJsonFormatting
List(1, 2, 3).in[Json].show   // pretty-printed across several lines
```

### Errors

A conversion that cannot be made raises a `Json.Error` whose reason says what went
wrong: a value of the wrong type, a required field that is absent, or a number
outside the target's range. The reason can be inspected:

```scala
capture[Json.Error](t""""abc"""".read[Json].as[Int]).reason
// Json.Error.Reason.NotType(Json.Primitive.String, Json.Primitive.Number)

capture[Json.Error](t"""{}""".read[Json].as[Person]).reason
// Json.Error.Reason.Absent
```

When decoding runs under an accruing error strategy, the errors are collected rather
than thrown at the first, and each is tagged with a [JSON Pointer](https://en.wikipedia.org/wiki/JSON_Pointer)
to the field that failed — `#/age` and `#/email` for two missing fields — so a whole
document's problems can be reported at once.

The pointer comes from a *focus* the decoder maintains as it descends, and which an accruing
strategy reads through `prior`. Decoding a contact whose person and address are each wrong in
two places yields four errors, pointed at `#/person/age`, `#/person/email` and so on — the whole
tree's faults in one pass, each locatable, rather than the first fault and nothing else:

```scala
case class Address(street: Text, postcode: Text)
case class Contact(person: Person, address: Address)

case class Issues(items: List[(Text, Json.Error)] = Nil)(using Diagnostics)
extends Error(m"${items.size} problems"):
  def +(pointer: Text, error: Json.Error): Issues = Issues(items :+ (pointer, error))

val damaged = t"""{"person": {"name": 1}, "address": {"street": 2}}""".read[Json]

Validate[Issues, [r] =>> r raises Json.Error, Json.Focus]
  ( Issues(),
    { case error: Json.Error => accrual + (prior.let(_.pointer.encode).or(t"#"), error) } )
. protect(damaged.as[Contact])
. items.map(_(0))   // List(t"#/person/name", t"#/person/age", t"#/address/street", t"#/address/postcode")
```

Missing and wrong-typed fields accrue together, and a field that is absent contributes exactly
one error rather than one per attempt to read it.

### Inspecting the structure

Two equal documents compare equal regardless of the order of their object keys, and a
whole number equals the same value written with a decimal point:

```scala
t"""{"x": 1, "y": 2}""".read[Json] == t"""{"y": 2, "x": 1}""".read[Json]   // true
t"5".read[Json] == t"5.0".read[Json]                                       // true
```

When the underlying tree is needed directly, `Json.unseal` exposes it, with
predicates for each kind and a `primitive` naming it:

```scala
Json.unseal(t"42".read[Json]).isLong          // true
Json.unseal(t""""x"""".read[Json]).primitive  // Json.Primitive.String
```

The predicates are finer than the primitives: `isLong` and `isDouble` distinguish the two
numeric representations, and `isNumber` covers both, so code that cares about the difference can
ask, and code that does not need not. `isString`, `isBoolean`, `isNull`, `isObject` and `isArray`
complete the set.

### Numbers and precision

JSON numbers have no inherent precision limit, and most parsers quietly impose one. The number
mode in scope decides what happens to a number too long to hold in the parser's fast path — about
fifteen digits — while everything shorter is decoded identically whatever the mode:

```scala
import numberModes.fullNumberMode
```

`fullNumberMode` keeps every digit, accumulating them as a binary-coded decimal at the cost of an
allocation for that number. It is the default, because losing digits silently is the wrong thing
to do by default.

`doubleNumberMode` drops the overflow and computes a `Double` from the leading digits — matching
what Jawn, Circe, Jsoniter and Jackson do — which is the mode to choose for a like-for-like
throughput comparison, or where the data is known to be within a double's range.

`bcdNumberMode` also drops the overflow but emits the raw packed accumulator with no allocation
at all. The `Long` it yields carries BCD nibbles rather than the number's value, so it is
throughput at the price of treating those numbers as opaque.

### NDJSON

[Newline-delimited JSON](https://en.wikipedia.org/wiki/JSON_streaming) — a stream of
independent documents, one per line — needs no type of its own. A source is split into
records at its line boundaries with `delineate`, and each record is read as `Json`:

```scala
import lineSeparation.adaptiveLinefeedLineSeparation

t"1\n2\n3".source[Text].delineate.records.map(_.read[Json].as[Int]).to(List)
// List(1, 2, 3)
```

Because each line is read independently, the values need not share a shape; a
heterogeneous log reads just as well as a uniform one.

### JSON Pointers

A [JSON Pointer](https://datatracker.ietf.org/doc/html/rfc6901) names a location
within a document. The `jp"…"` interpolator writes one and checks it as the code
compiles, and a pointer can equally be built segment by segment:

```scala
jp"#/foo/bar".encode             // t"#/foo/bar"
JsonPointer()(t"a")(t"b").encode  // t"#/a/b"
```

The special characters `~` and `/` within a segment are escaped as `~0` and `~1`, and
a malformed pointer fails to compile, focused on the character at fault:

```scala
jp"#/foo~2bar"   // does not compile: ~2 is not a valid escape
```

### Source positions

Ordinary parsing keeps no record of where each value sat in the text, since most
programs do not need it. When they do — to underline the source of an error, for
instance — `parseTracked` records positions, and `locate` returns the line and column
of the value a pointer names:

```scala
val tracked = Json.parseTracked(t"{\n  \"a\": 42\n}")
tracked.locate(JsonPointer()(t"a")).let(_.line)   // 2
```

A position covers the whole of the value it names, so a negative number's extent includes its
minus sign, and an object or array is located as the span from its opening brace to its closing
one. `locateKey` gives the position of the *key* matching the final segment rather than its
value, which is what underlining a misspelled field name needs.

A pointer that does not resolve — a key the document lacks, an index past the end of an array —
returns `Unset` rather than raising, so probing a document for a position is safe. Under ordinary,
untracked parsing every `locate` returns `Unset`, which is how a program that does not want the
cost pays none of it.

### Schemas

A [JSON Schema](https://en.wikipedia.org/wiki/JSON_Schema) describes the permitted
shape of a document. Soundness derives a schema from a Scala type, and a schema value
renders to a standard JSON Schema document:

```scala
(JsonSchema.Integer(): JsonSchema).in[Json].show   // contains "type":"integer"
```

A field can carry a description for the generated schema with the `@memo` annotation,
and string formats such as `date-time` and `email` are represented by
`JsonSchema.Format`. The derived schema is what the next two features rest on.

### Validation

A document is checked against the shape a type expects with `verify`. On success it
returns the same document, now carrying that type, ready to decode or to navigate; on
failure it raises a `Json.Error`:

```scala
case class Employee(name: Text, age: Int, email: Text)

val alice = t"""{"name": "Alice", "age": 30, "email": "a@b.c"}""".read[Json]
alice.verify[Employee].as[Employee]
// Employee(t"Alice", 30, t"a@b.c")
```

A document that does not conform fails to verify, which `safely` turns into an absent
result:

```scala
safely(t"""{"name": "Bob"}""".read[Json].verify[Employee]).absent   // true
```

The verified document also carries the schema's structure in its type, so its fields
can be reached with full checking. Reaching a known field, indexing an array, and
descending into a nested object all type-check; anything the schema does not permit is
a compile error:

```scala
alice.verify[Employee].name.as[Text]   // t"Alice"

alice.verify[Employee].nope            // does not compile: no such field
alice.verify[Employee].name(0)         // does not compile: name is not an array
alice.verify[Employee].name.deeper     // does not compile: a scalar has no fields
```

Navigation runs to any depth, through nested objects and into arrays, with each step checked:

```scala
case class Workplace(city: Text)
case class Posting(title: Text, workplace: Workplace)
case class Squad(members: List[Person])

val posting = t"""{"title": "Engineer", "workplace": {"city": "Tallinn"}}""".read[Json]
val squad = t"""{"members": [{"name": "Ada", "age": 36}, {"name": "Bob", "age": 41}]}""".read[Json]

posting.verify[Posting].workplace.city.as[Text]   // t"Tallinn"
squad.verify[Squad].members(1).name.as[Text]      // t"Bob"
```

Verified navigation needs no `dynamicJsonAccess` import — the schema, not a blanket enabler, is
what licenses it. Reaching a field of an *unverified* document still requires the enabler, and
the error says so, so the two kinds of access cannot be confused.

This closes the usual gap in working with JSON: a path through a document is checked
against the type it is supposed to have, before the program runs.

### Overriding one field's encoder

A derived encoder uses, for each field, whatever instance is in scope for that field's type — and
a given in scope changes every field of that type at once, which is rarely what a single
awkward field calls for. A `Specific` names the path instead, so an override applies along one
spine and nowhere else:

```scala
case class Staffer(name: Text, age: Int)
case class Firm(boss: Staffer, deputy: Staffer)

val shout: Text is Json.Encodable = Json.Encodable(() => Morphology.Str): text => Json(text.upper)

given (Firm is Specific over Json.Encodable) =
  specifically:
    case root.deputy.name() => shout

Firm(Staffer(t"ann", 30), Staffer(t"bob", 40)).in[Json].show
// t"""{"boss":{"name":"ann","age":30},"deputy":{"name":"BOB","age":40}}"""
```

The boss's name is untouched: only the path named is overridden. A path ending at a collection
takes an encoder for the collection, so re-deriving it against a local given re-encodes its
elements — the way to change how the members of one list are written without changing that type
everywhere:

```scala
val nameOnly: Person is Json.Encodable =
  Json.Encodable(() => Morphology.Str): person => Json(person.name)

given (Crew is Specific over Json.Encodable) =
  specifically:
    case root.rowers() =>
      given Person is Json.Encodable = nameOnly
      summon[List[Person] is Json.Encodable]
```

### Typed records from a schema

Where a schema is given as a JSON Schema document rather than a Scala type, a
`JsonBlueprint` parses it at compiletime and produces typed records from matching
JSON. A blueprint object — here `Catalogue` — holds the schema, and its `record`
method turns a `Json` value into a typed record. Each field reads at the type the
schema declares — a string as `Text`, a number as `Double`, a `format: "email"` field
as an `EmailAddress`, a `pattern` field as a `Regex` — and a field the schema does not
describe is a compile error:

<!-- doccheck: skip -->
```scala
object Catalogue extends JsonBlueprint(t"""{
  "type": "object",
  "required": ["name", "children"],
  "properties": {
    "name": { "type": "string" },
    "email": { "type": "string", "format": "email" },
    "children": {
      "type": "array",
      "items": { "weight": { "type": "number", "minimum": 0 } }
    }
  }
}""".read[Json].as[JsonBlueprint.Doc])

val input = t"""{"name": "Bicycle", "children": [{"weight": 9.5}]}""".read[Json]

val record = Catalogue.record(input)
record.name                       // t"Bicycle", a Text per the schema
record.children.prim.let(_.weight)   // 9.5, a Double per the schema
```

Constraints in the schema are enforced as the values are read: a value failing a
`pattern` raises a `JsonBlueprint.Error`, and a number outside a declared `minimum` and
`maximum` raises a bounds error. The shape of the data is taken from the schema and
checked by the compiler, without a Scala type mirroring it.

### Dates and times

Importing the time integration teaches Soundness to encode and decode the temporal
types of its date-and-time library. An `Instant` and a `Duration` travel as a whole
number of milliseconds:

```scala
import chronometries.unixChronometry

Instant(1700000000000L).in[Json].show                       // t"1700000000000"
t"5000".read[Json].as[Duration].value                   // 5.0
```

### JSON over HTTP

The HTTP integration makes a `Json` value a request and a response body, served as
`application/json`. A `Json` can be posted to a URL and a response parsed back, so a
JSON API is consumed and offered without any glue between the JSON and HTTP layers —
the encoders and decoders already described carry the values across.
