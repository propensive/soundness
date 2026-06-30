## JSON

### About

Soundness reads and writes [JSON](https://en.wikipedia.org/wiki/JSON). Text parses
into a `Json` value; a `Json` value converts to and from ordinary Scala types
without boilerplate; and the conversion is checked as it happens, so asking a
number for a string, or a document for a field it lacks, produces a typed error
rather than a wrong answer. A document can also be validated against a schema
derived from a Scala type, and — most unusually — its fields can be navigated with
the compiler checking each step.

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
cannot be made, it raises a `JsonError` that names the reason and, where it can, the
position in the source.

The same derivation produces a schema, and from a schema two further guarantees
follow: a document can be checked against the shape a type expects, and the fields
of a verified document can be read with the compiler rejecting any path the schema
does not allow. The sections below start with parsing and the everyday conversions,
then build toward schemas, validation, and the integrations with HTTP and time.

These examples assume a few imports — the package, an error strategy, a text
encoding, and a choice of output formatting:

```scala
import soundness.*
import strategies.throwUnsafely
import charEncoders.utf8Encoder
import formatting.compactJsonFormatting
```

### Parsing

Text becomes a `Json` value with `read`:

```scala
val document = t"""{"name": "Alice", "age": 30}""".read[Json]
```

Parsing can fail on malformed input, so it draws on the error strategy in scope;
`throwUnsafely` raises an exception, which suits a tutorial.

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

Person(t"Paul", 81).json.show   // t"""{"name":"Paul","age":81}"""

t"""{"name": "John", "age": 40}""".read[Json].as[Person]
// Person(t"John", 40)
```

The `json` method encodes any value for which an encoder is derived, and `show`
renders the result. Nested case classes and collections of them derive in turn, so a
whole structure converts in one step. Decoding straight from text to a type combines
the two:

```scala
case class Band(guitarists: List[Person], drummer: Person, bassist: Option[Person])

t"""{"guitarists":[{"name":"John","age":40}],"drummer":{"name":"Ringo","age":82}}"""
  .read[Band in Json]
// Band(List(Person(t"John", 40)), Person(t"Ringo", 82), None)
```

### Optional fields and defaults

A field typed as `Optional` (or `Option`) is omitted from the output when it is
absent, and supplied as `Unset` (or `None`) when missing on the way in:

```scala
case class Profile(name: Text, age: Optional[Int])

Profile(t"Eve", Unset).json.show   // t"""{"name":"Eve"}"""
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

Record(t"Ann", 1984).json.show   // t"""{"first_name":"Ann","yob":1984}"""
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

(Shape.Circle(1.0): Shape).json.show
// t"""{"radius":1.0,"kind":"Circle"}"""
```

Decoding reads the discriminator and reconstructs the right case, either as the whole
enumeration or as a known case:

```scala
t"""{"radius":1.0,"kind":"Circle"}""".read[Json].as[Shape]   // Shape.Circle(1.0)
```

`@name` renames a case's discriminator value just as it renames a field, so the wire
form and the Scala name can differ.

### Building JSON

Beyond encoding a typed value, a `Json` value can be assembled directly. `Json.make`
builds an object from named arguments, each itself a `Json`:

```scala
Json.make(a = 1.json, b = t"two".json, c = true.json).show
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

val rest: Map[Text, Json] = Map(t"b" -> 2.json, t"c" -> 3.json)
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

With dynamic access enabled, a field reads as though it were a member, which is
convenient for exploring a document whose shape is known informally:

```scala
import dynamicJsonAccess.enabled

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
import dynamicJsonAccess.enabled

val base = t"""{"x": 1}""".read[Json]
(base.y = 2).show        // t"""{"x":1,"y":2}"""
(base.x = Unset).show    // t"""{}"""
```

Deeper updates are written with a lens, which reaches through several levels and may
carry optics such as `Each` or `Filter` to touch many elements at once:

```scala
import dynamicJsonAccess.enabled, jsonConversion.encodable

case class Role(name: Text)
case class Entity(name: Text, age: Int, roles: List[Role])
case class Org(name: Text, leader: Entity)

val org = Org(t"The Beatles", Entity(t"John", 40, List(Role(t"Leader")))).json
org.lens(_.leader.age = 41.json).as[Org]
// the leader's age updated to 41
```

### Formatting

The output format is a given in scope. Compact formatting omits all whitespace;
indented formatting adds newlines and indentation for reading:

```scala
import formatting.indentedJsonFormatting
List(1, 2, 3).json.show   // pretty-printed across several lines
```

### Errors

A conversion that cannot be made raises a `JsonError` whose reason says what went
wrong: a value of the wrong type, a required field that is absent, or a number
outside the target's range. The reason can be inspected:

```scala
capture[JsonError](t""""abc"""".read[Json].as[Int]).reason
// JsonError.Reason.NotType(JsonPrimitive.String, JsonPrimitive.Number)

capture[JsonError](t"""{}""".read[Json].as[Person]).reason
// JsonError.Reason.Absent
```

When decoding runs under an accruing error strategy, the errors are collected rather
than thrown at the first, and each is tagged with a [JSON Pointer](https://en.wikipedia.org/wiki/JSON_Pointer)
to the field that failed — `#/age` and `#/email` for two missing fields — so a whole
document's problems can be reported at once.

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
Json.unseal(t""""x"""".read[Json]).primitive  // JsonPrimitive.String
```

### Numbers and precision

JSON numbers have no inherent precision limit, and Soundness can keep them exact. The
number mode in scope decides how parsed numbers are stored: `fullNumberMode` retains
every digit as a binary-coded decimal, while `doubleNumberMode` keeps a `Double` where
that is enough. The mode is chosen by import:

```scala
import numberModes.fullNumberMode
```

### NDJSON

[Newline-delimited JSON](https://en.wikipedia.org/wiki/JSON_streaming) — a stream of
independent documents, one per line — is represented by `Ndjson`, which wraps a stream
of `Json` values:

```scala
val stream = Stream(t"1".read[Json], t"2".read[Json], t"3".read[Json])
Ndjson(stream).stream.map(_.as[Int]).to(List)   // List(1, 2, 3)
```

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

### Schemas

A [JSON Schema](https://en.wikipedia.org/wiki/JSON_Schema) describes the permitted
shape of a document. Soundness derives a schema from a Scala type, and a schema value
renders to a standard JSON Schema document:

```scala
(JsonSchema.Integer(): JsonSchema).json.show   // contains "type":"integer"
```

A field can carry a description for the generated schema with the `@memo` annotation,
and string formats such as `date-time` and `email` are represented by
`JsonSchema.Format`. The derived schema is what the next two features rest on.

### Validation

A document is checked against the shape a type expects with `verify`. On success it
returns the same document, now carrying that type, ready to decode or to navigate; on
failure it raises a `JsonError`:

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
```

This closes the usual gap in working with JSON: a path through a document is checked
against the type it is supposed to have, before the program runs.

### Typed records from a schema

Where a schema is given as a JSON Schema document rather than a Scala type, a
`JsonBlueprint` parses it at compiletime and produces typed records from matching
JSON. A blueprint object — here `Catalogue` — holds the schema, and its `record`
method turns a `Json` value into a typed record. Each field reads at the type the
schema declares — a string as `Text`, a number as `Double`, a `format: "email"` field
as an `EmailAddress`, a `pattern` field as a `Regex` — and a field the schema does not
describe is a compile error:

```scala
val record = Catalogue.record(input)
record.name                  // a Text,   per the schema
record.children.head.weight  // a Double, per the schema
```

Constraints in the schema are enforced as the values are read: a value failing a
`pattern` raises a `JsonBlueprintError`, and a number outside a declared `minimum` and
`maximum` raises a bounds error. The shape of the data is taken from the schema and
checked by the compiler, without a Scala type mirroring it.

### Dates and times

Importing the time integration teaches Soundness to encode and decode the temporal
types of its date-and-time library. An `Instant` and a `Duration` travel as a whole
number of milliseconds:

```scala
import chronometries.posix

Instant(1700000000000L).json.show                       // t"1700000000000"
t"5000".read[Json].as[Duration].value                   // 5.0
```

### JSON over HTTP

The HTTP integration makes a `Json` value a request and a response body, served as
`application/json`. A `Json` can be posted to a URL and a response parsed back, so a
JSON API is consumed and offered without any glue between the JSON and HTTP layers —
the encoders and decoders already described carry the values across.
