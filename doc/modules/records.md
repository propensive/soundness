## Records

### About

Some data's shape is defined outside the program — a database table, a JSON Schema, a
configuration format — yet the program should still access its fields with static types. A
`Record` is a value whose fields come from such an external *specification*, read as the code
compiles, so `record.name` typechecks as `Text` and `record.nope` does not compile, without any
Scala class mirroring the schema by hand.

This is the machinery beneath the [JSON](json.md) and [TEL](tel.md) *providers*, where a JSON
Schema or TEL schema document produces typed records, and it is open to any source of schemas a
program can read at compiletime.

### On external schemas

When the authoritative shape of the data lives elsewhere, a hand-written case class is a copy, and
copies drift: the schema gains a field, the class does not, and the mismatch surfaces at runtime.
The usual alternative — accessing fields dynamically by string — gives up checking entirely,
trading a maintenance problem for a correctness one.

A `Record` avoids both. The schema is read once, as the program compiles, and Scala's
[structural types](https://docs.scala-lang.org/scala3/book/types-structural.html) manufacture the
record's exact interface from it — every field with the type the schema declares. The schema
cannot drift from the code, because the code's types *are* the schema's. Everything comes from the
`soundness` package:

```scala
import soundness.*
```

A record typed by its schema at compiletime is [safety by construction](../philosophy/safety-by-construction.md) with no class written by hand.

### Using records

A provider object — here a `Json.Provider` built from a JSON Schema document, declared in a file
of its own — offers a `record` method that turns raw data into a typed record:

<!-- doccheck: skip -->
```scala
object Catalogue extends Json.Provider(t"""{
  "type": "object",
  "required": ["name", "children"],
  "properties": {
    "name": { "type": "string" },
    "age": { "type": "integer" },
    "children": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["weight"],
        "properties": { "weight": { "type": "number", "minimum": 0 } }
      }
    }
  }
}""".read[Json]):
  transparent inline def record(json: Json): Record = ${build('json)}
```

<!-- doccheck: skip -->
```scala
val input = t"""{"name": "Bicycle", "children": [{"weight": 9.5}]}""".read[Json]
val record = Catalogue.record(input)

record.name                  // Text, because the schema says string
record.children.prim.let(_.weight)  // Double, because the schema says number
record.age                   // Optional[Int]: not required by the schema
record.nope                  // does not compile: the schema has no nope
```

Nested objects become nested records, arrays become lists, a field the schema does not require
becomes an `Optional`, and every access is checked against the specification — the same
guarantee a hand-written class would give, without the class.

### Tuples instead of records

The same schema object can produce a [named
tuple](https://docs.scala-lang.org/scala3/reference/other-new-features/named-tuples.html) instead
of a record, through a second one-line macro beside `record`:

<!-- doccheck: skip -->
```scala
object Catalogue extends Json.Provider(schema):
  transparent inline def record(json: Json): Record = ${build('json)}
  transparent inline def tuple(json: Json): NamedTuple.AnyNamedTuple = ${tuple('json)}
```

For the schema above, `Catalogue.tuple(input)` has the type
`(name: Text, age: Optional[Int], children: List[(weight: Double)])`: one element per field, named
as the schema names it, in the schema's order. Nested objects become nested named tuples and arrays
become lists of them. A named tuple is accessed by name like a record, but it is also an ordinary
tuple, so it destructures positionally and converts to its unnamed form:

<!-- doccheck: skip -->
```scala
val tuple = Catalogue.tuple(input)
tuple.name                        // Text
val (name, age, children) = tuple // in the schema's order
tuple.toTuple                     // (Text, Optional[Int], List[(weight: Double)])
```

#### Eager and lazy

The two forms differ in *when* the data is read. A record is lazy: it holds the raw data, and
each field access reads and converts the field afresh, so a malformed field is only noticed when
that field is accessed, and a field accessed twice is converted twice. A tuple is eager: every
field is read once, when the tuple is built, and the tuple holds the converted values. A malformed
field therefore fails the construction of the tuple, whether or not the program ever reads it.

This changes how fallible fields are typed. A specification may declare a field's type as, say,
`Int raises Json.Provider.Error`, meaning reading it can fail. In a record, that is the field's
type, and each access needs a handler in scope. In a tuple, the failure can only happen during
construction, so the element's type is plainly `Int`, and the handler must be in scope where the
tuple is built: a `raises` clause is discharged there, by whichever `Tactic` the call site
provides, and the call does not compile without one.

### Defining a specification

A new source of schemas — a database's table definitions, an XML Schema, a proprietary format —
plugs in by implementing `Specification`: a *provider*. It supplies the field names and types as
an ordered list of `Member`s (a tuple's elements follow that order), and three primitives over
the underlying data: `access`, fetching a named field's value from a value; `absent`, whether a
fetched value stands for a missing field; and `repeated`, every value of a field which occurs
many times — the elements of a JSON array, or every sibling with the field's keyword in a format
which repeats the field. A `Member` is a `Value`, naming the scalar type the schema declared, a
`Record` of further members, or a `Union` of alternatives each keyed by the *kind* of value
which selects it, and each carries a `Multiplicity`: `One`, `Optional` (read as `Optional`,
`Unset` where `absent`), `Many` (read as a `List`, through `repeated`) or `Keyed` (read as a
`Map[Text, _]`, through `entries`). The provider says nothing further about optionality or
repetition; the macro applies the multiplicity itself, and a union's alternative may carry its
own, so a single-alternative union nests one multiplicity inside another. A format which has
unions or dictionaries provides the further hooks `kind`, `elements`, `pairs` and `entries`;
one which does not leaves their defaults.

A fourth primitive, `required`, has a default: it is applied to the value of a field read under
`Multiplicity.One`, and a provider overrides it to fail in its own way when the value is absent.
One typeclass completes the picture: an `Intensional` instance for each scalar type name the
schema can declare, saying what Scala type it becomes and how to read a single value of it.
`Intensional(accessor)` makes one from a function; `Intensional.parametric` from a function
which also takes the member's parameters, for a type such as a bounded integer. A reading which
can fail — a value outside its bounds, a string not matching its pattern — is an
`Intensional.Fallible`, whose `transform` takes the `Tactic` for its `Error`: the field then
reads as `Result raises Error`, and the tactic is supplied where the field is read. The
instances live in the provider's companion, so they are found without an import. A provider
over XML, for instance, would fetch a child element by its label, find a field absent where no
such child exists, and repeat over every child with the label.

The schema object then exposes the one-line macro that makes it usable:

<!-- doccheck: skip -->
```scala
transparent inline def record(json: Json): Record = ${build('json)}
```

From that point, every caller gets records typed by whatever the specification said at the moment
the calling code was compiled.

`record` (and likewise `tuple`) must be `transparent inline` for any of this to work. Its declared return type is
`Record`, but what it actually returns is a *structural refinement* of it — for a schema of three
fields, the type

<!-- doccheck: skip -->
```scala
Record { def age: Double; def name: Text; def employed: Boolean }
```

— and only a transparent method lets that more precise type reach the call site. Declared as an
ordinary `inline def`, every field access would fail to compile against the bare `Record`, which
is the whole point lost.

A record's underlying data is `record.data`, an extension rather than a member, so that a
schema may declare a field named `data`; the members `Record` itself declares are `recordData`,
`recordAccess` and `selectDynamic`, and a field whose name is a method of every JVM object
(`toString`, `notify`, …) is reachable only through `selectDynamic`.

### Compilation order

The schema object must live in a *different file* from the code that calls its `record` method.
The macro runs during the compilation of the call site and evaluates the schema object as a
runtime value — reading the JSON Schema document, querying the database — so that object must
already be compiled. Scala guarantees this ordering for a macro's definition and its use provided
they sit in separate files with no cycle between them; put them in one file and the schema is not
yet available when the macro needs it.

This is also what makes the pattern a type provider in the F# sense: the authoritative schema is
consulted by the compiler, and the types the program is checked against are manufactured from it.
