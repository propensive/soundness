## Records

### About

Some data's shape is defined outside the program — a database table, a JSON Schema, a
configuration format — yet the program should still access its fields with static types. A
`Record` is a value whose fields come from such an external *specification*, read as the code
compiles, so `record.name` typechecks as `Text` and `record.nope` does not compile, without any
Scala class mirroring the schema by hand.

This is the machinery beneath the [JSON](json.md), [XML](xml.md) and [TEL](tel.md) *providers*,
where a JSON Schema, an XML Schema or a TEL schema document produces typed records, and beneath
the [OpenAPI](openapi.md) client's `record()`, where an operation's response schema does. It is
open to any source of schemas a program can read at compiletime.

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
import codepages.utf8Codepage
import strategies.throwUnsafely
```

A record typed by its schema at compiletime is [safety by construction](../philosophy/safety-by-construction.md) with no class written by hand.

### Using records

A provider object — here a `Json.Provider` built from a JSON Schema document, declared in a file
of its own — offers a `record` method that turns raw data into a typed record:

```scala fuel
typecheck
##
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
}""".read[Json])

val input = t"""{"name": "Bicycle", "children": [{"weight": 9.5}]}""".read[Json]
```

```scala fuel
typecheck
##
val record = Catalogue.record(input)

record.name                  // Text, because the schema says string
record.children.head.let(_.weight)  // Double, because the schema says number
record.age                   // Optional[Int]: not required by the schema
record.nope                  // does not compile: the schema has no nope
```

Nested objects become nested records, arrays become lists, a field the schema does not require
becomes an `Optional`, and every access is checked against the specification — the same
guarantee a hand-written class would give, without the class.

### Tuples instead of records

The same schema object can produce a [named
tuple](https://docs.scala-lang.org/scala3/reference/other-new-features/named-tuples.html) instead
of a record, through its `tuple` method, the twin of `record`.

For the schema above, `Catalogue.tuple(input)` has the type
`(name: Text, age: Optional[Int], children: List[(weight: Double)])`: one element per field, named
as the schema names it, in the schema's order. Nested objects become nested named tuples and arrays
become lists of them. A named tuple is accessed by name like a record, but it is also an ordinary
tuple, so it destructures positionally and converts to its unnamed form:

```scala fuel
typecheck
##
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

This changes how fields are typed. Every field can fail as it is read — a required value which
is absent, a value of the wrong kind, one which breaks the schema's constraints — and fails with
the specification's own error, such as `Json.Provider.Error`, through the `Tactic` of the code
which reads it. In a record, a field reads as `Record.Field[Int, Json.Provider.Error]`, a context
function over that `Tactic`, applied wherever the field is selected, so each access needs a
handler in scope. In a tuple, the failure can only happen during construction, so the element's
type is plainly `Int`, and the handler must be in scope where the tuple is built: the call does
not compile without one.

Since every failure goes through the caller's `Tactic`, a reader can choose how to meet them:
`attempt` or `recover` one field, or `validate` and `track` a whole record with a `venture` per
field, which collects every problem in a document before reporting them together.

In summary:

```tabular
column  Aspect
column  Record
column  Tuple
row  Data is read  at each field access  once, when the tuple is built
row  A malformed field fails  when it is accessed  the construction
row  A field of schema type integer has type  `Record.Field[Int, Json.Provider.Error]`  `Int`
row  The `Tactic` must be in scope  wherever a field is selected  where the tuple is built
row  Destructures positionally  no  yes
```

### Defining a specification

A new source of schemas — a database's table definitions, an XML Schema, a proprietary format —
plugs in by implementing `Specification`: a *provider*. It supplies the field names and types as
an ordered list of `Member`s (a tuple's elements follow that order), and primitives over the
underlying data:

```tabular
column  Hook
column  Default
column  Supplies
row  `access`  none: must be implemented  a named field's value, fetched from a value
row  `absent`  none: must be implemented  whether a fetched value stands for a missing field
row  `repeated`  none: must be implemented  every value of a field which occurs many times
row  `required`  returns the value as it is  the value of a field read under `One`; overridden to fail when it is absent
row  `entries`  fails  a keyed field's values by name
row  `kind`  fails  the kind of a value, by which a union chooses its alternative
row  `elements`  fails  the elements of a value which is itself a sequence
row  `pairs`  fails  the named values of a value which is itself a dictionary
row  `unoffered`  fails  the failure for a value of a kind no alternative of a union offers
```

The values of a repeated field are the elements of a JSON array, say, or every sibling with the
field's keyword in a format which repeats the field. A format which has unions or dictionaries
provides `entries`, `kind`, `elements` and `pairs`; one which does not leaves their defaults.

A `Member` is a `Value`, naming the scalar type the schema declared, a `Record` of further
members, or a `Union` of alternatives each keyed by the *kind* of value which selects it, and each
carries a `Multiplicity`:

```tabular
column  Multiplicity
column  Reads as
column  Through
row  `One`  the value  `access`, then `required`
row  `Optional`  an `Optional`, `Unset` where `absent`  `access` and `absent`
row  `Many`  a `List`  `repeated`
row  `Keyed`  a `Map[Text, _]`  `entries`
```

The provider says nothing further about optionality or repetition; the macro applies the
multiplicity itself, and a union's alternative may carry its own, so a single-alternative union
nests one multiplicity inside another.

A provider names the error its fields fail with as its `Failure` type, and the primitives which
can fail — `access`, `repeated`, `required`, `entries`, `elements`, `pairs` and `unoffered` —
take the `Tactic` for it. One typeclass completes the picture: an `Intensional` instance for each
scalar type name the schema can declare, saying what Scala type it becomes and how to read a
single value of it. There are three ways to make one:

```tabular
column  Constructor
column  From
row  `Intensional(accessor)`  a function of the value
row  `Intensional.parametric`  a function of the value and the member's parameters, for a type such as a bounded integer
row  `Intensional.Reading`  a function of the value, its parameters and a `Tactic`, for a reading which can fail
```

A reading which can fail — a value of the wrong kind, a value outside its bounds, a string not
matching its pattern — is an `Intensional.Fallible`, whose `transform` takes the `Tactic` for its
`Error`. Its `Error` must be the provider's `Failure` or a subtype of it, so that every field
fails in one way; an instance which raises anything else is a compile error. The instances live
in the provider's companion, so they are found without an import. A provider
over XML, for instance, would fetch a child element by its label, find a field absent where no
such child exists, and repeat over every child with the label.

Every specification object has `record` and `tuple` from `Specification` itself: nothing need
be declared in the object beyond its schema. Both are `transparent inline`, expanded where they
are called with the receiver — the specification object — known, so the macro can find that
object by its type and evaluate its `fields`. The declared return type of `record` is `Record`,
but what a call actually has is a *structural refinement* of it — for a schema of three fields,
the type

```scala fuel
typecheck
##
type Person = Record { def age: Double; def name: Text; def employed: Boolean }
```

— and only a transparent method lets that more precise type reach the call site.

### Compilation order

The schema object must live in a *different file* from the code that calls its `record` method.
The macro runs during the compilation of the call site and evaluates the schema object as a
runtime value — reading the JSON Schema document, querying the database — so that object must
already be compiled. Scala guarantees this ordering for a macro's definition and its use provided
they sit in separate files with no cycle between them; put them in one file and the schema is not
yet available when the macro needs it.

This is also what makes the pattern a type provider in the F# sense: the authoritative schema is
consulted by the compiler, and the types the program is checked against are manufactured from it.
