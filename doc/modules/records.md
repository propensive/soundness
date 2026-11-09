## Records

### About

The shape of some data is defined outside the program: by a database table, a JSON Schema or a
configuration format. The program should still read its fields with static types.

A `Record` is a value whose fields come from such an external *specification*, which is read as
the code compiles. So `record.name` typechecks as `Text`, and `record.nope` does not compile,
and no class is written by hand to mirror the schema.

The [JSON](json.md#typed-records-from-a-schema), [XML](xml.md) and [TEL](tel.md) *providers*
are built on this. A JSON Schema, an XML Schema or a TEL schema document produces typed records.
So does the response schema of an operation, through `record()` in the [OpenAPI](openapi.md)
client. Any source of schemas which a program can read at [compiletime] can do the same.

### On external schemas

When the authoritative shape of the data lives elsewhere, a case class written by hand is a
copy, and copies drift. The schema gains a field, the class does not, and the mismatch appears
at [runtime]. The usual alternative is to read fields dynamically, by string. That gives up
checking altogether.

A `Record` avoids both. The schema is read once, as the program compiles. Scala's
[structural types](https://docs.scala-lang.org/scala3/book/types-structural.html) then build
the exact interface of the record from it, with every field at the type which the schema
declares. The schema cannot drift from the code, because the types in the code *are* the
schema's. This is [safety by construction](../philosophy/safety-by-construction.md), with no
class to maintain.

Everything comes from the `soundness` package. The other imports choose an encoding, and a
[strategy](errors.md#choosing-a-strategy) for errors:

```scala
import soundness.*
import codepages.utf8Codepage
import strategies.throwUnsafely
```

### Using records

A provider object holds a schema. This one is a `Json.Provider`, built from a JSON Schema
document. In a real program it is declared in a file of its own, for the reason which
[compilation order](#compilation-order) explains. For the same reason, the samples on this page
are checked but not run:

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

The `record` method of the provider turns raw data into a typed record. Each field has the type
which the schema gives it. `name` is a string in the schema, so it is a `Text`. `weight` is a
number, so it is a `Double`. The schema does not require `age`, so it is an
[`Optional[Int]`](optional-values.md):

```scala fuel
typecheck
##
val record = Catalogue.record(input)

val name: Text = record.name
val weight: Optional[Double] = record.children.head.let(_.weight)
val age: Optional[Int] = record.age
```

The schema has no field called `nope`, so `record.nope` does not compile.

A nested object becomes a nested record, and an array becomes a list. Every access is checked
against the specification. A class written by hand would give the same guarantee, but there is
no class.

### Tuples instead of records

The same provider can produce a
[named tuple](https://docs.scala-lang.org/scala3/reference/other-new-features/named-tuples.html)
instead of a record, through `tuple`, the twin of `record`.

For the schema above, `Catalogue.tuple(input)` has the type
`(name: Text, age: Optional[Int], children: List[(weight: Double)])`. It has one element for
each field, named as the schema names it, in the order of the schema. Nested objects become
nested named tuples, and arrays become lists of them.

A named tuple is read by name, like a record. It is also an ordinary tuple, so it is taken apart
by position, and converts to its unnamed form:

```scala fuel
typecheck
##
val tuple = Catalogue.tuple(input)

val name: Text = tuple.name
val (first, second, third) = tuple
val plain: (Text, Optional[Int], List[(weight: Double)]) = tuple.toTuple
```

#### Eager and lazy

The two forms differ in *when* the data is read.

A record is lazy. It holds the raw data, and each access reads and converts its field afresh. A
malformed field is noticed only when that field is read, and a field read twice is converted
twice.

A tuple is eager. Every field is read once, when the tuple is built, and the tuple holds the
converted values. A malformed field fails the construction of the tuple, whether or not the
program ever reads it.

This changes the types of the fields. Reading any field can fail: a required value may be
absent, a value may be of the wrong kind, or it may break a constraint of the schema. It fails
with the specification's own error, such as `Json.Provider.Error`, through the [tactic] of the
code which reads it.

In a record, a field has the type `Record.Field[Int, Json.Provider.Error]`. That is a context
function over the tactic, applied wherever the field is selected, so each access needs a handler
in scope. In a tuple, a failure can happen only during construction. The type of the element is
plainly `Int`, and the handler must be in scope where the tuple is built.

Since every failure goes through the caller's tactic, a reader chooses how to meet it. One field
can be wrapped in `attempt` or `recover`. A whole record can be read with `validate` and
`track`, with a `venture` for each field, which collects every problem in a document and reports
them together. [Errors](errors.md) describes these.

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

A new source of schemas plugs in by implementing `Specification`. Such an implementation is a
*provider*. The source might be the table definitions of a database, an XML Schema, or a
proprietary format.

A provider supplies the names and types of the fields as an ordered list of `Member` values. The
elements of a tuple follow that order. It also supplies primitive operations over the underlying
data:

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

The values of a repeated field are the elements of a JSON array, say. In a format which repeats
a field, they are every sibling with the field's keyword. A format which has unions or
dictionaries provides `entries`, `kind`, `elements` and `pairs`. One which does not leaves their
defaults.

A `Member` is one of three things. A `Value` names the scalar type which the schema declared. A
`Record` holds further members. A `Union` holds alternatives, each keyed by the *kind* of value
which selects it. Every member carries a `Multiplicity`:

```tabular
column  Multiplicity
column  Reads as
column  Through
row  `One`  the value  `access`, then `required`
row  `Optional`  an `Optional`, `Unset` where `absent`  `access` and `absent`
row  `Many`  a `List`  `repeated`
row  `Keyed`  a `Map[Text, _]`  `entries`
```

The provider says nothing more about optionality or repetition. The macro applies the
multiplicity itself. An alternative of a union may carry its own, so a union with a single
alternative nests one multiplicity inside another.

A provider names the error which its fields fail with as its `Failure` type. The primitives
which can fail are `access`, `repeated`, `required`, `entries`, `elements`, `pairs` and
`unoffered`, and each takes the tactic for that error.

One [typeclass] completes the picture. There is an `Intensional` instance for each scalar type
name which the schema can declare. It says which Scala type the name becomes, and how to read a
single value of it. There are three ways to make one:

```tabular
column  Constructor
column  From
row  `Intensional(accessor)`  a function of the value
row  `Intensional.parametric`  a function of the value and the member's parameters, for a type such as a bounded integer
row  `Intensional.Reading`  a function of the value, its parameters and a `Tactic`, for a reading which can fail
```

A reading may fail: the value may be of the wrong kind, or outside its bounds, or a string which
does not match its pattern. Such a reading is an `Intensional.Fallible`, whose `transform` takes
the tactic for its `Error`. That `Error` must be the provider's `Failure`, or a subtype of it,
so that every field fails in one way. An instance which raises anything else is a compile error.

The instances live in the companion of the provider, so they are found without an import. A
provider over XML, for instance, would fetch a child element by its label, find a field absent
where no such child exists, and repeat over every child with the label.

Every specification object gets `record` and `tuple` from `Specification` itself. Nothing need
be declared in the object beyond its schema. Both methods are `transparent inline`. They are
expanded where they are called, with the specification object known, so the macro can find that
object by its type and evaluate its `fields`.

The declared return type of `record` is `Record`. What a call actually has is a *structural
refinement* of it. For a schema of three fields, it is a type such as this:

```scala fuel
typecheck
##
type Person = Record { def age: Double; def name: Text; def employed: Boolean }
```

Only a transparent method lets that more precise type reach the place where it is called.

### Compilation order

The schema object must be in a *different file* from the code which calls its `record` method.

The macro runs while the calling code compiles. It evaluates the schema object as a runtime
value, by reading the JSON Schema document or querying the database. So that object must already
be compiled. Scala guarantees this order for a macro and its use, provided they are in separate
files with no cycle between them. If they are in one file, the schema is not yet available when
the macro needs it.

This is also what makes the pattern a type provider, in the sense of F#. The compiler consults
the authoritative schema, and the types which the program is checked against are built from it.

### Related

- [JSON](json.md#typed-records-from-a-schema), [XML](xml.md) and [TEL](tel.md) each describe
  the provider for their own schema language.
- [OpenAPI](openapi.md) builds records from the response schemas of an API.
- [Errors](errors.md) describes `attempt`, `recover`, `validate` and `track`, which meet the
  failure of a field.
- [Optional values](optional-values.md) describes the type of a field which the schema does not
  require.
- [Metaprogramming](metaprogramming.md) describes the macros which a provider relies on.
- [Catalogs](catalogs.md), [databases](database.md) and [tables](tables.md) work with the fields of a record by name.
