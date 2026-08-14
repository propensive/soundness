## Records

### About

Some data's shape is defined outside the program — a database table, a JSON Schema, a
configuration format — yet the program should still access its fields with static types. A
`Record` is a value whose fields come from such an external *specification*, read as the code
compiles, so `record.name` typechecks as `Text` and `record.nope` does not compile, without any
Scala class mirroring the schema by hand.

This is the machinery beneath the [JSON](json.md) blueprint feature, where a JSON Schema document
produces typed records, and it is open to any source of schemas a program can read at
compiletime.

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

### Using records

A schema object — here a `JsonBlueprint` built from a JSON Schema document — offers a `record`
method that turns raw data into a typed record:

```scala
val record = Catalogue.record(input)

record.name                  // Text, because the schema says string
record.children.head.weight  // Double, because the schema says number
record.age                   // does not compile if the schema has no age
```

Nested objects become nested records, arrays become lists of records, and every access is checked
against the specification — the same guarantee a hand-written class would give, without the class.

### Defining a specification

A new source of schemas — a database's table definitions, a proprietary format — plugs in by
implementing `Specification`: it supplies the field names and types as a map of `Member`s, and how
a field's value is fetched from the underlying data at runtime. Two typeclasses complete the
picture: an `Intensional` instance for each scalar type name the schema can declare, saying what
Scala type it becomes and how to read it, and a `Structural` instance for the container shapes —
nested objects, arrays.

The schema object then exposes the one-line macro that makes it usable:

```scala
transparent inline def record(json: Json): Record = ${build('json)}
```

From that point, every caller gets records typed by whatever the specification said at the moment
the calling code was compiled.

`record` must be `transparent inline` for any of this to work. Its declared return type is
`Record`, but what it actually returns is a *structural refinement* of it — for a schema of three
fields, the type

```scala
Record { def age: Double; def name: Text; def employed: Boolean }
```

— and only a transparent method lets that more precise type reach the call site. Declared as an
ordinary `inline def`, every field access would fail to compile against the bare `Record`, which
is the whole point lost.

### Compilation order

The schema object must live in a *different file* from the code that calls its `record` method.
The macro runs during the compilation of the call site and evaluates the schema object as a
runtime value — reading the JSON Schema document, querying the database — so that object must
already be compiled. Scala guarantees this ordering for a macro's definition and its use provided
they sit in separate files with no cycle between them; put them in one file and the schema is not
yet available when the macro needs it.

This is also what makes the pattern a type provider in the F# sense: the authoritative schema is
consulted by the compiler, and the types the program is checked against are manufactured from it.
