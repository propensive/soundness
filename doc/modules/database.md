## Databases

### About

An in-memory store for typed entities and the relations between them: values are stored once and
identified by typed references, and the relations a database supports are declared *in its type*,
so relating two entities the schema does not connect is a compile error. It is the shape of a
relational database — entities, foreign keys, joins — for the many programs whose working data fits
in memory and whose schema should be checked by the compiler.

### On typed storage

In-memory data models drift toward maps of maps: entities keyed by id, relations as
`Map[Id, Set[Id]]`, and the schema — which kinds of thing may relate to which — living only in
convention. The compiler, which knows the types of everything else, knows nothing about which
relationships are meant to exist, so a box assigned to a person, in a domain where boxes belong to
shelves, is a runtime surprise or a silent corruption.

Soundness declares the relations as a type. A database is created *of* its relation tuple, and
every assignment, removal and lookup is checked against it. Everything comes from the `soundness`
package:

```scala
import soundness.*
import strategies.throwUnsafely
```

A query built from typed fragments cannot be assembled into something the database will reject: [safety by construction](../philosophy/safety-by-construction.md) applied to SQL.

### Declaring a database

The relations form the database's type — `A -< B` reads "an `A` may have `B`s":

```scala
case class Cabinet(name: Text)
case class Shelf(name: Text)
case class Box(name: Text)

given db: Database of (Cabinet -< Shelf, Shelf -< Box) = Database()
```

### Storing and referencing

`store` interns a value and returns its typed reference — a `Ref of Box in db.type`, so a
reference cannot be confused with one from another database or to another type:

```scala
val shelf = Shelf(t"top").store()
val box = Box(t"alpha").store()
```

A reference dereferences by application, giving back the value:

```scala
box()   // Box(t"alpha")
```

### References by key

A reference need not come from a store at all. Where an entity carries its own key — an email
address, a product code — a `Reference` to it is that key, typed by what it refers to, and a
`Resolvable` instance says how a key finds its entity. Built from a set of values, it searches
them by the field marked `@primary`:

```scala
case class Person(@primary email: Text, name: Text)

val jack = Person(t"jack@example.com", t"Jack")
val jill = Person(t"jill@example.com", t"Jill")

given Person is Resolvable by Text = Resolvable(Set(jack, jill))
```

With that in scope, `ref` takes the reference of a value, which holds nothing but its key, and
applying a reference resolves it back. A reference built from a key that matches nothing raises
a `Reference.Error` when it is applied, not when it is built:

```scala
val jillRef = jill.ref   // Reference to Person
jillRef.key              // t"jill@example.com"
jillRef()                // jill

safely(Reference[Person](t"nobody@example.com")())   // Unset
```

### Relating

References relate with `assign`, unrelate with `unassign`, and traverse with `lookup` — each valid
only for a relation the database's type declares:

```scala
shelf.assign(box)
shelf.lookup[Box]        // Set(box)
shelf.unassign(box)

box.assign(shelf)        // does not compile: no Box -< Shelf relation
```

The last line is the point: the schema is not documentation but a type, and an operation outside it
never runs because it never compiles.

`ref` finds the reference for a value already stored, raising a `Database.Error` where it is not —
which is how a value arriving from elsewhere is matched to what the database already holds,
rather than stored a second time.

### Interning

`store` *interns*: storing an equal value twice yields the same reference, so identity in the
database is by value rather than by allocation. That is what makes a reference safe to compare, to
use as a key, and to hold in place of the value it names.

A reference is an opaque handle rather than a pointer or an index into a table the caller can see,
so a reference from one database cannot be dereferenced against another — the type says which
database it belongs to, and there is no way to construct one that lies.

### Querying

`lookup` follows one relation, and yields every reference of the type it is asked for, so a
query that must select rather than traverse is a predicate over those references' values:

```scala
shelf.assign(box)
shelf.lookup[Box].filter(_().name.starts(t"a"))   // Set(box)
```

Because the predicate is an ordinary function over the stored type, a query is written in Scala
rather than in a query language embedded in strings — and is therefore checked by the compiler
like the rest of the program.
