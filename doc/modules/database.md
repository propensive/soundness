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

`lookup` follows one relation. Where a query must select rather than traverse, a `Listable`
instance lists the references whose values satisfy a predicate:

```scala
summon[Box is Listable].list(_.name.starts(t"a"))
```

Because the predicate is an ordinary function over the stored type, a query is written in Scala
rather than in a query language embedded in strings — and is therefore checked by the compiler
like the rest of the program.
