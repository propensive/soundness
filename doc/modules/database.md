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
