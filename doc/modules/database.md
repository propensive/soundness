## Databases

### About

A `Database` is an in-memory store for typed entities and the relations between them. A value is
stored once, and identified by a typed reference. The relations which a database supports are
declared *in its type*, so relating two entities which the schema does not connect is a compile
error.

It has the shape of a relational database, with entities, foreign keys and joins, for the many
programs whose working data fits in memory and whose schema should be checked by the compiler.

### On typed storage

In-memory data models drift toward maps of maps: entities keyed by id, relations as
`Map[Id, Set[Id]]`, and the schema, which says which kinds of thing may relate to which, living
only in convention. The compiler knows the types of everything else, and knows nothing about
which relationships are meant to exist. So a box assigned to a person, in a domain where boxes
belong to shelves, is a surprise at [runtime], or a silent corruption.

Here the relations are a type. A database is created *of* its tuple of relations, and every
assignment, removal and lookup is checked against it. An operation which the schema does not
allow cannot be written, which is
[safety by construction](../philosophy/safety-by-construction.md) for a data model.

Everything comes from the `soundness` package. The second import chooses a
[strategy](errors.md#choosing-a-strategy) for errors, and the third lets the samples print their
results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Declaring a database

The relations form the type of the database. `A -< B` reads "an `A` may have `B`s":

```scala
case class Cabinet(name: Text)
case class Shelf(name: Text)
case class Box(name: Text)

given db: Database of (Cabinet -< Shelf, Shelf -< Box) = Database()
```

### Storing and referencing

`store` interns a value, and returns its typed reference. The reference is a
`Ref of Box in db.type`, so it cannot be confused with a reference from another database, or to
another type:

```scala
val shelf = Shelf(t"top").store()
val box = Box(t"alpha").store()
```

A reference is dereferenced by applying it, which gives back the value:

```scala fuel
run
##
Out.println(box().inspect)
```

### References by key

A reference need not come from a store at all. Where an entity carries its own key, such as an
email address or a product code, a `Reference` to it is that key, typed by what it refers to. A
`Resolvable` instance says how a key finds its entity. This one is built from a set of values,
and searches them by the field marked `@primary`:

```scala
case class Person(@primary email: Text, name: Text)

val jack = Person(t"jack@example.com", t"Jack")
val jill = Person(t"jill@example.com", t"Jill")

given personResolvable: (Person is Resolvable by Text) = Resolvable(Set(jack, jill))
```

With that in scope, `ref` takes the reference of a value, which holds nothing but its key.
Applying the reference resolves it back:

```scala fuel
run
##
val jillRef = jill.ref

Out.println(jillRef.key)
Out.println(jillRef().inspect)
```

A reference built from a key which matches nothing raises a `Reference.Error` when it is
applied, and not when it is built:

```scala fuel
run
errors show
##
Reference[Person](t"nobody@example.com")()
```

### Relating

References are related with `assign`, unrelated with `unassign`, and traversed with `lookup`.
Each is valid only for a relation which the type of the database declares:

```scala fuel
run
##
shelf.assign(box)
Out.println(shelf.lookup[Box].map(_()).inspect)
shelf.unassign(box)
Out.println(shelf.lookup[Box].map(_()).inspect)
```

There is no `Box -< Shelf` relation, so a box cannot be assigned a shelf:

```scala fuel
compile
errors show
##
box.assign(shelf)
```

That is the point. The schema is not documentation but a type, and an operation outside it never
runs, because it never compiles.

`ref` finds the reference for a value which is already stored, and raises a `Database.Error`
where it is not. That is how a value which arrives from elsewhere is matched to what the
database already holds, rather than being stored a second time.

### Interning

`store` *interns*. Storing an equal value twice gives the same reference, so identity in the
database is by value, and not by allocation:

```scala fuel
run
##
Out.println((Box(t"alpha").store() == box).inspect)
```

That is what makes a reference safe to compare, to use as a key, and to hold in place of the
value it names.

A reference is an opaque handle, not a pointer or an index into a table which the caller can see.
A reference from one database cannot be dereferenced against another. The type says which
database it belongs to, and there is no way to construct one which lies.

### Querying

`lookup` follows one relation, and gives every reference of the type it is asked for. A query
which must select rather than traverse is a predicate over the values of those references:

```scala fuel
run
##
shelf.assign(box)
shelf.assign(Box(t"beta").store())

Out.println(shelf.lookup[Box].filter(_().name.starts(t"a")).map(_()).inspect)
```

The predicate is an ordinary function over the stored type, so a query is written in Scala, and
not in a query language embedded in strings. The compiler checks it like the rest of the
program.

### Related

- [Annotations](annotations.md#finding-the-annotated-field) describes `@primary`, which marks
  the key a `Resolvable` searches by.
- [Collections](collections.md) describes the `Set` operations which a lookup supports.
- [Errors](errors.md) describes `safely` and the strategies which meet a `Reference.Error`.
- [Records](records.md) types data whose schema comes from a real database.
- [HTTP server](http-server.md) serves the data which a database holds.
