## Catalogs

### About

A *catalog* holds one value for every field of a type — not only its immediate fields, but every
field reachable through them, and the whole value itself — each computed by one function applied
to the field's value. The entries are addressed by the field's *path*, `cto.name` rather than
`name`, written as a selection through a proxy of the type, so a path that does not exist is a
compile error. A catalog is built once and read many times, and two catalogs over the same type
combine entry by entry.

Catalogs sit between [optics](optics.md), which walk a path to update what lies at its end, and
[records](records.md), whose fields come from a schema outside the program. A catalog walks no
path and reads no schema: it is the structure of a type, flattened into values and indexed by the
paths the type itself defines.

### On a value for every field

Some things are naturally per-field: how wide a column should be, what label a field is shown
under, whether it is redacted in a log. A map from field name to value is the usual answer, and it
has two weaknesses. Two fields of the same name at different depths — the `name` of a `ceo` and of
a `cto` — collide, so the key must become a path, and a path written as text is checked by nobody:
a misspelled key is found when a lookup comes back empty.

A catalog takes the type as the authority on what fields exist. Its entries are laid out from the
type's structure as the code compiles, and a lookup names its entry through a proxy of the same
structure, so the compiler sees the path and rejects one the type does not have. Everything comes
from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Building a catalog

`catalog` takes a value and a polymorphic function, and applies the function to the value and to
every field within it, depth-first. The function must accept a field of any type, so what it can
do with one is what any value supports; here, each entry is the field's textual rendering:

```scala
case class Person(name: Text, age: Int)
case class Org(ceo: Person, cto: Person)

val org = Org(Person(t"Ann", 51), Person(t"Bob", 47))

val labels: Catalog[Org, Text] = catalog(org)([field] => (field: field) => field.toString.tt)
labels.size   // 7: the whole Org, ceo, ceo.name, ceo.age, cto, cto.name, cto.age
```

The first entry is for the whole value; after it come the fields in declaration order, each
followed by its own fields. A `Catalog[Org, Text]` is indexed by `Org`'s structure and holds a
`Text` at every position; the type parameter for the key is what makes two catalogs over the same
type combinable, and what checks every path used to read one.

### Addressing an entry

A catalog is read with a path, written as selections on a proxy of the key type. The proxy has no
fields of its own: each selection is resolved as the code compiles into the index of the entry it
names, and a selection the type does not have is reported there:

```scala
labels(proxy ?=> proxy.cto.name)    // t"Bob"
labels(proxy ?=> proxy.ceo)         // t"Person(Ann,51)": the entry for the whole ceo field
labels(proxy ?=> proxy)             // the entry for the whole Org
labels(proxy ?=> proxy.cto.title)   // does not compile: Person has no title field
```

The path is the field's position, not its name, so `ceo.name` and `cto.name` are distinct entries,
and there is no string anywhere to misspell. The proxy is provided for the catalog's key type; the
accessor only has to navigate it.

### Transforming a catalog

`map` applies a function to every entry, giving a catalog over the same type with a new value
type, and the same paths address the new entries:

```scala
val widths: Catalog[Org, Int] = labels.map(_.length)

widths(proxy ?=> proxy.cto.name)   // 3
widths(proxy ?=> proxy.ceo.age)    // 2
```

Since the structure is fixed by the key type, nothing about the paths changes under `map`: a
function that was valid on `labels` is valid on `widths`.

### Combining catalogs

`braid` joins two catalogs over the same key type, entry by entry, with a function of both values.
The compiler ensures the catalogs share a key, so their entries align position for position
without any matching of names:

```scala
val cells: Catalog[Org, Text] = labels.braid(widths)((label, width) => t"$label ($width)")

cells(proxy ?=> proxy.cto.name)   // t"Bob (3)"
```

This is how a table of per-field settings is assembled: a catalog of labels, a catalog of widths,
a catalog of whichever other property matters, built separately and braided together, each
addressed by the same paths.

### The same paths in derivation

The proxy that addresses a catalog also names a path where no catalog is involved. A `Specific`
maps paths of a type to typeclass instances, and a derived instance consults it as it visits each
field, so one field's behavior can be overridden by its path rather than by its type:

```scala
import formatting.compactJsonFormatting

val shout: Text is Json.Encodable = Json.Encodable(() => Morphology.Str): text => Json(text.upper)

given (Org is Specific over Json.Encodable) =
  specifically:
    case root.cto.name() => shout

org.in[Json].show   // t"""{"ceo":{"name":"Ann","age":51},"cto":{"name":"BOB","age":47}}"""
```

Each case names a path through `root`, and its value must be an instance for the type at that
path — `Text` here — or the definition does not compile; an unknown path is likewise rejected.
The [optics](optics.md) tutorial introduces `Specific` beside the lenses that use the same path
syntax, and the [JSON](json.md) tutorial shows it overriding one field's encoder.
