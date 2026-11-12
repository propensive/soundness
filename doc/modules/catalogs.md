## Catalogs

### About

A *catalog* holds one value for every field of a type. That means not only its immediate
fields, but every field which can be reached through them, and the whole value itself. Each
value is computed by one function, applied to the value of the field.

The entries are addressed by the *path* of the field, as `villain.name` and not `name`. A path
is written as a selection through a proxy of the type, so a path which does not exist is a
compile error. A catalog is built once and read many times, and two catalogs over the same
type combine entry by entry.

Catalogs sit between [optics](optics.md), which walk a path to update what lies at its end,
and [records](records.md), whose fields come from a schema outside the program. A catalog
walks no path, and reads no schema. It is the structure of a type, flattened into values,
and indexed by the paths which the type itself defines.

### On a value for every field

Some things naturally belong to each field: how wide a column should be, what label a field
is shown under, whether it is redacted in a log. A map from the name of a field to a value
is the usual answer, and it has two weaknesses. Two fields of the same name, at different
depths, such as the `name` of a `hero` and of a `villain`, collide, so the key must become a
path. And a path which is written as text is checked by nobody: a misspelled key is found
when a lookup comes back empty.

A catalog takes the type as the authority on which fields exist. Its entries are laid out
from the structure of the type at [compiletime]. A lookup names its entry through a proxy of
the same structure, so the compiler sees the path, and rejects one which the type does not
have.

Everything comes from the `soundness` package. The last import lets the samples print their
results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Building a catalog

`catalog` takes a value and a polymorphic function. It applies the function to the value,
and to every field within it, depth first. The function must accept a field of any type, so
what it can do with one is what any value supports. Here each entry is the field's textual
form:

```scala
case class Person(name: Text, age: Int)
case class Play(hero: Person, villain: Person)

val play = Play(Person(t"Othello", 40), Person(t"Iago", 28))

val labels: Catalog[Play, Text] = catalog(play)([field] => (field: field) => field.toString.tt)
```

```scala fuel
run
##
Out.println(labels.size.show)
```

There are seven entries: the whole `Play`, then `hero`, `hero.name`, `hero.age`, `villain`,
`villain.name` and `villain.age`. The first entry is for the whole value. After it come the
fields, in the order in which they are declared, each followed by its own fields.

A `Catalog[Play, Text]` is indexed by the structure of `Play`, and holds a `Text` at every
position. The type parameter for the key is what lets two catalogs over the same type be
combined, and what checks every path which is used to read one.
[Derivation](derivation.md#what-a-derivation-can-see) explains the polymorphic function.

### Addressing an entry

A catalog is read with a path, written as selections on a proxy of the key type. The proxy
has no fields of its own. Each selection is resolved at compiletime, into the index of the
entry which it names:

```scala
val villainName: Text = labels(proxy ?=> proxy.villain.name)
val wholeHero: Text = labels(proxy ?=> proxy.hero)
val wholePlay: Text = labels(proxy ?=> proxy)
```

The first is the label of the villain's name, the second is the entry for the whole `hero`
field, and the third is the entry for the whole `Play`.

A selection which the type does not have is reported where it is written:

```scala fuel
compile
errors show
##
labels(proxy ?=> proxy.villain.title)
```

The path is the position of the field, not its name. `hero.name` and `villain.name` are
therefore distinct entries, and there is no string anywhere to misspell. The proxy is
provided for the catalog's key type, and the accessor only has to navigate it.

### Transforming a catalog

`map` applies a function to every entry. It gives a catalog over the same type, with a new
type of value, and the same paths address the new entries:

```scala
val widths: Catalog[Play, Int] = labels.map(_.length)
```

```scala
val nameWidth: Int = widths(proxy ?=> proxy.villain.name)
val ageWidth: Int = widths(proxy ?=> proxy.hero.age)
```

The structure is fixed by the key type, so nothing about the paths changes under `map`. A
path which was valid on `labels` is valid on `widths`.

### Combining catalogs

`braid` joins two catalogs over the same key type, entry by entry, with a function of both
values. The compiler ensures that the catalogs share a key, so their entries align, position
for position, without any matching of names:

```scala
val cells: Catalog[Play, Text] = labels.braid(widths)((label, width) => t"$label ($width)")
```

```scala
val cell: Text = cells(proxy ?=> proxy.villain.name)
```

This is how a table of settings for each field is assembled: a catalog of labels, a catalog
of widths, and a catalog of whichever other property matters, built separately and braided
together, each addressed by the same paths. [Tables](tables.md) lays out the result.

### The same paths in derivation

The proxy which addresses a catalog also names a path where no catalog is involved. A
`Specific` maps paths of a type to [typeclass] instances. A derived instance consults it as
it visits each field, so the behavior of one field can be overridden by its path, not by
its type:

```scala
import formatting.compactJsonFormatting

val shout: Text is Json.Encodable = Json.Encodable(() => Morphology.Str): text => Json(text.upper)
```

```scala fuel
run
##
locally:
  given (Play is Specific over Json.Encodable) =
    specifically:
      case root.villain.name() => shout

  Out.println(play.in[Json].show)
```

Each case names a path through `root`. Its value must be an instance for the type at that
path, which is `Text` here, or the definition does not compile, and an unknown path is
rejected likewise. [Optics](optics.md) introduces `Specific` beside the lenses which use the
same syntax for paths, and [JSON](json.md#overriding-one-fields-encoder) shows it overriding
the encoder of one field.

### Related

- [Internationalization](internationalization.md) keeps its translations in a catalog.
