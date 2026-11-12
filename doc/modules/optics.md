## Optics

### About

Changing a value deep inside an [immutable] structure ordinarily means rebuilding every layer
above it by hand, with nested calls to `copy` which grow with the depth of the data. A
[lens](https://en.wikipedia.org/wiki/Bidirectional_transformation) does the rebuilding instead.

The `lens` method takes an update written as a plain assignment to a path, such as
`_.opening.name = t"Act One"`, and returns the new structure. Optics such as `Each` and
`Filter` carry a path through the elements of a collection, so one assignment can update many
values.

The same syntax updates the dynamic data formats: [JSON](json.md), [XML](xml.md),
[YAML](yaml.md) and [CBOR](cbor.md). A deep update looks the same whether the structure is a
case class or a parsed document.

### On immutable updates

An immutable value is safe to share, and awkward to change. Replacing one field three levels
down means copying three enclosing values, and naming every field on the way. Functional lenses
solve this, but classically with ceremony: a lens value declared for each field, composed with
operators, and applied with `get` and `set`. Many programs give up, and mutate.

Here the lenses are generated from the types, and hidden behind the syntax which the update
would have had anyway: a path and an assignment. This is what makes
[immutability](../philosophy/immutability.md) practical.

Everything comes from the `soundness` package. The second import chooses a
[strategy](errors.md#choosing-a-strategy) for errors, and the third lets the samples print their
results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Updating a field

The samples update one small structure:

```scala
case class Scene(place: Text, number: Int)
case class Act(name: Text, scenes: List[Scene])
case class Play(opening: Act, title: Text)

val play = Play(Act(t"Act I", List(Scene(t"A heath", 1), Scene(t"Forres", 2))), t"Macbeth")
```

`lens` applies an update to a value. The update is a path which ends in an assignment. The
result is a new `Play`, in which only the name of the opening act is different:

```scala fuel
run
##
Out.println(play.lens(_.opening.name = t"Act One").inspect)
```

The original is untouched:

```scala fuel
run
##
Out.println(play.opening.name)
```

Several updates are applied together. Updates which share a prefix rebuild the shared structure
only once:

```scala fuel
run
##
val renamed = play.lens
  ( _.opening.name = t"Act One",
    _.title = t"The Tragedy of Macbeth" )

Out.println(renamed.inspect)
```

A path is checked against the types as the code compiles. A field which does not exist is a
compile error:

```scala fuel
compile
errors show
##
play.lens(_.opening.epilogue = t"Exeunt")
```

### Reaching into collections

A path is a chain of steps, and a step in parentheses goes into a collection:

```tabular
column  Step
column  Selects
row  a field name, as in `_.opening`  that field
row  `(Prim)`, or any ordinal  one element
row  `(Each)`  every element
row  `(key)`  the value at a key, in a `Map`
row  `(Filter[T](predicate))`  the elements matching a predicate
```

An ordinal such as `Prim` selects one element:

```scala fuel
run
##
Out.println(play.lens(_.opening.scenes(Prim).place = t"A desert place").opening.scenes.inspect)
```

`Each` selects every element:

```scala fuel
run
##
Out.println(play.lens(_.opening.scenes(Each).number = 0).opening.scenes.inspect)
```

`Filter` selects the elements which satisfy a predicate:

```scala fuel
run
##
val updated = play.lens(_.opening.scenes(Filter[Scene](_.number > 1)).number = 0)
Out.println(updated.opening.scenes.inspect)
```

### Updating from the old value

Within an assignment, `prior` is the value being replaced. An update can therefore modify a
value, and need not replace it outright:

```scala fuel
run
##
Out.println(play.lens(_.opening.scenes(Each).place = t"$prior!").opening.scenes.inspect)
```

### Documents too

The dynamic formats define the same optics over their own structures. A parsed document is
updated with the same syntax:

```scala
import dynamicAccess.dynamicJson, conversions.encodableToJson
import optics.{jsonLens, jsonEachOptical}
import formatting.compactJsonFormatting

val json = play.in[Json]
```

```scala fuel
run
##
Out.println(json.lens(_.opening.name = t"Act One").show)
Out.println(json.lens(_.opening.scenes(Each).place = t"Dunsinane").show)
```

The `conversions` import lets a plain encodable value, such as a `Text`, an `Int` or a case
class, stand for the format's own value on the right of the `=`. Each format exports its own
conversion. Its optics are either imported from `optics`, or found in its companion with no
import:

```tabular
column  Format
column  Conversion
column  Optics
row  JSON  `conversions.encodableToJson`  `optics.jsonLens`, `jsonOrdinalOptical`, `jsonEachOptical`, `jsonFilterOptical`
row  TEL  `conversions.encodableToTel`  `optics.telLens`, `telOrdinalOptical`, `telEachOptical`
row  [XML](xml.md)  none: it lives on the `Xml` companion  `optics.xmlLens`, `xmlOrdinalOptical`, `xmlEachOptical`
row  YAML  `conversions.encodableToYaml`  in the `Yaml` companion
row  CBOR  `conversions.encodableToCbor`  in the `Cbor` companion
row  Protobuf  `conversions.encodableToProtobuf`  in the `Protobuf` companion, by field number
```

An `Xml` lens assignment therefore takes a plain encodable value with no conversion import at
all.

### Lenses as values

Underneath the syntax, a lens is an ordinary value: a getter and a setter. It can be summoned,
passed around and composed, for the cases where the access itself is the abstraction:

```scala
val nameLens = summon["name" is Lens from Act]
```

```scala fuel
run
##
Out.println(nameLens.modify(play.opening)(_.upper).inspect)
```

Two lenses `compose` into one, so a path can be assembled from parts which are held separately:
a lens onto a field, and a lens into what that field contains, joined into a lens which reaches
all the way through. Where the whole path is known in one place, the syntax above says it more
directly.

### Naming paths without traversing them

The path syntax which writes an update can also name a position without going there. A
`Specific` maps paths to values. This is how a derived [typeclass] is overridden at one field,
and not for a whole type:

```scala fuel
typecheck
##
case class Person(name: Text, age: Int)
case class Duel(challenger: Person, opponent: Person)

val shout: Text is Json.Encodable = Json.Encodable(() => Morphology.Str): text => Json(text.upper)

val duelSpecific: Duel is Specific over Json.Encodable =
  specifically:
    case root.opponent.name() => shout
    case root.challenger.name() => shout
```

The paths are checked against the structure of the type as the code compiles, so a misspelled
field is a compile error. Each value must also have the right type for the field it names. The
result is an ordinary map from each path to its value, which a [derivation] consults as it
visits each field:

```scala fuel
typecheck
##
duelSpecific.instances.keys
```

The per-field encoder override for [JSON](json.md) is built on this. The
mechanism is not specific to codecs: any typeclass whose derivation consults a `Specific` can be
overridden in the same way.

### Related

- [JSON](json.md), [XML](xml.md), [YAML](yaml.md), [CBOR](cbor.md), [TEL](tel.md) and
  [Protobuf](protobuf.md) describe the documents which the same lens syntax updates.
- [Catalogs](catalogs.md) uses the same field paths to look up values by position.
- [Collections](collections.md) describes ordinals such as `Prim`.
- [Derivation](derivation.md) explains how a typeclass instance is generated from a type's
  structure.
- [Immutability](../philosophy/immutability.md) explains why values are not changed in place.
- [Annotations](annotations.md) finds an annotated field and offers a lens onto it.
