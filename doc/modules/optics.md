## Optics

### About

Changing a value deep inside an [immutable] structure ordinarily means rebuilding every layer
above it by hand, with nested calls to `copy` which grow with the depth of the data. A
[lens](https://en.wikipedia.org/wiki/Bidirectional_transformation) does the rebuilding instead.

The `lens` method takes an update written as a plain assignment to a path, such as
`_.ceo.name = t"Bill"`, and returns the new structure. Optics such as `Each` and `Filter` carry
a path through the elements of a collection, so one assignment can update many values.

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
case class Role(name: Text, count: Int)
case class Person(name: Text, roles: List[Role])
case class Company(ceo: Person, name: Text)

val company = Company(Person(t"John", List(Role(t"CEO", 1), Role(t"CFO", 2))), t"Acme")
```

`lens` applies an update to a value. The update is a path which ends in an assignment. The
result is a new `Company`, in which only the name of the CEO is different:

```scala fuel
run
##
Out.println(company.lens(_.ceo.name = t"Bill").inspect)
```

The original is untouched:

```scala fuel
run
##
Out.println(company.ceo.name)
```

Several updates are applied together. Updates which share a prefix rebuild the shared structure
only once:

```scala fuel
run
##
val renamed = company.lens
  ( _.ceo.name = t"Bill",
    _.name = t"Acme Ltd" )

Out.println(renamed.inspect)
```

A path is checked against the types as the code compiles. A field which does not exist is a
compile error:

```scala fuel
compile
errors show
##
company.lens(_.ceo.title = t"Dr")
```

### Reaching into collections

A path is a chain of steps, and a step in parentheses goes into a collection:

```tabular
column  Step
column  Selects
row  a field name, as in `_.ceo`  that field
row  `(Prim)`, or any ordinal  one element
row  `(Each)`  every element
row  `(key)`  the value at a key, in a `Map`
row  `(Filter[T](predicate))`  the elements matching a predicate
```

An ordinal such as `Prim` selects one element:

```scala fuel
run
##
Out.println(company.lens(_.ceo.roles(Prim).name = t"Founder").ceo.roles.inspect)
```

`Each` selects every element:

```scala fuel
run
##
Out.println(company.lens(_.ceo.roles(Each).count = 0).ceo.roles.inspect)
```

`Filter` selects the elements which satisfy a predicate:

```scala fuel
run
##
val updated = company.lens(_.ceo.roles(Filter[Role](_.count > 1)).count = 0)
Out.println(updated.ceo.roles.inspect)
```

### Updating from the old value

Within an assignment, `prior` is the value being replaced. An update can therefore modify a
value, and need not replace it outright:

```scala fuel
run
##
Out.println(company.lens(_.ceo.roles(Each).name = t"$prior!").ceo.roles.inspect)
```

### Documents too

The dynamic formats define the same optics over their own structures. A parsed document is
updated with the same syntax:

```scala
import dynamicAccess.dynamicJson, conversions.encodableToJson
import optics.{jsonLens, jsonEachOptical}
import formatting.compactJsonFormatting

val json = company.in[Json]
```

```scala fuel
run
##
Out.println(json.lens(_.ceo.name = t"Bill").show)
Out.println(json.lens(_.ceo.roles(Each).name = t"member").show)
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
val nameLens = summon["name" is Lens from Person]
```

```scala fuel
run
##
Out.println(nameLens.modify(company.ceo)(_.upper).inspect)
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
case class Org(ceo: Person, cto: Person)

val shout: Text is Json.Encodable = Json.Encodable(() => Morphology.Str): text => Json(text.upper)

val orgSpecific: Org is Specific over Json.Encodable =
  specifically:
    case root.cto.name() => shout
    case root.ceo.name() => shout
```

The paths are checked against the structure of the type as the code compiles, so a misspelled
field is a compile error. Each value must also have the right type for the field it names. The
result is an ordinary map from each path to its value, which a [derivation] consults as it
visits each field:

```scala fuel
typecheck
##
orgSpecific.instances.keys
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
