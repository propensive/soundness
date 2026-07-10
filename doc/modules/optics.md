## Optics

### About

Updating a value deep inside an immutable structure ordinarily means rebuilding every layer above
it by hand — nested `copy` calls that grow with the depth of the data. A
[lens](https://en.wikipedia.org/wiki/Bidirectional_transformation) does the rebuilding instead:
the `lens` method takes an update written as a plain path assignment, `_.ceo.name = t"Bill"`, and
returns the new structure. Optics such as `Each` and `Filter` extend a path through the elements
of a collection, so one assignment can update many values at once.

The same mechanism serves the dynamic data formats — [JSON](json.md), [XML](xml.md),
[YAML](yaml.md), [CBOR](cbor.md) — so a deep update looks identical whether the structure is a
case class or a parsed document.

### On immutable updates

Immutability makes values safe to share, and awkward to change: replacing one field three levels
down means copying three enclosing values, naming every intermediate field. Functional lenses
solve this, but classically at the price of ceremony — lens values declared per field, composed
with operators, applied with `get` and `set` — heavy enough that many programs give up and mutate.

Soundness generates the lenses from the types and hides them behind the syntax the update would
have had anyway: a path and an assignment. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Updating a field

`lens` applies one or more updates to a value, each written as a path ending in an assignment:

```scala
case class Role(name: Text, count: Int)
case class Person(name: Text, roles: List[Role])
case class Company(ceo: Person, name: Text)

val company = Company(Person(t"John", List(Role(t"CEO", 1), Role(t"CFO", 2))), t"Acme")

company.lens(_.ceo.name = t"Bill")
// the same Company, with only the CEO's name changed
```

Several updates apply together, and updates sharing a prefix rebuild the shared structure once:

```scala
company.lens
  ( _.ceo.roles = Nil,
    _.ceo.name = t"Bill" )
```

### Reaching into collections

A step in parentheses traverses a collection: an ordinal picks one element, `Each` touches every
element, a key indexes a `Map`, and `Filter` selects by a predicate:

```scala
company.lens(_.ceo.roles(Prim).name = t"Founder")               // the first role
company.lens(_.ceo.roles(Each).count = 0)                       // every role
company.lens(_.ceo.roles(Filter[Role](_.count > 1)).count = 0)  // only matching roles
```

### Updating from the old value

Within an assignment, `prior` is the value being replaced, so an update can be a modification
rather than a replacement:

```scala
company.lens(_.ceo.roles(Each).name = t"$prior!")
// every role's name, with an exclamation mark appended
```

### Documents too

The dynamic formats define the same optics over their own structures, so a parsed document updates
with the identical syntax — one skill, both worlds:

```scala
import dynamicJsonAccess.enabled, jsonConversion.encodable

json.lens(_.leader.age = 41)
json.lens(_.leader.roles(Each).name = t"member")
```

### Lenses as values

Underneath the syntax, a lens is an ordinary value — a getter and setter pair — that can be
summoned, passed around and composed, for the cases where the access itself is the abstraction:

```scala
val nameLens = summon["name" is Lens from Json onto Json]
nameLens.modify(document)(transform)
```
