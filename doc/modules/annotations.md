## Annotations

### About

Scala's [static annotations](https://docs.scala-lang.org/scala3/reference/other-new-features/annotations.html)
attach metadata to types and fields, but the language offers no ordinary way to read them back —
that normally means writing a macro. Soundness exposes them through a typeclass instead: summoning
`Annotated` for a type yields the annotations on it, on its fields, or on the variants of a sealed
hierarchy, resolved entirely at compiletime, with no reflection at runtime and no macro written by
the consumer.

This is the machinery behind the `@name` annotation that renames fields across Soundness's
serialization formats — itself defined here.

### On annotations

An annotation is exactly where certain metadata belongs — a wire name on the field it renames, a
marker on the case it distinguishes — because it sits beside the thing it describes and travels
with it. What has kept annotations underused in Scala is the reading side: runtime reflection is
slow and untyped, and compiletime access means quotes-and-splices expertise.

Reading annotations at compiletime, so that a misspelled name or a wrong type is a compile error, is [safety by construction](../philosophy/safety-by-construction.md).

The `Annotated` typeclass hides the machinery. A request describes what is wanted — which
annotation type, on which field, of which type — and the compiler supplies the answer as a value.
Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Reading annotations

An annotation is an ordinary case class extending `StaticAnnotation`. Given a few of them, and
some definitions that carry them:

```scala
final case class ident() extends StaticAnnotation
final case class primary() extends StaticAnnotation
final case class number(number: Int) extends StaticAnnotation

case class Person(name: Text, @ident email: Text)
case class Employee(person: Person, @ident @primary code: Long)

@number(10)
case class Company(name: Text)
```

the annotations of a field are summoned by naming the type and the field, and the query narrows by
annotation type with `by`:

```scala
summon[Employee is Annotated on "code"]()          // Set(ident(), primary())
summon[Employee is Annotated by ident on "code"]() // Set(ident())
```

Annotations on the type itself, rather than a field, come from the plain form, and `fields` gives
every annotated field at once:

```scala
summon[Company is Annotated]()                // Set(number(10))
summon[Employee is Annotated by ident].fields  // Map(t"code" -> Set(ident()))
```

A field with no annotation of the requested type is simply absent from the map. Because the
answer is computed as the code compiles, asking about a field the type does not have is a compile
error, not an empty result.

### Finding the annotated field

Often the question runs the other way: *which* field carries the annotation? When exactly one
does, the summoned instance knows it, and — since finding the field is usually the prelude to
reading or writing it — offers a [lens](optics.md) onto it:

```scala
summon[Employee is Annotated by primary].field   // t"code"

val person = Person(t"Jack", t"jack@example.com")
summon[Person is Annotated by ident].lens(person)                          // t"jack@example.com"
summon[Person is Annotated by ident].lens.update(person, t"jill@example.com")
```

This is how a serialization library finds "the field marked as the identifier" without being told
its name. If more than one field carries the annotation, `field` is unavailable, because the
instance's type records whether the match was unique.

### Variants of a sealed type

The cases of an enumeration or sealed trait are queried with `under`, mapping each variant's name
to its annotations:

```scala
sealed trait Colored

@number(3)
case class Hsv(hue: Double, saturation: Double, @ident value: Double) extends Colored

@primary()
case class Rgb(red: Int, green: Int, blue: Int) extends Colored

summon[Annotated under Colored].subtypes
// Map(t"Hsv" -> Set(number(3)), t"Rgb" -> Set(primary()))
```

### Parameterized annotations

An annotation may take a type argument, and a query can filter by it. A bare `@marker(1)` infers
`marker[Any]`, which a `marker[Any]` query sees and a `marker[Person]` query does not:

```scala
final case class marker[topic](id: Int) extends StaticAnnotation

case class Marked(@marker[Person](1) @marker[Company](2) one: Int, @marker[Company](3) two: Int)

summon[Marked is Annotated by marker[Person]].fields   // Map(t"one" -> Set(marker(1)))
summon[Marked is Annotated by marker[Company]].fields  // both fields
```

### The `@name` annotation

The `@name` annotation, defined here and honored by every Soundness serialization format, renames
a field or a variant on the wire. Bare, it applies to all formats; with a type argument, to one:

```scala
case class Record(@name[Json](t"first_name") firstName: Text, @name(t"yob") year: Int)
```

A format's derivation reads these through `relabelling`, which maps each renamed field to its
serialized name with the per-format rename overriding the bare default — which is why one
annotation serves [JSON](json.md), [XML](xml.md), [YAML](yaml.md) and the rest identically.
`variantRelabelling` does the same for the cases of a sum type:

```scala
relabelling[Record, Json]   // Map(t"firstName" -> t"first_name", t"year" -> t"yob")
relabelling[Record, Xml]    // Map(t"year" -> t"yob")
```

`fieldAnnotations[Record, name[Json]]` and `subtypeAnnotations` are the general forms behind
these, returning only the fields or variants that carry the annotation in question.

### Reaching fields by name

Alongside annotations, a case class's fields can be reached by name at runtime, with the mapping
generated as the code compiles rather than by reflection. A `Dereferenceable` gives the field
names, one field's value by name, and the whole record as a map. The instance is parameterized
by the type the fields yield, so a record whose fields are all `Int` dereferences to `Int` — and
the same instance can put a value back by name, which is why the yield type must be one every
field accepts:

```scala
case class Letters(alpha: Int, beta: Int, gamma: Int)
val letters = Letters(1, 2, 3)
val fields = summon[Letters is Dereferenceable to Int]

fields.names(letters)            // List(t"alpha", t"beta", t"gamma")
fields.select(letters, t"beta")  // 2
fields.members(letters)          // Map(t"alpha" -> 1, t"beta" -> 2, t"gamma" -> 3)
```

Where only the values of one type are wanted, `membersOfType` picks them out of any record,
which is what makes a generic renderer or a template engine able to read a value's fields without
casting:

```scala
letters.membersOfType[Int]   // List(1, 2, 3)
```

Each accessor is a [lens](optics.md), so a field found by name can be written as well as read;
`update` replaces the named field and `modify` applies a function to it, each yielding a new
record, or `Unset` where the name is not a constructor parameter:

```scala
val numbers = summon[Letters is Dereferenceable to Int]

numbers.update(letters, t"beta", 20)        // Letters(1, 20, 3)
numbers.modify(letters, t"gamma")(_*10)     // Letters(1, 2, 30)
numbers.lens(t"epsilon")                    // Unset: not a field
```

Because the mapping is a compiled table of accessors, reaching a field costs a method call, and a
name the type does not have is discovered where the table is built rather than by a reflective
lookup failing at runtime.
