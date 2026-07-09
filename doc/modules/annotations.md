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

The `Annotated` typeclass hides the machinery. A request describes what is wanted — which
annotation type, on which field, of which type — and the compiler supplies the answer as a value.
Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Reading annotations

Given annotated definitions:

```scala
final case class ident() extends StaticAnnotation

case class Employee(person: Person, @ident code: Long)
```

the annotations of a field are summoned by naming the type and the field, and the query narrows by
annotation type with `by`:

```scala
summon[Employee is Annotated on "code"]()          // Set(ident())
summon[Employee is Annotated by ident on "code"]() // just the ident annotations
```

Annotations on the type itself, rather than a field, come from the plain form,
`summon[Company is Annotated]()`.

### Finding the annotated field

Often the question runs the other way: *which* field carries the annotation? When exactly one
does, the summoned instance knows it:

```scala
summon[Employee is Annotated by ident].field   // t"code"
```

This is how a serialization library finds "the field marked as the identifier" without being told
its name.

### Variants of a sealed type

The cases of an enumeration or sealed trait are queried with `under`, mapping each variant's name
to its annotations:

```scala
summon[Annotated under Colored].subtypes
// Map(t"Rgb" -> Set(unique()), t"Hsv" -> Set(number(3)))
```

### The `@name` annotation

The `@name` annotation, defined here and honoured by every Soundness serialization format, renames
a field or a variant on the wire. Bare, it applies to all formats; with a type argument, to one:

```scala
case class Record(@name[Json](t"first_name") firstName: Text, @name(t"yob") year: Int)
```

A format's derivation reads these through the same typeclass — the per-format rename overriding the
bare default — which is why one annotation serves [JSON](json.md), [XML](xml.md),
[YAML](yaml.md) and the rest identically.
