## Derivation

### About

The derived codecs that appear throughout Soundness — [JSON](json.md) encoders from case classes,
[CSV](csv.md) rows from fields, digests from structures — are all produced by one derivation
engine. It generates a typeclass instance for any *product* (a case class, whose instance combines
its fields') or *coproduct* (an enumeration or sealed trait, whose instance dispatches on the
variant), so a typeclass author writes the two combination rules once and every user-defined type
gains an instance for free.

To the user of a typeclass, derivation is invisible: defining a case class is enough. To the author
of a typeclass, it is two methods — one saying how fields combine, one saying how variants are
told apart.

### On derivation

The instances of most typeclasses over most data types are mechanical. A `Show` for a case class
shows each field; an `Eq` compares each field; a JSON encoder encodes each field under its name.
Writing them by hand is boilerplate that drifts; writing a macro per typeclass is expertise most
libraries do not have. What is wanted is a statement of the two general rules — products combine,
coproducts choose — from which every concrete instance follows.

Soundness's engine — the successor to Magnolia — provides exactly that, as ordinary inheritance
rather than macro-writing. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Deriving instances of a typeclass

A typeclass gains derivation by extending `Derivation` (or `ProductDerivation` where only case
classes make sense) and implementing `conjunction` for products and `disjunction` for coproducts.
A Show-like typeclass, in full:

```scala
trait Presentation[value]:
  def present(value: value): Text

object Presentation extends Derivation[Presentation]:
  given Presentation[Text] = identity(_)
  given Presentation[Int] = _.toString.tt

  inline def conjunction[derivation <: Product: ProductReflection]: Presentation[derivation] =
    value =>
      fields(value):
        [field] => field => t"$label=${contextual.present(field)}"
      . mkString(s"${typeName[derivation]}(", ", ", ")").tt

  inline def disjunction[derivation: SumReflection]: Presentation[derivation] =
    value =>
      variant(value):
        [variant <: derivation] => variant => contextual.present(variant)
```

`fields` folds over a product's fields, giving each one its `label`, its position, and the
`contextual` instance of the typeclass for its type; `variant` dispatches on a coproduct's actual
case. That is the whole cost of derivation for a typeclass.

### Using a derived instance

With the object above in scope, any case class or enumeration has a `Presentation` — resolved
automatically, or requested explicitly with a `derives` clause:

```scala
case class Person(name: Text, age: Int) derives Presentation

Person(t"Ada", 36).present   // t"Person(name=Ada, age=36)"
```

Nested structures derive recursively: a case class of case classes needs nothing more.

### Per-field instances

Occasionally one field of one type needs a different instance from the default — a special codec
for one column, an override for one path. `specifically` builds such an override map by naming the
paths, checked against the type's actual structure:

```scala
val custom: Org is Specific over (Codec in Json) =
  specifically:
    case root.cto.name() => nameCodec
    case root.ceo.age()  => ageCodec
```

A misspelled path, or an instance of the wrong type for its field, is a compile error; a derivation
that supports overrides consults the map at each field it visits.
