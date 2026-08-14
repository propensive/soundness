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

### Products, sums and typeclasses

Two words carry most of the weight. A *product* is a type composed of a fixed sequence of values
of other types — a case class, an enumeration case, a tuple, a singleton — and those values are
its *fields*, which have fixed types, a canonical order and labels. A *sum* is a type
representing a single choice from a fixed set of disjoint types — an enumeration, a sealed trait
— and each of those types is a *variant*. Products and sums are category-theoretic duals, and so,
correspondingly, are fields and variants.

```scala
sealed trait Temporal

enum Month:
  case Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec

case class Date(day: Int, month: Month, year: Int) extends Temporal
case class Time(hour: Int, minute: Int)
case class DateTime(date: Date, time: Time) extends Temporal
```

Here `Temporal` is a sum whose variants are `Date` and `DateTime`; `Date`, `Time` and `DateTime`
are products, with `day`, `month` and `year` the fields of `Date`; `Month` is a sum whose twelve
variants are themselves products — singletons, products with no fields.

Typeclasses divide along a related line. Where the type parameter appears in the abstract
method's *return* type, the typeclass is a *producer*: it makes new instances of that type, and
may be covariant. Where it appears in the method's *parameters*, it is a *consumer*: existing
instances are handed to it, and it may be contravariant. (Nothing is used up; the word only says
which way the value travels.)

```scala
trait Show[value]:                     // consumer
  def show(value: value): Text

trait Default[+value]:                 // producer
  def apply(): value
```

The distinction matters because the two derive differently: a consumer folds over the fields of a
value it was given, while a producer has no value to fold over and must build one.

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

The `[field] => field => …` syntax is a *polymorphic lambda*, and it is worth a moment because it
is the one unfamiliar piece. It stands to a polymorphic method exactly as an ordinary lambda
stands to an ordinary one: where the lambda for `def transform(field: Field): Text` is `Field =>
Text`, the lambda for `def transform[field](field: field): Text` is `[field] => field => Text`.

It is needed because each field has a different type, and the body knows nothing whatever about
those types — except the one thing that matters: that two occurrences of `field` name the *same*
type. That is enough, because it means the `contextual` instance supplied alongside is a
`Presentation[field]` for precisely this field's type, and can therefore be applied to it. The
derivation body has no other way to do anything with a value whose type it cannot see, which is
also why parametricity makes these bodies nearly write themselves.

`contexts` is the same fold without a value to fold over, giving each field's typeclass instance
alone; and `typeName`, `tuple` and `singleton` describe the product being derived for, so a
derivation can drop the type name for a tuple, or the parentheses for a singleton.

### Using a derived instance

With the object above in scope, any case class or enumeration has a `Presentation` — resolved
automatically, or requested explicitly with a `derives` clause:

```scala
case class Person(name: Text, age: Int) derives Presentation

Person(t"Ada", 36).present   // t"Person(name=Ada, age=36)"
```

Nested structures derive recursively: a case class of case classes needs nothing more.

### What a derivation can see

Inside `conjunction`, each field carries more than its value. `label` is the field's name as
written, `index` its position, `typeName` the enclosing type's name, and `contextual` the instance
of the typeclass being derived for that field's type. A derivation can therefore produce something
that mentions the structure, not only something that folds over it:

```scala
Labels.derived[Person].labels    // List(t"name", t"age", t"male")
Labels.derived[Empty].labels     // Nil
```

An empty product is a product with no fields, and a single-field product is not special-cased —
both derive from the same rule, which is where hand-written instances usually go wrong.

Inside `disjunction`, `variant` narrows the value to its actual case, so the body is typed at that
case rather than at the sum, and the case's own label is available for a discriminator.

### Producing a value rather than consuming one

A producer has no instance to fold over, so instead of `fields` it uses `build`, which constructs
a new instance of the product. Its lambda receives the typeclass instance for each field — the
only thing capable of making a value of a type the body cannot see — and returns that field's
value:

```scala
inline def conjunction[derivation <: Product: ProductReflection]: Readable[derivation] = text =>
  build[derivation]:
    [field] => readable => readable.read(column(index))
```

The sum counterpart is `delegate`, which takes the label of the variant to build and dispatches to
that variant's instance. Unlike `variant`, which can read the answer off a value it was given,
`delegate` is told which variant to produce, so the label usually comes out of the input:

```scala
inline def disjunction[derivation: SumReflection]: Readable[derivation] = text =>
  text.cut(t":") match
    case List(prefix, rest) =>
      delegate[derivation](prefix):
        [variant <: derivation] => context => context.read(rest)
```

Where the produced value is wrapped in a type constructor — a parser returning an `Optional`, a
decoder returning an `Attempt` — `construct` takes the polymorphic `pure` and `bind` operations
for that constructor, and threads the fields through it.

### Typeclasses over two values

Some typeclasses take two values of the same type: `Eq` is the obvious one. Folding over the
fields of the left value is easy, but the right value's corresponding field is the problem. Doing
it by hand would mean building parallel arrays of left fields, right fields and instances, and the
moment those values are separated from the lambda that typed them, their types erase and only
casts put them back together.

`complement` avoids that. Inside the fold for one value, it retrieves the corresponding field of
*another* value of the same type, typed identically — so it is compatible with the same
`contextual` instance:

```scala
inline def conjunction[derivation <: Product: ProductReflection]: Eq[derivation] =
  (left, right) =>
    fields(left):
      [field] => leftField => contextual.equal(leftField, complement(right))
    . all { equal => equal }
```

For sums it works the same way but returns an `Optional`, since the two values need not be the
same variant — and if they are not, there is no meaningfully-typed value to return:

```scala
inline def disjunction[derivation: SumReflection]: Eq[derivation] =
  (left, right) =>
    variant(left):
      [variant <: derivation] => leftValue =>
        complement(right).lay(false)(contextual.equal(leftValue, _))
```

Different variants compare unequal; the same variant compares through the instance both share.

### Enumerations of singletons

An enumeration whose cases are all singletons — a straight choice between names, with no data —
supports things a general sum does not. `choice` reports, at compiletime, whether that is the
case, so a derivation can restrict itself to such enumerations and reject the rest with a message
of its own:

```scala
inline def disjunction[derivation: SumReflection]: Show[derivation] = value =>
  inline if choice[derivation] then
    variant(value):
      [variant <: derivation] => arm => t"${typeName[derivation]}.${contextual.show(arm)}"
  else compiletime.error("cannot derive Show for this ADT")
```

The `inline if` matters: it forces `choice` to be evaluated as the code compiles, so the
`compiletime.error` branch is either eliminated or reached, and reaching it fails the build.

For such an enumeration, `singleton` turns a label back into the value it names, `variantLabels`
lists the labels, and `choices` folds over every variant without needing a value to dispatch on —
which is what a derivation producing a *schema*, rather than an instance, needs.

### Default field values

A case class may give its fields default values, and a decoder usually wants them: a field absent
from the input should take the default the author wrote rather than fail. Within the lambdas of
`fields`, `contexts` and `build`, a contextual `Default[Optional[field]]` is available, and
calling `default` yields an `Optional[field]` — the declared default, or `Unset` where the field
has none:

```scala
[field] => readable =>
  if index < columns.length then readable.read(columns(index)) else default.or(abort(Missing()))
```

### Deriving beyond codecs

Nothing about the engine is specific to encoding. Any typeclass whose instances compose
structurally derives the same way — including the arithmetic ones, which combine two values of a
product field by field:

```scala
import arithmetic.addable

Pair(t"foo", 10) + Pair(t"bar", 15)   // Pair(t"foobar", 25)
```

Addition on `Pair` is addition on each field, so `Text` concatenates while `Int` sums, with no
instance written for `Pair` at all. `Subtractable`, `Multiplicable` and their siblings follow the
same rule.

### When derivation fails

A typeclass may sensibly derive for products but not for coproducts, or the reverse. Extending
`ProductDerivation` rather than `Derivation` says so, and applying it to a sum type is then a
compile error naming the mismatch rather than a runtime surprise. A type with no `Mirror` at all —
an ordinary trait, a class that is not a case class — likewise fails at the point of derivation,
where the message can say what is missing.

Three failures are common enough to name. A long message ending "given instance derived in trait
Derivation does not match type…" almost always means a polymorphic lambda's type variable is
missing its upper bound: it must be `[variant <: derivation]`, not `[variant]`. A type mismatch
against `Product` means `conjunction`'s type parameter is missing its `<: Product` constraint. And
an instance that is simply not found for a type whose fields look fine is usually one of *those*
fields lacking an instance — compiling an explicit call to `derived` at the call site says which.

A recursive type is a special case: it cannot be derived in place, because the derivation would
have to summon itself part-way through. Defining the instance on the type's companion, with a
`derives` clause, breaks the cycle:

```scala
enum Tree derives Presentation:
  case Leaf
  case Branch(left: Tree, value: Int, right: Tree)
```

### Choosing between candidate instances

A derived instance comes from a `given` inherited from `Derivation`, so it sits at lower priority
than any `given` written in the companion body — which is usually what is wanted, and occasionally
not. Where a derived instance and a hand-written one are genuinely ambiguous, adjusting priorities
tends to resolve one ambiguity by creating another elsewhere.

The reliable fix is to make the choice explicit rather than to rank it. Turn the competing
`given`s into ordinary methods and write a single `derived` that selects among them in order with
`summonFrom`:

```scala
object Debug:
  inline given derived[value]: Debug[value] = value =>
    compiletime.summonFrom:
      case encoder: Encoder[value] => encoder.encode(value)
      case given Show[value]       => value.show
      case _                       => value.toString.tt
```

Read plainly: use an `Encoder` if one exists; otherwise a `Show`, brought into scope for the
right-hand side; otherwise fall back. The order in the source *is* the priority, which is why this
is easier to reason about than a lattice of implicit scopes.

The same technique gives derivation without exposing it: define `conjunction` and `disjunction` on
an ordinary object rather than the companion, and call `derived` on it explicitly wherever an
instance is wanted. Nothing then reaches implicit search unless it is asked for.

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
