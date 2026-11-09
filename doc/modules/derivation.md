## Derivation

### About

The derived codecs which appear throughout these libraries, such as [JSON](json.md) encoders
from case classes, [CSV](csv.md) rows from fields and digests from structures, are all produced
by one derivation engine. It generates a [typeclass] instance for any *product*, which is a case
class, whose instance combines those of its fields, or any *coproduct*, which is an
[enumeration] or sealed trait, whose instance dispatches on the variant. A typeclass author
writes the two rules of combination once, and every user-defined type gains an instance for
free.

To the user of a typeclass, [derivation] is invisible. Defining a case class is enough. To the
author of a typeclass, it is two methods: one which says how fields combine, and one which says
how variants are told apart.

### On derivation

The instances of most typeclasses over most data types are mechanical. A `Show` for a case class
shows each field. An `Eq` compares each field. A JSON encoder encodes each field under its name.
Writing them by hand is boilerplate which drifts. Writing a macro for each typeclass needs
expertise which most libraries do not have. What is wanted is a statement of the two general
rules, that products combine and coproducts choose, from which every concrete instance follows.

The engine here, the successor to Magnolia, provides exactly that, as ordinary inheritance and
not as macro-writing. Derivation is what makes [small APIs](../philosophy/small-apis.md)
possible: a typeclass needs two rules, not an instance for each type.

Everything comes from the `soundness` package. The second import brings in the `label` of a
field, whose name the umbrella shares with a browser element's. The third permits indexing the
arrays of fields which a derivation produces, and the fourth chooses a [strategy] for errors:

```scala
import soundness.*
import wisteria.label
import dysasymptotics.linearAccess
import strategies.throwUnsafely
```

The samples on this page are checked by a compiled test in the library, `wisteria_doccheck.scala`.
They are not run here.

### Products, sums and typeclasses

Two words carry most of the weight, *product* and *sum*:

```tabular
column  Kind
column  A type which is
column  For example
column  Made of
row  product  a fixed sequence of values of other types  a case class, an enumeration case, a tuple, a singleton  *fields*
row  sum  a single choice from a fixed set of disjoint types  an enumeration, a sealed trait  *variants*
```

The fields of a product have fixed types, a canonical order and labels. Products and sums are
category-theoretic duals, and so, correspondingly, are fields and variants.

```scala
sealed trait Temporal

enum Month:
  case Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec

case class Date(day: Int, month: Month, year: Int) extends Temporal
case class Time(hour: Int, minute: Int)
case class DateTime(date: Date, time: Time) extends Temporal
```

Here `Temporal` is a sum whose variants are `Date` and `DateTime`. `Date`, `Time` and `DateTime`
are products, and `day`, `month` and `year` are the fields of `Date`. `Month` is a sum whose
twelve variants are themselves products: singletons, which are products with no fields.

Typeclasses divide along a related line. Where the type parameter appears in the *return* type
of the abstract method, the typeclass is a *producer*. It makes new instances of that type, and
may be covariant. Where the parameter appears in the method's *parameters*, the typeclass is a
*consumer*. Existing instances are handed to it, and it may be contravariant. Nothing is used
up. The word only says which way the value travels:

```scala
trait Presenting[value]:
  def present(value: value): Text

trait Producing[+value]:
  def produce(): value
```

The distinction matters because the two derive differently. A consumer folds over the fields of
a value it was given. A producer has no value to fold over, and must build one.

### Deriving instances of a typeclass

A typeclass gains derivation by extending `Derivation`, or `ProductDerivation` where only case
classes make sense, and implementing `conjunction` for products and `disjunction` for
coproducts. Here is a Show-like typeclass in full, with the extension method which makes it
convenient to call:

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
      . join(t"${typeName[derivation]}(", t", ", t")")

  inline def disjunction[derivation: SumReflection]: Presentation[derivation] =
    value =>
      variant(value):
        [variant <: derivation] => variant => contextual.present(variant)

extension [value](value: value)
  def present(using presentation: Presentation[value]): Text = presentation.present(value)
```

`fields` folds over the fields of a product, and gives each one its `label`, its position, and
the `contextual` instance of the typeclass for its type. `variant` dispatches on the actual case
of a coproduct. That is the whole cost of derivation for a typeclass.

`fields` and `variant` are two of a small set of primitives, which the sections below introduce
in turn:

```tabular
column  Primitive
column  Used in
column  Does
row  `fields(value)`  `conjunction`  folds over a product's fields, with their values
row  `contexts[derivation]()`  `conjunction`  folds over a product's fields, with no value in hand
row  `build[derivation]`  `conjunction`  constructs a product from a value for each field
row  `construct`  `conjunction`  constructs a product inside a type constructor, given its `pure` and `bind`
row  `dereference`  a `contexts` lambda  extracts the current field from a product
row  `variant(value)`  `disjunction`  dispatches on the variant of a value
row  `delegate[derivation](label)`  `disjunction`  dispatches to the variant a label names
row  `complement(other)`  a `variant` lambda  narrows another value to the same variant, as an `Optional`
row  `choice[derivation]`  `disjunction`  says, as the code compiles, whether every variant is a singleton
row  `choices[derivation]`  `disjunction`  folds over every variant, with no value in hand
row  `singleton[derivation](label)`  `disjunction`  yields the singleton a label names
row  `variantLabels[derivation]`  `disjunction`  lists the labels of the variants
```

The `[field] => field => …` syntax is a *polymorphic lambda*, and it is worth a moment, because it
is the one unfamiliar piece. It stands to a polymorphic method exactly as an ordinary lambda
stands to an ordinary one. Where the lambda for `def transform(field: Field): Text` is
`Field => Text`, the lambda for `def transform[field](field: field): Text` is
`[field] => field => Text`.

It is needed because each field has a different type, and the body knows nothing at all about
those types, except the one thing which matters: that two occurrences of `field` name the *same*
type. That is enough, because it means the `contextual` instance supplied beside the field is a
`Presentation[field]` for precisely this field's type, and can therefore be applied to it. The
body of a derivation has no other way to do anything with a value whose type it cannot see.
That is also why parametricity makes these bodies nearly write themselves.

`contexts` is the same fold without a value to fold over, and gives the typeclass instance of
each field alone. `typeName`, `tuple` and `singleton` describe the product being derived for,
so a derivation can drop the type name for a tuple, or the parentheses for a singleton.

### Using a derived instance

With the object above in scope, any case class or enumeration has a `Presentation`. It is
resolved automatically, or requested explicitly with a `derives` clause:

```scala fuel
parse
##
case class Person(name: Text, age: Int) derives Presentation

val presented: Text = Person(t"Ada", 36).present
```

Here `presented` is `t"Person(name=Ada, age=36)"`. Nested structures derive recursively. A case
class of case classes needs nothing more.

### What a derivation can see

Inside `conjunction`, each field carries more than its value, and the type being derived for can
be described too:

```tabular
column  Value
column  Available in
column  Gives
row  `label`  the lambda of any primitive that visits fields or variants  the field's or variant's name as written
row  `index`  the lambda of any primitive that visits fields or variants  its position
row  `contextual`  the lambda of any primitive that visits fields or variants  the instance of the typeclass for its type
row  `default`  the lambdas of `fields`, `contexts`, `build` and `construct`  the field's declared default, or `Unset`
row  `typeName[derivation]`  a derivation  the name of the type being derived for
row  `tuple[derivation]`  a derivation  whether that type is a tuple
row  `singleton[derivation]`  a derivation  whether that type is a singleton
```

A derivation can therefore produce something which mentions the structure, and not only
something which folds over it. Here it produces the labels of a product's fields, with no value
in hand, through `contexts`. Since derivation composes the instances of a product's fields,
every field type needs one, even where it is trivial, as it is for the labels of a `Text` or an
`Int`, which have none:

```scala fuel
parse
##
trait Labels[value]:
  def labels: List[Text]

object Labels extends ProductDerivation[Labels]:
  given Labels[Text] = new Labels[Text] { def labels: List[Text] = Nil }
  given Labels[Int] = new Labels[Int] { def labels: List[Text] = Nil }

  inline def conjunction[derivation <: Product: ProductReflection]: Labels[derivation] =
    val fieldLabels = contexts[derivation]() { [field] => context => label }
    new Labels[derivation]:
      def labels: List[Text] = fieldLabels.to[List]

case class Empty()

Labels.derived[Person].labels
Labels.derived[Empty].labels
```

`Labels.derived[Person].labels` is `List(t"name", t"age")`, and `Labels.derived[Empty].labels`
is the empty list. An empty product is a product with no fields, and a single-field product is
not a special case. Both derive from the same rule, which is where hand-written instances
usually go wrong.

Inside `disjunction`, `variant` narrows the value to its actual case, so the body is typed at
that case rather than at the sum, and the case's own label is available for a discriminator.

### Producing a value rather than consuming one

A producer has no instance to fold over, so instead of `fields` it uses `build`, which constructs
a new instance of the product. Its lambda receives the typeclass instance for each field, which
is the only thing capable of making a value of a type the body cannot see, and returns the value
of that field. Here is a parser of comma-separated fields, in full:

```scala fuel
parse
##
trait Parsing[value]:
  def parse(text: Text): value

object Parsing extends ProductDerivation[Parsing]:
  given Parsing[Text] = identity(_)
  given Parsing[Int] = _.s.toInt

  inline def conjunction[derivation <: Product: ProductReflection]: Parsing[derivation] =
    text =>
      val columns = text.cut(t",")
      build[derivation]:
        [field] => parsing => parsing.parse(columns(Ordinal.zerary(index)).or(t""))

Parsing.derived[Person].parse(t"Ada,36")
```

`Parsing.derived[Person].parse(t"Ada,36")` is `Person(t"Ada", 36)`. `Parsing` is a typeclass of
its own, and not the `Readable` which `read` looks for, so `t"Ada,36".read[Person]` does not
compile. Its instances are reached through `derived`.

The counterpart for sums is `delegate`, which takes the label of the variant to build, and
dispatches to the instance of that variant. `variant` can read the answer off a value it was
given. `delegate` is told which variant to produce, so the label usually comes out of the input,
here a `Date:` or `DateTime:` prefix. A label which names no variant raises a `Variant.Error`:

```scala fuel
parse
##
object ParsingSums extends Derivation[Parsing]:
  inline def conjunction[derivation <: Product: ProductReflection]: Parsing[derivation] =
    Parsing.conjunction[derivation]

  inline def disjunction[derivation: SumReflection]: Parsing[derivation] =
    text =>
      text.cut(t":") match
        case List(prefix: Text, rest: Text) =>
          delegate[derivation](prefix):
            [variant <: derivation] => parsing => parsing.parse(rest)
```

Where the produced value is wrapped in a type constructor, as a parser which returns an
`Optional` or a decoder which returns an `Attempt` is, `construct` takes the polymorphic `pure`
and `bind` operations for that constructor, and threads the fields through it.

### Typeclasses over two values

Some typeclasses take two values of the same type. An equivalence is the obvious one. Folding
over the fields of the left value is easy. The corresponding field of the right value is the
problem. Doing it by hand would mean building parallel arrays of left fields, right fields and
instances, and the moment those values are separated from the lambda which typed them, their
types erase, and only casts put them back together.

`dereference` and `complement` avoid that. Inside `contexts`, `dereference` is a function from
the product to the current field, typed with that field's type, so the same field can be pulled
out of *both* values and handed to the one `contextual` instance. For sums, `complement` does
the corresponding job inside `variant`. It retrieves the other value, narrowed to the same case.
It returns an [`Optional`](optional-values.md), since the two values need not be the same
variant, and if they are not, there is no meaningfully typed value to return:

```scala fuel
parse
##
trait Equivalence[value]:
  def equal(left: value, right: value): Boolean

object Equivalence extends Derivation[Equivalence]:
  given Equivalence[Int] = _ == _
  given Equivalence[Text] = _.lower == _.lower

  inline def conjunction[derivation <: Product: ProductReflection]: Equivalence[derivation] =
    (left, right) =>
      contexts[derivation]():
        [field] => equivalence =>
          val extract: derivation => field = dereference
          equivalence.equal(extract(left), extract(right))
      . all { boolean => boolean }

  inline def disjunction[derivation: SumReflection]: Equivalence[derivation] =
    (left, right) =>
      variant(left):
        [variant <: derivation] => leftValue =>
          complement(right).lay(false)(contextual.equal(leftValue, _))

Equivalence.derived[Person].equal(Person(t"Ada", 36), Person(t"ADA", 36))
```

`Equivalence.derived[Person].equal(Person(t"Ada", 36), Person(t"ADA", 36))` is `true`, since
the instance for `Text` ignores case. Different variants compare unequal. The same variant
compares through the instance which both share.

### Enumerations of singletons

An enumeration whose cases are all singletons is a straight choice between names, with no data,
and it supports things which a general sum does not. `choice` reports, at [compiletime],
whether that is the case, so a derivation can restrict itself to such enumerations, and reject
the rest with a message of its own:

```scala
trait Naming[value]:
  def name(value: value): Text

object Naming extends Derivation[Naming]:
  inline def conjunction[derivation <: Product: ProductReflection]: Naming[derivation] =
    value => typeName[derivation]

  inline def disjunction[derivation: SumReflection]: Naming[derivation] = value =>
    inline if choice[derivation] then
      variant(value):
        [variant <: derivation] => arm => t"${typeName[derivation]}.${contextual.name(arm)}"
    else scala.compiletime.error("cannot derive Naming for a sum whose variants carry data")

Naming.derived[Month].name(Month.Mar)
```

`Naming.derived[Month].name(Month.Mar)` is `t"Month.Mar"`. The `inline if` matters. It forces
`choice` to be evaluated as the code compiles, so the `scala.compiletime.error` branch is either
eliminated or reached, and reaching it fails the build.

For such an enumeration, `singleton` turns a label back into the value it names,
`variantLabels` lists the labels, and `choices` folds over every variant without needing a value
to dispatch on. That is what a derivation which produces a *schema*, rather than an instance,
needs.

### Default field values

A case class may give its fields default values, and a decoder usually wants them. A field
absent from the input should take the default which the author wrote, and not fail. Within the
lambdas of `fields`, `contexts` and `build`, a contextual `Default[Optional[field]]` is
available, and calling `default` gives an `Optional[field]`: the declared default, or `Unset`
where the field has none. Here is a parser which tolerates missing trailing columns:

```scala fuel
parse
frame
  label Every column must be present
  code
      object StrictParsing extends ProductDerivation[Parsing]:
        given Parsing[Text] = identity(_)
        given Parsing[Int] = _.s.toInt

        inline def conjunction[derivation <: Product: ProductReflection]: Parsing[derivation] =
          text =>
            val columns = text.cut(t",")
            build[derivation]:
              [field] => parsing => parsing.parse(columns(Ordinal.zerary(index)).or(t""))
frame
  label A missing column takes the field's default
  code
      object LenientParsing extends ProductDerivation[Parsing]:
        given Parsing[Text] = identity(_)
        given Parsing[Int] = _.s.toInt

        inline def conjunction[derivation <: Product: ProductReflection]: Parsing[derivation] =
          text =>
            val columns = text.cut(t",")
            build[derivation]:
              [field] => parsing =>
                columns(Ordinal.zerary(index)).let(parsing.parse(_))
                . or(default.or(panic(m"no column and no default for $label")))

      case class Settings(name: Text, retries: Int = 3)

      LenientParsing.derived[Settings].parse(t"primary")
##
```

`LenientParsing.derived[Settings].parse(t"primary")` is `Settings(t"primary", 3)`.

### Deriving beyond codecs

Nothing about the engine is specific to encoding. Any typeclass whose instances compose
structurally derives the same way, including the arithmetic ones, which combine two values of a
product field by field:

```scala
import arithmetic.addable

case class Pair(label: Text, count: Int)

val sum: Pair = Pair(t"foo", 10) + Pair(t"bar", 15)
```

Here `sum` is `Pair(t"foobar", 25)`. Addition on `Pair` is addition on each field, so `Text`
concatenates while `Int` sums, and no instance is written for `Pair` at all. `Subtractable`,
`Multiplicable` and their siblings follow the same rule.

### When derivation fails

A typeclass may sensibly derive for products but not for coproducts, or the reverse. Extending
`ProductDerivation` rather than `Derivation` says so, and applying it to a sum type is then a
compile error which names the mismatch, and not a surprise at [runtime]. A type with no `Mirror`
at all, such as an ordinary trait or a class which is not a case class, likewise fails at the
point of derivation, where the message can say what is missing.

Three failures are common enough to name:

```tabular
column  Symptom
column  Usual cause
column  Fix
row  a long message ending "given instance derived in trait Derivation does not match type…"  a polymorphic lambda's type variable has no upper bound  write `[variant <: derivation]`, not `[variant]`
row  a type mismatch against `Product`  `conjunction`'s type parameter lacks its bound  add `<: Product`
row  no instance found for a type whose fields look fine  one of *those* fields lacks an instance  compile an explicit call to `derived`, which says which
```

A recursive type is a special case. It cannot be derived in place, because the derivation would
have to summon itself part of the way through. Defining the instance on the type's companion,
with a `derives` clause, breaks the cycle:

```scala fuel
parse
##
enum Tree derives Presentation:
  case Leaf
  case Branch(left: Tree, value: Int, right: Tree)

val tree: Text = Tree.Branch(Tree.Leaf, 1, Tree.Leaf).present
```

Here `tree` is `t"Branch(left=Leaf, value=1, right=Leaf)"`.

### Choosing between candidate instances

A derived instance comes from a `given` inherited from `Derivation`, so it sits at lower
priority than any `given` written in the body of the companion. That is usually what is wanted,
and occasionally not. Where a derived instance and a hand-written one are truly ambiguous,
adjusting priorities tends to resolve one ambiguity by creating another elsewhere.

The reliable fix is to make the choice explicit, and not to rank it. Turn the competing `given`s
into ordinary methods, and write a single `derived` which selects among them in order, with
`scala.compiletime.summonFrom`:

```scala
trait Rendering[value]:
  def render(value: value): Text

object Rendering:
  inline given derived[Value]: Rendering[Value] = item =>
    scala.compiletime.summonFrom:
      case presentation: Presentation[Value] => presentation.present(item)
      case given (Value is Showable)         => item.show
      case _                                 => item.toString.tt

extension [value](value: value)
  def render(using rendering: Rendering[value]): Text = rendering.render(value)
```

```scala fuel
parse
##
val person: Text = Person(t"Ada", 36).render
val number: Text = 3.14.render
```

The first goes through `Presentation`, which `Person` derives. The second goes through
`Showable`, which `Double` has. Read plainly: use a `Presentation` if one exists; otherwise a
`Showable`, brought into scope for the right-hand side; otherwise fall back. The order in the
source *is* the priority, which is why this is easier to reason about than a lattice of implicit
scopes. The type parameter is capitalized here for a reason. In a `summonFrom` pattern, a
lowercase type name would be read as a fresh pattern variable, and not as the given's own
parameter.

The same technique gives derivation without exposing it. Define `conjunction` and `disjunction`
on an ordinary object rather than the companion, and call `derived` on it explicitly wherever an
instance is wanted, as `ParsingSums` and `LenientParsing` did above. Nothing then reaches
implicit search unless it is asked for.

### Per-field instances

Occasionally one field of one type needs a different instance from the default: a special codec
for one column, or an override for one path. `specifically` builds such a map of overrides by
naming the paths, which are checked against the actual structure of the type:

```scala fuel
parse
##
case class Org(cto: Person, ceo: Person)

val shout: Text is Json.Encodable = Json.Encodable(() => Morphology.Str): text => Json(text.upper)

given orgSpecific: (Org is Specific over Json.Encodable) =
  specifically:
    case root.ceo.name() => shout

import formatting.compactJsonFormatting

val encoded: Text = Org(Person(t"Ada", 36), Person(t"Bob", 40)).in[Json].show
```

Here `encoded` is `{"cto":{"name":"Ada","age":36},"ceo":{"name":"BOB","age":40}}`. A misspelled
path, or an instance of the wrong type for its field, is a compile error. A derivation which
supports overrides consults the map at each field it visits, as [JSON](json.md) and
[catalogs](catalogs.md) do.

### Related

- [JSON](json.md), [CSV](csv.md), [XML](xml.md) and the other formats derive their codecs this
  way.
- [Annotations](annotations.md) describes the `@name` annotation which a derivation reads to
  rename a field.
- [Catalogs](catalogs.md) and [optics](optics.md) describe `Specific` and the field paths it is
  keyed by.
- [Metaprogramming](metaprogramming.md) describes the compiletime tools beneath the engine.
- [Generic data](generic-data.md) describes a derived codec over structures of any shape.
- [Small APIs](../philosophy/small-apis.md) explains why two rules are better than an instance
  for each type.
