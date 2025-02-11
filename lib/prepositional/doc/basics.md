### Supplementary types

Prepositional defines the aliases, `by`, `from`, `in`, `into`, `of`, `on`, `onto`, `over` and `under`,
to be used in _type supplements_.

A type supplement is a transformation of a type `T`, in the form `T op S`, which specifies a
particular type member on `T`, according to the rules of the infix type alias, `op`.

For example, we could define the type `Expressible` as a typeclass interface for specifying how
things of a particular format should be expressed:
```amok
syntax scala
##
trait Expressible:
  type Self
  type Format

  def express(value: Self): Format
```

`Expressible` has two type members, `Self` and `Format`, which respectively
specify the type of the value to be expressed and the type in which it should be expressed. Here's
an example instance of `Expressible`:
```amok
syntax scala
##
given Expressible = new Expressible:
  type Self = Int
  type Format = Text

  def express(value: Int): Text = value.toString.tt
```

Using Scala 3's new modularity syntax,
```amok
syntax scala
##
import language.experimental.modularity
```
it is possible to write that as,
```amok
syntax scala
##
given Int is Expressible:
  type Format = Text

  def express(value: Int): Text = value.toString.tt
```
using the `is` type alias that "injects" the `Self` type member of `Expressible` with the type
`Int`. So the type
`Int is Expressible` is an alias of `Expressible { type Self = Int }`.

We can achieve similar with the type `Expressible in Text`, which dealiases to,
`Expressible { type Format = Text }`. The supplement `in Text` again "injects" the `Format` type
member into `Expressible` as the type `Text`.

Note that the type member name, `Format`, is specific to the `in` type alias.

We can compose a type using both `is` and `in`. The type `Int is Expressible in Text` is
equal to the type `Expressible { type Self = Int; type Format = Text }`.

### Type Members

The eight infix types defined in Prepositional, all of which are English prepositions, each adds
a different type member to an existing
type. These type member names relate semantically to the preposition, but not linguistically.

They are:
- `by` adds the `Operand` type member
- `from` adds the `Source` type member
- `in` adds the `Format` type member
- `into` adds the `Result` type member
- `of` adds the `Subject` type member
- `on` adds the `Platform` type member
- `onto` adds the `Target` type member
- `over` adds the `Carrier` type member
- `under` adds the `Constraint` type member

So, for example, a type such as `Fillable by Text on Linux into Data` would correspond to the type,
`Fillable { type Operand = Text; type Platform = Linux; type Result = Data }`.

It is important to be clear that Prepositional _does not_ specify how `Operand`, `Platform` and
`Result` should be interpreted. In the `Fillable` example, it is for the definition of `Fillable`
to specify that.

Prepositional does nothing more than to provide the means of specifying type members with the names
`Operand`, `Source`, `Format`, `Result`, `Subject`, `Platform`, `Target` and `Carrier`. And by
offering convenient syntax, it _suggests_ these type member names as general, convenient and
reusable. But it takes no responsibility for their meaning beyond their English language semantics.

### Prepositions

Prepositions are a rare commodity in English (or almost any language). They can be somewhat vague
in the meaning they convey, and are sometimes interchangeable. Consequently, a few prepositions go
a long way towards disambiguating the roles of different objects involved in an action.

So it is not necessary to define many infix operators corresponding to many prepositions. Types
rarely have _many_ type members, and the important thing is that their roles can be disambiguated.
And therefore, just a few definitions are sufficient to express a broad variety of relationships
between types.

However, it's possible that new general purpose infix type operators could be defined later.
