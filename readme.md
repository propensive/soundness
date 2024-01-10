[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/wisteria/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/wisteria/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Wisteria

__Simple, fast and transparant generic derivation for typeclasses__

__Wisteria__ is a generic macro for automatic materialization of typeclasses for datatypes composed from product
types (e.g. case classes) and coproduct types (e.g. enums). It supports recursively-defined datatypes
out-of-the-box, and incurs no significant time-penalty during compilation.

## Features

 - derives typeclasses for case classes, case objects, sealed traits and enumerations
 - offers a lightweight but typesafe syntax for writing derivations avoiding complex macro code
 - builds upon Scala 3's built-in generic derivation
 - works with recursive and mutually-recursive definitions
 - supports parameterized ADTs (GADTs), including those in recursive types
 - supports both consumer and producer typeclass interfaces
 - fast at compiletime
 - generates performant runtime code, without unnecessary runtime allocations


## Availability Plan

Wisteria has not yet been published. The medium-term plan is to build Wisteria
with [Fury](/propensive/fury) and to publish it as a source build on
[Vent](/propensive/vent). This will enable ordinary users to write and build
software which depends on Wisteria.

Subsequently, Wisteria will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

Wisteria makes it easy to derive typeclass instances for product and sum types,
by defining the rules for composition and delegation as simply as possible.

This is called _generic derivation_, and given a typeclass which provides some
functionality on a type, it makes it possible to automatically extend that
typeclass's functionality to all product types, so long as it is available for
each of the product's fields; and optionally, to extend that typeclass's
functionality to all sum types, so long as it is available for each of the
sum's variants.

In other words, if we know how to do something to each field in a product, then
we can do the same thing to the product itself; or if we can do something to
each variant of a sum, then we can do the same thing to the sum itself.

### Terminology

#### Sums and Products

In this documentation, and in Wisteria, we use the term _product_ for types
which are composed of a specific sequence of zero or more values of other
types. Products include case classes, enumeration cases, tuples and singleton
types, and the values from which they are composed are called _fields_. The
fields for any given product have fixed types, appear in a canonical order and
are labelled, though for tuples, the labels only indicate the field's position.
Singletons have no fields.

Likewise, we use the term _sum_ for types which represent a single choice from
a specific and fixed set of disjoint types. Sum types include enumerations and
sealed traits. Each of the disjoint types that together form a sum type is
called a _variant_ of the sum.

From a category-theoretical perspective, products and sums are each others'
duals, and thus fields and variants are duals.

In the following example,
```scala
sealed trait Temporal

enum Month:
  case Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec

case class Date(day: Int, month: Month, year: Int) extends Temporal
case class Time(hour: Int, minute: Int)
case class DateTime(date: Date, time: Time) extends Temporal
```
we can say the following:
- `Temporal` is a sum type
- `Date` and `DateTime` are variants of `Temporal`
- `Date`, `Time` and `DateTime` are all product types
- `day`, `month` and `year` are fields of `Date`
- `hour` and `minute` are fields of `Time`
- `date` and `time` are fields of `DateTime`
- `Month` is a sum type
- `Jan` through to `Dec` are all product types, all singletons, and all
  variants of `Month`
- the type, `(Month, Int)` (representing a month and a year) would be a product
  type, and a tuple

#### Typeclasses

A typeclass is a type (usually defined as a trait), whose instances provide
some functionality, through different implementations of an abstract method on
the typeclass, corresponding to different types which are specified in one of
the typeclass's type parameters. Instances are provided as contextual values
(`given`s), requested when needed through `using` parameters, and resolved
through contextual search (implicit search) at the callsite.

Where necessary, we distinguish clearly between a typeclass _interface_ (the
generic trait and abstract method) and a typeclass _instance_ (a `given`
definition which implements the aforesaid trait). The term _typeclass_ alone
refers to the typeclass interface.

The exact structure of a typeclass interface varies greatly, but typically, a
typeclass is a trait, with a single type parameter, and a single abstract
method, where the type parameter appears either in the method's return type or
in one or more of its parameters.

We call typeclasses whose type parameter appears in their abstract method's
return type _producers_, because they produce new instances of the parameter
type. Typeclasses whose type parameter appears in their abstract method's
parameters, _consumers_ because existing instances of the parameter type are
given to them. (The term _consumer_ shouldn't be misinterpreted to imply that
any value is "used up" in applying the typeclass's functionality; it will be
passed into a method, but will continue to exist for as long as references to
it continue to exist.)

Producers may be covariant (indicated by a `+` before their type
parameter), and consumers may be contravariant (indicated by a `-` before their
type parameter). But either can be defined as invariant.

For example,
```scala
trait Size[ValueType]:
  def size(value: ValueType): Double
```
is an invariant consumer typeclass interface for getting a representation (as a
double) of the size of an instance of `ValueType`. It might have instances
defined as:
```scala
object Size:
  given Size[Boolean] = new Size[Boolean]:
    def size(value: Boolean): Double = 1.0

  given Size[Char] with
    def size(value: Char): Double = 2.0

  given Size[String] = _.length.toDouble
```
and even,
```scala
given [ElementType](using size: Size[ElementType]): Size[List[ElementType]] =
  _.map(size.size(_)).sum
```
which constructs new typeclass instances for `List`s on-demand, and which
requires a typeclass instance corresponding to the type of the `List`'s
elements. Since `Size` is a single-abstract-method (SAM) type, it can be
implemented as a simple lambda corresponding to the abstract method.

Another typeclass example would be,
```scala
trait Default[+ValueType]:
  def apply(): ValueType
```
which is a covariant producer typeclass interface.

### Derivation

Wisteria lets us say, for a _particular_ typeclass interface but for _any_
product type, "if we have instances of the typeclass available for every field,
then we can construct a typeclass instance for that product type", and provides
the means to specify how they should be combined.

Dually, we can say that, for a _particular_ typeclass instance but for _any_
sum type, "if we have instances of the typeclass available for every variant of
the sum, then we can construct a typeclass instance for that sum type", and
provides the means to specify how the instances should be combined.

Naturally, fields and variants may themselves be products or sums, so generic
derivation may be applied recursively.

Hence, if we define all our datatypes out of products and sum types of "simple"
types, then for a particular typeclass interface, we can define typeclass
instances for the simple types plus a generic derivation mechanism, and
typeclass instances will effectively be available for every datatype.

Generic derivation for sum types is not always needed or even desirable, so we
will start by exploring product derivation.

### Deriving Products

#### Consumer Typeclasses

A typical example of a consumer typeclass is the `Show` typeclass. It provides
the functionality to take a value, and produce a string representation of that
value, and could be defined as,
```scala
trait Show[ValueType]:
  def show(value: ValueType): Text
```
with an extension method to make it easier to apply the typeclass:
```scala
extension [ValueType: Show](value: ValueType)
  def show: Text = summon[Show[ValueType]].show(value)
```

Generalizing over all products (and hence, all possible field types),
our task is to define _how_ a product type should be shown, if we're provided
with the means to show each of its fields.

So, if we have `Show` instances for `Int`s and `Text`s, then we want to be able
to derive a `Show` instance for a type such as:

```scala
case class Person(name: Text, age: Int)
```

However, in the general case, we do not know how many fields there will be or
what their types are, so we cannot rely on any of these details in our generic
derivation definition.

To use Wisteria, we need to import the `wisteria` package,
```
import wisteria.*
```
and add the `ProductDerivation` trait to the companion object of the type we
want to define generic derivation for, along with the stub for the `join`
method, like so:
```scala
object Show extends ProductDerivation[Show]:
  inline def join[DerivationType <: Product: ProductReflection]: Show[DerivationType] = ???
```

The signature of `join` must be defined exactly like this:
- it must be `inline`
- its type parameter must be a subtype of `Product`
- it must have a context bound on `ProductReflection`
- its return type must be an instance of the typeclass, parameterized on the
  method's type parameter

Given the return type, we know that we need to construct a new
`Show[DerivationType]` instance, so we can start with the definition,
```scala
object Show extends ProductDerivation[Show]:
  inline def join[DerivationType <: Product: ProductReflection]: Show[DerivationType] =
    new Show[DerivationType]:
      def show(value: DerivationType): Text = ???
```

We will implement `show` by calling the method `fields`, which is available as
a protected method inside `ProductDerivation`, and which allows us to map over
each field in the product to produce an array of values, by means of a
polymorphic lambda. `fields` also takes an instance of the product type, so it
can provide the actual field value from the product inside the lambda.

Here's what a call to `fields` looks like:
```scala
object Show extends ProductDerivation[Show]:
  inline def join[DerivationType <: Product: ProductReflection]: Show[DerivationType] =
    new Show[DerivationType]:
      def show(value: DerivationType): Text =
        val array: IArray[Nothing] = fields(value):
          [FieldType] => field =>
            ???

        ???
```

The polymorphic lambda may be unfamiliar syntax, but it can be thought of as
equivalent to as a lambda equivalent of a polymorphic method. So if the lambda
for,
```scala
def transform(field: Field): Text
```
is, `Field => Text`, then the lambda for,
```scala
def transform[FieldType](field: FieldType): Text
```
is, `[FieldType] => FieldType => Text`.

This is necessary because each field will potentially have a different type,
but in the context of the `fields` method, we know nothing about what these
types are, but it _is_ useful to be able to name the type. The lambda variable,
`field`, has the type `FieldType`.

Although we can refer to `field`'s type as `FieldType` in the lambda body, we
still have almost no information at all about the properties of this type. The
one thing we can assert, however, is that another occurrence of `FieldType` is
at least referring to the _same_ type.

Therefore, an instance of `Show[FieldType]`, regardless of where it comes from,
_will_ be able to show an instance of `FieldType`.

By default, Wisteria will make just such an instance available contextually
within the lambda body.
```scala
[FieldType] => field =>
  summon[Show[FieldType]].show(field)
```

So, for each field this lambda is invoked on, a `Show[Int]`, `Show[Text]` or
`Show[Person]` (or whatever type necessary) is summoned and supplied to it
contextually as a `Show[FieldType]`. It's also available contextually by name
as `context, so we can also write,
```scala
[FieldType] => field =>
  context.show(field)
```
but since it's contextual we can use the extension method above, and so it is
sufficient to write, `[FieldType] => field => field.show`.  ```


This gives us enough to construct an array of `Text` values corresponding to
each field in a product, which we can join together to surround the :
```scala
object Show extends ProductDerivation[Show]:
  inline def join[DerivationType <: Product: ProductReflection]: Show[DerivationType] =
    new Show[DerivationType]:
      def show(value: DerivationType): Text =
        val array: IArray[Nothing] = fields(value):
          [FieldType] => field =>
            field.show

        array.join(t"[", t", ", t"]")
```

This definiton is sufficient to generate new (and working) contextual instances
of `Show` for product types. Given the definition of `Person` above,
`Person(t"George", 19).show` would produce the string, `[George, 19]`.

#### Labels

This is close to what we need, but we would also like to include the type name.
This is available as a protected method of `ProductDerivation` called,
`typeName`, so we can adjust the last line to, `array.join(t"$typeName[", t",
", t"]")`, and our new derivation will produce the string, `Person[George,
19]`.

But we can go further. The name of each field can also be included in the
string output. The value `label` is provided as a named contextual value inside
`fields`'s lambda, so we can access the label for any field from within the
lambda. Changing the definition to,
```scala
[FieldsType] => field =>
  t"$label:${field.show}"
```
will change the output to `Person[name:George, age:19]`.

#### Special Product types

We might also like to provide different behavior for certain kinds of product
type; singletons and tuples. Singletons have no fields, so the brackets could
be omitted for these products. And tuples' names are not so meaningful, so
these could be omitted.

Two methods returning boolean values, `singleton` and `tuple` can be used to
determine whether the current product type is a singleton or a tuple. The
implementation of `join` can be adapted to provide different strings in these
cases.

#### Full Example

Since `Show` is a SAM type, we can also simplify the implementation and write
the implementation of `join` as a lambda. A full implementation would look like
this:
```scala
object Show extends ProductDerivation[Show]:
  inline def join[DerivationType <: Product: ProductReflection]: Show[DerivationType] = value =>
    if singleton then typeName else
      fields(value):
        [FieldType] => field => if tuple then field.show else t"$label=$field"
      .join(if tuple then t"[" else t"$typeName[", t", ", t"]")
```

#### Complementary Values

Some typeclasses operate on two values of the same type. An example is the `Eq`
typeclass for determining structural equality of two values:
```scala
trait Eq[ValueType]:
  def equal(left: ValueType, right: ValueType): Boolean
```

When defining the `join` method for `Eq`, we could use the `fields` method to
map over the fields of either `left` or `right`, but not both.

One solution would be to construct arrays of the field values of `left`, the
field values of `right` and the `Eq` typeclasses corresponding to each field.
(Although the field values, and hence their corresponding typeclass instances
will be different from each other, the types of the elements of the left and
right arrays will at least be pairwise-compatible.) We could then iterate over
the three arrays together, applying the each typeclass to its corresponding
left and right field value, and then aggregating the results.

While possible, this would be inefficient and would rquire a significant
compromise of typesafety: inside the lambda, a value and a typeclass will be
typed according to `FieldType`, and therefore uniquely compatible with each
other. But as soon as they are aggregated into an array, independent of each
other, their types would become incompatible, erased to `Any` or `Nothing`, and
could only be combined with explicit `asInstanceOf` casts.

Wisteria avoids this by making it possible, within the `fields` lambda of one
product value, to access the field value, from another product value, which
corresponds to the field in the current lambda, using the `complement` method,
and to provide it with the same type so that it is compatible with that field's
contextual typeclass instance.

Here's a full implementation of `Eq`:
```scala
object Eq extends ProductDerivation[Eq]:
  inline def join[DerivationType <: Product: ProductReflection]: Eq[DerivationType] =
    (left, right) =>
      fields(left):
        [FieldType] => leftField =>
          context.eq(leftField, complement(right))
      .foldLeft(true)(_ && _)
```

#### Producer Product Typeclasses

Producer typeclasses can also be generically derived. Wheras a _consumer typeclass_ will receive a pre-existing
instance of the derivation type as input, and produce a value of some invariant type, a _producer typeclass_
will take an invariant type as input, and will construct a new instance of the derivation type.

An example of a producer typeclass would be a simple `Random` typeclass which takes a long "seed" value as input
and constructs a random new instance from that seed. A `Random` instance for a generic product type should
produce a new product instance, all of whose field values are chosen randomly.

Here is the definition of `Random`:
```scala
trait Random[+ValueType]:
  def next(seed: Long): ValueType
```

For a producer typeclass derivation, The `join` signature will be identical, but instead of the `fields` method,
we will need to use the `construct` method to construct a new instance, without taking an existing instance of
the product type as input. A call to `fields` will also take a polymorphic lambda specifying the field type, but
since we have no preexisting instance, and therefore no fields, its lambda variable is a reference to the
typeclass instance which can be used to instantiate the new field value.

```scala
object Random extends ProductDerivation[Random]:
  inline def join[DerivationType <: Product: ProductReflection]: Random[DerivationType] = seed =>
    construct:
      [FieldType] => random =>
        ???
```

In fact, since we know nothing about the type of the field in the context of the lambda (except that we have a
name for it), the typeclass instance, which shares the same type in its parameter, is our _only_ means of
constructing a new instance for that field.

Therefore, by parametricity, the only sensible way to implement the method is to invoke the `next` method, like
so:
```scala
object Random extends ProductDerivation[Random]:
  inline def join[DerivationType <: Product: ProductReflection]: Random[DerivationType] = seed =>
    construct:
      [FieldType] => random => random.next(seed)
```

Calling `constuct`, specifying how each field's value will be computed, will return a new instance of the
product, `DerivationType`. Since `Random` is a SAM type, this expression of `Long => DerivationType` provides
a suitable implementation for the new typeclass.

### Deriving Sum Types

Deriving sums, or coproducts, is possible by making a choice of which of their variants is represented by the
sum type. Deriving sums may be omitted for many typeclasses, since it's not as commonly useful as deriving
products. But if it is desired in addition to product derivation, a typeclass's companion object will need to
extend `Derivation` instead of `ProductDerivation`, and define an additional `split` method.

Here are the adjusted stub implementations for the `Show` typeclass:
```scala
object Show extends Derivation[Show]:
  inline def join[DerivationType <: Product: ProductReflection]: Show[DerivationType] = ???
  inline def split[DerivationType: SumReflection]: Show[DerivationType] = ???
```

Note that `split`'s signature is similar to `join`'s, but lacks the subtype constraint on `DerivationType` and
uses a `SumReflection[DerivationType]` instead of a `ProductReflection`. An implementation of `split` will have
some similarities with a `join` implementation, but will use `variant` and `delegate` methods instead of
`fields` and `construct`.

#### Consumer Sum Types

To show an instance of a typeclass, we will use the `variant` method to inspect a preexisting instance of the
derivation type and apply a lambda to the one variant which matches. This is a dual of the `fields` method for
sum types, but unlike `fields` the lambda will apply _only_ to the matching variant; not to every variant.

Like `fields`, though, we have no greater knowledge about the type of that variant in the context of the lambda,
so once again, we will specify a polymorphic lambda which takes a `VariantType` type parameter. We do, however,
have one more piece of useful information about `VariantType` which we didn't know about a field's type:
`VariantType` must be a subtype of the derivation type. Therefore, we specify the lambda type variable as
`[VariantType <: DerivationType]`:
```scala
inline def split[DerivationType: SumReflection]: Show[DerivationType] = value =>
  variant(value):
    [VariantType <: DerivationType] => variant =>
      ???
```

So, in the body of the `variant` lambda, we now have an instance of `VariantType`, which we know to be a subtype
of `DerivationType`. This is actually exactly the same value as `value`, but its type has been refined—to a type
which is more precise; but also abstract.

As was the case with `fields`'s lambda, we have some additional context available in this lambda: `context`
is an instance of `Show[VariantType]` and `label` is the name of the variant.

A trivial implementation of this lambda would just call `variant.show`, since the contextual `Show[VariantType]`
value is available.

```scala
inline def split[DerivationType: SumReflection]: Show[DerivationType] = value =>
  variant(value):
    [VariantType <: DerivationType] => variant => variant.show
```

#### Complementary Variants

When we provided the product derivation for `Eq`, we used the `complement` method to get the corresponding field
with the correct type inside the body of `fields`. The same is possible inside the body of `variant`, but it
returns an `Optional` value, since an unrelated value of the same sum type is, by no means, guaranteed to be
the _same_ variant: if the other value is a different variant, then it would not make sense to resolve that
value with the same type—and so an `Unset` value is returned from `complement`.

If, however, both values represent the same variant, then we can access that value, safely typed with the same
type.

Here is an implementation of `split` for `Eq`:
```scala
inline def split[DerivationType: SumReflection]: Eq[DerivationType] = (left, right) =>
  variant(left):
    [VariantType <: DerivationType] => leftValue =>
      complement(right).let(context.equal(leftValue, _)).or(false)
```

The interpretation of this implementation is that if the left and right sum types represent the same variant,
then we use `context`, the typeclass instance that is common to both, to compare them. Otherwise, since they are
evidently different, we return `false`.

#### Producer Sum Typeclasses

As with the `construct` method for product types, the `delegate` method is used for producer sum types which
must return a new instance of the derivation type, without having a preexisting value to work with. While
`variant` can unambiguously resolve which of the variants its parameter value represents, just from its runtime
type, the method of discerning which variant is required from its input will depend on the type of that input,
and is not guaranteed to succeed.

Imagine defining a `Decoder` type which reads values from strings, and we expect the variant's type to be
encoded at the start of the string, for example, `"Developer:Hamza,39"` and `"Manager:Jane,52,2"` could both be
representations of instances of the sum type:
```scala
enum Employee:
  case Developer(name: Text, age: Int)
  case Manager(name: Text, age: Int, level: Int)
```

We would like to inspect the part of the string before the `:` and delegate to either the `Developer` or
`Manager` variants accordingly.

But the typeclass could be passed the string, `"Director:Beatrice,47"`, and no variant would exist in the
`Employee` sum type to delegate to.

As its first parameter, `delegate` expects the name of the variant (i.e. its `label` value) to delegate to. Its
second parameter is another polymorphic lambda. As with `construct` which had no `field` lambda variable,
`delegate` has no `variant` lambda variable, and (likewise) offers the matching variant's context.

For our `Decoder` example, we have:
```scala
object Decoder extends Derivation[Decoder]:
  inline def split[DerivationType: SumReflection]: Decoder[DerivationType] = text =>
    val prefix = text.cut(t":").head
    delegate(prefix):
      [VariantType <: DerivationType] => decoder =>
        ???
```

Having discerned which variant's decoder should be used, we can then use this to decode the text following the
`:`, like so:
```scala
object Decoder extends Derivation[Decoder]:
  inline def split[DerivationType: SumReflection]: Decoder[DerivationType] = text =>
    text.cut(t":") match
      case List(prefix, content) => delegate(prefix):
        [VariantType <: DerivationType] => decoder => decoder.decode(content)
```

### Optional Derivation

By default, derivation will fail at compiletime if a field's or variant's corresponding typeclass instance
cannot be found by contextual search. This is usually the desired behavior because it indicates the absence of
definitions which are inherently necessary.

But it's not unusual to want generic derivation to succeed, accepting that we should provide a fallback option
when a contextual value is not found. This can be achieved by importing `derivationContext.relaxed` in the scope
where `join` and `split` are defined.

The presence of this import will change the signature of methods such as `fields` slightly, so that the
contextual value provided to its lambda is an `Optional[Typeclass]` instead of a `Typeclass` instance. This
means that there will no longer be a contextual `Typeclass` available, so any calls which expect one will fail
to compile, but there will be a contextual `Optional[Typeclass]` value instead, and various control methods on
`Optional` values can be used to work with such a type.

We could take the `Show` example from earlier and adjust it to fall back to a field's `toString` value if a
`Show` typeclass does not exist for that type:
```scala
object Show extends ProductDerivation[Show]:
  inline def join[DerivationType <: Product: ProductReflection]: Show[DerivationType] = value =>
    fields(value):
      [FieldType] => field => context.layGiven(field.toString.tt)(field.show)
    .join(t"[", t", ", t"]")
```

This adjusted version refers to the contextual `Show[FieldType]` value, which is available as `context` inside
the lambda, and uses `layGiven` to provide the fallback option in the first parameter block, with the original
code (for when the typeclass *is* available) in the second block. This is made possible because when the
`Optional` value is present, `layGiven` injects its value contextually into this parameter block.

### Default Values

### Frequently-asked Questions

> How can I avoid generic derivation failing when a typeclass for one or more parameters is missing?

Include the import,
```scala
import wisteria.derivationContext.relaxed
```
in the context where `join` and `split` are defined. This will transform the type of the typeclass value
corresponding to the field from `TypeclassType[ValueType]` to `Optional[TypeclassType[ValueType]]`. Normally,
this also means that the typeclass will need to be applied explicitly.

> How can I use other unrelated typeclasses in a `join` or `split` implementation?

The signatures of `join` and `split` cannot be changed, so it is impossible to include other typeclass instances
in their implementations. But both are inline methods, so `summonInline` and `summonFrom` can be used to summon
instances of other typeclasses at compiletime, whether these relate to the derivation type or a field type.

> How can I use Wisteria for generic derivation without making the generically-derived typeclasses available to
> implicit search?

Use a non-companion object extending `Derivation` or `ProductDerivation` for the definitions of `join` and
`split`, and call the inline `derived` method on that object, passing in the derivation type.

> Why is a generically-derived typeclass instance not being found when it is summoned?

This is usually because typeclass instances relating to one or more field or variant values cannot be found. To
test this theory, try compiling an explicit call to the inline `derived` method at the callsite where
contextual search is failing.

> Why is another contextual instance being selected by contextual search instead of a generically-derived one?

Assuming the generically-derived typeclass instance *is* a valid candidate for selection, this is probably
because the derived candidate has a lower priority. Since the `given` instance is defined in either
`ProductDerivation` or `Derivation`, which is typically inherited by the typeclass's companion object, its
priority is naturally lower than `given` instances defined in the body of that companion object.

One solution would be to artificially reduce the priority of the undesired contextual instances, for example by
adding an additional `(using DummyImplicit)` parameter, or moving the definition to an inherited trait.

Another solution is to define `join` and `split` in an unrelated (non-companion) object, and to define an inline
given called `derived` directly in the companion object, like so:
```scala
object Unrelated extends ProductDerivation[Typeclass]:
  def join[DerivationType <: Product: ProductReflection]: Typeclass[DerivationType] = ???

object Typeclass:
  inline given derived[DerivationType]: Typeclass[DerivationType] = Unrelated.derived
```

> How can I generically-derive a typeclass for a type which indirectly refers to its own type in its fields?

A recursive type such as `Tree`,
```scala
enum Tree:
  case Leaf
  case Branch(left: Tree, value: Int, right: Tree)
```
cannot be derived in-place, and should be explicitly defined on that type's companion object. The easiest way to
do this is to add a `derives` clause to the companion. For example,
```scala
object Tree derives Typeclass
```

> Why does the compiler fail dering derivation with a long message that mentions that,
> `given instance derived in trait Derivation does not match type...`?

This is usually because the polymorphic lambda's type variable for `delegate` or `variant` is missing its upper
bound. It is essential that the type variable is specified as `[VariantType <: DerivationType]` and not just,
`[VariantType]`.

> Why does the compiler report a type mismatch between the derivation type and `Product`?

This is usually because the derivation type in the signature of `join` is missing the `<: Product` constraint.

## Status

Wisteria is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Wisteria is designed to be _small_. Its entire source code currently consists
of 434 lines of code.

## Building

Wisteria will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Wisteria?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Wisteria's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Wisteria and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `wisteria`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Wisteria's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Wisteria are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/wisteria/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Wisteria
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Wisteria was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Wisteria is a flowering plant, much like magnolia is, and Wisteria is a derivative of Magnolia.

In general, Scala One project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows a hazy, floral shape in pale colors.

## License

Wisteria is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

