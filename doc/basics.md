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

So, if we have `Show` instances for `Int`s and `Text`s, then we want to be able to derive a `Show` instance for a type such as:
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
