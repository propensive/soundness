All Jacinta terms and types are in the `jacinta` package, and exported to the `soundness` package.
So we begin either by importing,
```scala
import jacinta.*
```
or:
```scala
import soundness.*
```

### Core types

Jacinta's most important type is `Json` which represents an instance of a JSON value, that is,
a JSON object, array or primitive (string, number or boolean). It does not represent
_serialized JSON_, so details like whitespace and the ordering of keys in an object are not
represented.

Scala's type system knows nothing more about the internal structure of a `Json` value than this. So
a `Json` value representing the number `12` is indistinguishable by the type system from a `Json`
value representing an array of complex objects.

### Parsing

We can obtain a `Json` value by constructing one from existing values.. Or we can parse some textual
input.

The `Json.parse` method takes any input that is `Readable by Bytes`. This includes not only
`Text` and `Bytes` values, but other types like filesystem `Path`s—if suitable context is provided
for a `Readable by Bytes` value to be resolved.

Here is an example of parsing `Text` as JSON:
```scala
Json.parse(t"""{ "name": "Alfred", "age": 83 }""")
```

Calling `Json.parse` may raise a `JsonParseError`, so this should be handled in some way. Full
details of error handling in Soundness is provided by
[Contingency](https://github.com/propensive/contingency/).

### Dynamic field access

As a dynamically-typed value, Scala's type system does not know anything about the fields that are
available on a particular `Json` value. It does not even know if it is an object with fields, an
array with indices, or a primitive value.

But we may know more than the type system. Or at least, we may wish to program to the assumption
that we know more. So Jacinta makes it possible to access fields and array indices _dynamically_
using selection or arbitrary field names with the familiar `.`, and application with parentheses
for numeric indices.

This would normally be a significant compromise on typesafety, since it would allow us to call
nonexistent methods on `Json` values, without protection from the compiler. So access must be
explicitly enabled with the import:
```scala
import dynamicJsonAccess.enabled
```

With this contextual value in-scope, we can dereference and deindex `Json` values, dynamically.
The result will always be another `Json` value, ready to be deindexed or dereferenced, or a
`JsonError` will be thrown if the index is out of range, or the object key does not exist.

`Json` values are not useful (in most cases) for use elsewhere in Scala code, unless we can
convert them to typed Scala values. This is called _decoding_.

### Decoding `Json` values



### Encoding as `Json`

Many Scala values can be mapped directly (and often unambiguously) to JSON values. Trivially, this
includes `Text`, `Boolean` and various numeric types. But collection types like `List` and `Set`
can also be mapped to JSON arrays, if their elements are types which can be mapped. And case
classes and tuples of these types may also be mapped, so long as their elements can. With a suitable
choice of encoding, sum types (like enumerations or sealed traits) can also be mapped.

This is true compositionally. For example, `List`s of sealed traits, composed of case classes whose
parameters are tuples of `Int`s, `Set`s and other case classes are equally encodable.

Encoding a value to `Json` is as simple as calling `.json` on that value. If it is able to be
encoded, the result will be a `Json` value. (Note that this is not the same as encoding a `Json`
value to string-like representation, which is a useful—but different—operation, described below.)

Here is an example of a simple, but nontrivial case-class structure:
```scala
case class Person(firstName: Text, lastName: Text)
case class Recipient(person: Person, emailAddress: Text)

val recipient = Recipient(Person(t"Piotr", t"Nowak"), t"pn@example.com")
```

Given these definitions, the `recipient` instance can be encoded with `recipient.json` into a
`Json` value representing the following JSON object:
```json
{
  "person": {
    "firstName": "Piotr",
    "lastName": "Nowak"
  },
  "emailAddress": "pn@example.com"
}
```

#### Coproducts


### Encoding as `Text`


### Decoding as `Json`



Sealed traits of two or more case class subtypes will be serialized to JSON objects, exactly as each of the
subtypes would be, but with an additional field called `_type`, whose value will be set to the unqualified type
name, e.g. `"_type": "Leaf"`.

Although this encoding of coproduct types is non-standard, it is a reasonable default, and can always be
overridden with specific typeclass instances for the sealed trait type.

#### Collections

Furthermore, all traversable standard collection types can be serialized to JSON arrays, provided the elements
of the collection can be.

### Acessing values

Instances of `Json` are dynamically-typed which means that members with arbitrary names may be accessed as if
they were methods. Taking the `recipient` example above, it would be valid to access `recipient.person`, as if
the method `person` existed on the `Json` type. It doesn't, but since `Json` instances inherit from the special
`Dynamic` trait, the code will be transformed into `recipient.selectDynamic("person")` at compiletime, which
will return a new `Json` instance representing the JSON:
```json
{
  "firstName": "Mike",
  "lastName": "Smith"
}
```

It is therefore possible to call `recipient.person.firstName` directly and get a `Json` value representing the
JSON string, `"Mike"`.

As dynamic values with little known statically about them, instances of type `Json` are not particularly useful
_directly_, and should be converted to other types like `Text`, `Int` or `Person` before being used elsewhere in
a program. This is achieved with the `Json#as` method which takes the destination type as a parameter, for
example,
```scala
val addressee: Text = recipient.person.firstName.as[Text]
```
or,
```scala
val person: Person = recipient.person.as[Person]
```

As well as accessing arbitrary fields in a JSON object, elements of an array may be accessed by simply applying
the integer index to a `Json` value representing an array, for example, `json.organisation.users(2).as[User]`.

#### Errors

Since dynamic field access is unchecked at compiletime, it's possible that a JSON object would not contain the
requested field, or a JSON array would not contain the requsted index. This will throw an exception only when
attempting to convert the value to a static type. So the expression, `recipient.user.firstName`, (noting that
`user` is not a valid field of `recipient`) would not produce an error in itself. Only when invoking,
`recipient.user.firstname.as[Text]` would an exception be thrown, of type `JsonAccessError`.

Similarly, if the expression, `recipient.person.firstName.as[Int]` were evaluated, a `JsonAccessError` would be
thrown due to the field, `firstName` being a JSON string and not a JSON number.

All methods which throw exceptions are annotated with `throws` clauses, and if `saferExceptions` is enabled,
these must be handled.

### Typeclasses

While all Java primitive types and `String`s, collection types and case class types can be serialized and
deserialized automatically, it's possible to support other types or to replace existing default implementations
by providing contextual instances of the typeclasses, `Json.Writer` and `Json.Reader`.

For example, assuming the existence of an `Email` type (which simply wraps a `Text` instance), a `Reader` and
`Writer` for `Email` could be provided in `Email`'s companion object, like so:
```scala
case class Email(value: Text)

object Email:
  given Json.Reader[Email] = json => Email(json.as[Text])
  given Json.Writer[Email] = _.value.json
```
Note that `Email` is a case class, so default instances of `Json.Reader[Email]` and `Json.Writer[Email]` would
exist already, but would be replaced by these new definitions. (If `Email` were instead a non-`case` `class`,
these would be chosen unambiguously as the only contextual instances.)

#### Functor and Cofunctor

`Json.Reader`s are functors, and the `Reader#map` method is provided to transform a reader of one type into a
reader of another. Likewise, `Json.Writer`s are cofunctors with `Writer#contramap` methods. Given these
definitions, an alternative way to write the definitions for `Email` by transforming the existing instances for
the `Text` type would be:
```scala
object Email:
  given Json.Reader[Email] = summon[Json.Reader[Text]].map(Email(_))
  given Json.Writer[Email] = summon[Json.Writer[Text]].contramap(_.value)
```
