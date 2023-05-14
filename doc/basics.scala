### Basic Usage

Given a value of some type, e.g. a value `artifact` of type `Artifact`, calling `artifact.show` will
try to resolve a `Show[Artifact]` typeclass and apply it to `artifact`. If such a contextual instance
exists in scope the result will be a `Text` value; if it does not, then a compile error will be raised.

Calling `artifact.debug` works similarly, but will never fail to compile. First, a `Debug[Artifact]`
will be sought and applied. Failing that, a `Show[Artifact]` will be tried instead. Finally, if that
fails, a `Text` value will be created from `artifact.toString`. Since `toString` is always defined
(however inadequately), the call always succeeds.

Note that this only applies when calling `debug` on a value; summoning the `Debug` instance and
applying it directly will still fail to compile if no `Debug` instance exists in scope.

### Defining `Show` and `Debug` instances

Creating a given instance of `Show` or `Debug` is simple in both cases. Both are single-abstract-method
traits, so definitions are often take just a single line. For example,
```scala
given Debug[Short] = short => t"Short("+short.toString+t")"
```

### Generic derivation

Every product type, such as case classes, will have a default `Debug` instance, derived from calling
`debug` on each of its parameters. This will exist even if some parameters do not have their own
`Debug` instance, since a `Show` typeclass will be used as a fallback, and `toString` as a last resort.

For example, the case class,
```scala
class Id(id: String) { override def toString(): String = id }
case class Person(id: Id, name: Text, age: Int)
Person(Id("jqm"), t"Jennifer", 61).debug
```
will produce the text,
```
Person(id=jqm,name=t"Jennifer",age=61)
```
using the `toString` of `Id` and the `Debug` instances (provided by Spectacular) for `Text` and `age`.
Note that `Text`'s `Debug` instance produces pastable code, rather than simply returning the exact same
`Text` value, while its `Show` instance does exactly that.

### Showing `Boolean` values

The values `true` and `false` often mean different things in different scenarios, and without
specifying, Spectacular will not presume any particular `show` serialization of a `Boolean` value.

But by importing a `BooleanStyle` value from the `spectacular.booleanStyles` package, a style can be
chosen, for example,
```scala
import booleanStyles.yesNo
Io.println(true.show)
```
will print the text `yes`.
