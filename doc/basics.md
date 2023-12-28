All Spectacular terms and types are defined in the `spectacular` package:
```scala
import spectacular.*
```

### `Show` and `Debug`

Given a value of some type, e.g. a value `artifact` of type `Artifact`, calling
`artifact.show` will try to resolve a `Show[Artifact]` typeclass and apply it
to `artifact`. If such a contextual instance exists in scope the result will be
a `Text` value; if it does not, then a compile error will be raised.

Calling `artifact.debug` works similarly, but will never fail to compile:
First, a `Debug[Artifact]` will be sought and applied. Failing that an
`Encoder[Artifact]` will be used (see below). Then, a `Show[Artifact]` will be
tried instead. Finally, if that fails, a `Text` value will be created from
`artifact.toString`. Since `toString` is always defined (however inadequately),
the call always succeeds.

Note that this only applies when calling `debug` on a value; summoning the
`Debug` instance and applying it directly will still fail to compile if no
`Debug` instance exists in scope.

Both `Show` and `Debug` provide very similar functionality, and for many types,
their implementations will be the same. They differ primarily in their intended
audience: `Show` instances should produce textual output which is intended for
_users_, not _programmers_, while `Debug` output is intended only for
programmers. The absence of a `Show` typeclass for a particular type
corresponds to that type not being intended for user consumption, while a
programmer should always be happy to see _some representation_ of a value,
however technical.

Thus, messages which are intended for end-users should use `Show` and
"internal" messages should use `Debug`.


### Defining `Show` and `Debug` instances

Creating a given instance of `Show` or `Debug` is simple in both cases. Both
are single-abstract-method traits, so definitions are often take just a single
line. For example,
```scala
given Debug[Short] = short => t"Short(${short.toString})"
```

Since the `debug` extension method will fall back to using a `Show` instance
whenever a `Debug` instance is not available, if the implementations are
identical, then it is sufficient just to provide a `Show` instance.

### Generic derivation

Every product type, such as case classes, will have a default `Debug` instance,
derived from calling `debug` on each of its parameters. This will exist even if
some parameters do not have their own `Debug` instance, since a `Show`
typeclass will be used as a fallback, and `toString` as a last resort.

For example, the case class,
```scala
import anticipation.Text

class Id(id: String) { override def toString(): String = id }
case class Person(id: Id, name: Text, age: Int)
val person = Person(Id("jqm"), t"Jennifer", 61).debug
```
will produce the text,
```
Person(id=jqm,name=t"Jennifer",age=61)
```
using the `toString` of `Id` and the `Debug` instances (provided by
Spectacular) for `Text` and `age`.  Note that `Text`'s `Debug` instance
produces pastable code, rather than simply returning the exact same `Text`
value, while its `Show` instance does exactly that.

### Showing `Boolean` values

The values `true` and `false` often mean different things in different
scenarios, and without specifying, Spectacular will not presume any particular
`show` serialization of a `Boolean` value.

But by importing a `BooleanStyle` value from the `spectacular.booleanStyles`
package, a style can be chosen, for example,
```scala
import turbulence.Out
import turbulence.stdioSources.jvm
import booleanStyles.yesNo

def run(): Unit = Out.println(true.show)
```
will print the text `yes`.

Several `BooleanStyle` options are provided in the `booleanStyles` package,
 - `yesNo` - `yes` or `no`
 - `onOff` - `on` or `off`
 - `trueFalse` - `true` or `false`
 - `oneZero` - `1` (`true`) or `0` (`false`)
and it is trivial to provide alternatives, for example:
```scala
import gossamer.*
given posNeg: BooleanStyle = BooleanStyle(t"+", t"-")
```

### `Encoder` and `Decoder`

A further typeclass, `Encoder`, also converts from a particular type to `Text`,
but comes with a complementary `Decoder` typeclass and has a particular intent:
it is intended to represent a canonical way to encode a value as a string, such
that that text may be decoded to restore the original value.

For example, a `Url` (as defined in
[Nettlesome](https://github.com/propensive/nettlesome/)) represents the
structure of a URL, but is encoded in a very standard way to a familiar
representation of a URL, such as `https://example.com/`. This conversion should
be provided by an `Encoder` instance, and a corresponding `Decoder` should be
provided in order to parse the `Text` representation of the URL back into a
`Url` instance.

`Encoder`s and `Decoder`s are intended to be used by libraries which use text
as a serialization format. [Jacinta](https://github.com/propensive/jacinta/)
allows any type for which an `Encoder` exists to be serialized to JSON, and any
type for which a `Decoder` exists to be read from JSON.
[Xylophone](https://github.com/propensive/xylophone/) provides the same
functionality for XML and [Cellulose](https://github.com/propensive/cellulose/)
for CoDL.

#### Decoding errors

While encoding to text will normally succeed in all cases, it's common for
decoder (or deserialization) to fail, if the input text is in the wrong format.
However, the API of `Decoder` does not include any optionality in the signature
of its `decode` method. That's because _capabilities_ should be used to handle
failures, with greater flexibility.  Given `Decoder` instances should include
appropriate `using` clauses to demand the capability to raise errors. If using
[Perforate](https://github.com/propensive/perforate/) for error handling, that
implies a `Raises` instance, while Scala's checked exceptions require a
`CanThrow` instance for the exception type.

