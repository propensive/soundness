Utilities in _Rudiments_ are mostly provided through extension methods, and importing the `rudiments` package
will bring all utility methods into scope.

### Y-Combinator

An implementation of a [Y-Combinator](https://shorturl.at/jqKOY), called `fix`, is provided, implemented using a
Scala 3 context function, which enables slightly more favorable syntax than was possible in Scala 2. This method
makes it easier to write code in a point-free style.

The `fix` method takes a type parameter (which must be explicitly specified for type inference) and a lambda as
its first parameter, to which an additional parameter should be supplied as its initial value. Crucially, in the
body of `fix`'s lambda, a call to `recur` should be used to signal recursion.

This is best illustrated with an example. Here is an implementation of a factorial function.
```scala
def factorial(n: Int): Int =
  fix[Int] { i => if i <= 0 then 1 else i*recur(i - 1) } (n)
```

This avoids the explicit definition of a private or nested helper function, which would normally be necessary
for a definition such as this.

### Primitive `String` Extractors

Extractors for all the primitive types are provided for matching on `String`s. These are defines as extensions
to the (virtual) companion objects of the primitive types, so they have the same names as the types they
extract.

Here is an example of them in use:
```scala
def parse(number: String): Boolean | Int | Double =
  number match
    case Boolean(b) => b
    case Int(i)     => i
    case Double(d)  => d
    case _          => 0
```

### Typesafe `String` operations

The extension method `String#cut` has identical semantics to `String#split`, but returns an immutable `IArray`
instead of an `Array`. Likewise, the methods `String#bytes` and `String#chars` mirror `String#getBytes` and
`String#toCharArray`, returning an `IArray[Byte]` and `IArray[Char]` respectively.

The `bytes` method currently uses the `UTF-8` in all cases, though this may change if there is sufficient
demand.

Four variants of the extension method `join` are provided on `Traversable[String]` instances, which provide the
same functionality as `mkString` (but only operating on `String`s) with one additional two-parameter version
that's specialized for natural language lists where each element is separated by a comma except the last, which
is preceded by a word such as `and` or `or`.

For example,
```scala
List("one", "two", "three").join(", ", " or maybe ")
```
will produce the string `one, two or maybe three`.

### Lightweight system property access

Many JVM system properties are available in the map, `System.getProperties` and are typically given identifiers
written in a dot-notation style, such as `user.dir`. Rudiments provides syntactic sugar for accessing these
dynamically through the `Sys` object, for example,
```scala
val pwd: Option[String] = Sys.user.dir()
```

### `unit`

Often a side-effecting expression returns a value which is not used at a particular call site, and can be
discarded. However, the expression's return type can result in type-inference choosing an undesired return type,
when `Unit` would be preferable, or a compile-time warning about discarded values may be produced.

The `unit` extension method silently discards the return value of any expression, and instead produces a `Unit`,
`()`.

### `only`

The `only` extension method applies a partial function to a value and lifts the result into an option.

For example,
```scala
val result: Option[Int] = string.only { case Int(i) => i*i }
```

### `str""` Interpolator

The `s""` interpolator takes parameters of `Any` type as substitutions, calling `String#toString` on them as
necessary. This may be considered too permissive, so `str""` is provided as a typesafe alternative that requires
every substitution to be a `String`.

### `twin` and `triple`

These two extension methods produce a two-tuple and a three-tuple (respectively) of repetitions of the value it
is applied to. This can be useful in a subsequent `map` operation.


