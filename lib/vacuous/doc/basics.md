### The `Optional` type

An optional value, which might be an instance of `ValueType`, or may be
_absent_, may be given the type `Optional[ValueType]`. If it is absent, then it
has the value, `Unset`, which is a singleton object. `Optional[ValueType]` is
an alias for the union type, `ValueType | Unset.type`.

Note that the declarations,
```scala
val value: Text = t"Hello world"
```
and,
```scala
val value: Optional[Text] = t"Hello world"
```
differ only in their types; the syntax of the expression is identical, and does
not need to be wrapped with another factory method, like `Some`.

Since union types are unordered sets of types, nesting two `Optional`s, for
example in `Optional[Optional[Int]]` expands to
`Int | Unset.type | Unset.type`, which is identical to `Int | Unset.type`. And
this is the same as `Optional[Int]`. While there is nothing to prevent nesting
one `Optional` within another `Optional`, it's impossible to distinguish
between the types, and impossible for an `Unset` value to be considered
_present_ rather than _absent_; it is the definition of absence.

### `or`, `let` and `lay`

An `Optional[Text]` value may seem very similar to a `Text` value, but the
possibility that it might be `Unset` makes it impossible to use any methods
defined on `Text` on an `Optional[Text]`, since those methods are not
applicable to just one of the possible values of the type, `Unset`. So several
convenience methods are provided to make `Optional`s easy to work with.

The method `or` replaces the `Unset` value with another value, eliminating the
optionality from the type. This is equivalent to _both_ `getOrElse` and
`orElse` on `Option`. This equivalence comes from the lack of nesting of
`Optional` values.

Similarly, `let` applies a lambda to the _present_ values, and leaves the
_absent_ value unchanged. It is equivalent to both `map` and `flatMap` on
`Option`s.

Finally, `lay` combines `or` and `let` in a single, two-parameter method: the
alternative value for `Unset` is specified first, followed by the lambda
mapping the _present_ values. This is equivalent to `fold` on an `Option`.

These method names were deliberately chosen to be short, as they are intended
to be used frequently and are rarely the most interesting part of an
expression.

