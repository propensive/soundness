## The `Ordinal` type

_Denominative_ introduces a new type, `Ordinal`, which represents an ordinal number. Unlike cardinal numbers
(still represented by `Int`s and `Long`s) an `Ordinal` has a first element, called `Prim`, which unambiguously
refers to the first element of any sequence without the need to specify if the sequence is zero- or one-indexed.
(There is, absolutely fundamentally, no concept of a "zeroth" `Ordinal`.)

`Ordinal` is represented internally by an `Int`, so shares the performance characteristics of using `Int`s, but
is a distinct type. Thus, an `Int` such as `1`, `138` or `-12`, cannot be used where an `Ordinal` is expected,
and an `Ordinal` cannot be used where an `Int` is expected.

Conversions between `Int`s and `Ordinal`s may only 

The first ten `Ordinal` numbers have names, which arise from the first part of the sequence that begins,
"primary", "secondary", "tertiary", etc.:
 - `Prim`
 - `Sec`
 - `Ter`
 - `Quat`
 - `Quin`
 - `Sen`
 - `Sept`
 - `Oct`
 - `Non`
 - `Den`

However, in practice, only `Prim` and `Sec` are likely to find regular use.

Given a sequence of elements, it's often useful to be able to refer to the last or second-to-last elements. This
is possible with the `ult` (short for "ultimate") and `pen` (short for "penultimate") extension methods that are
available on any _countable_ value, and which return the `Ordinal` referring to these elements. A _countable_
value typically means a `Seq` or one of its subtypes, but is actually an instance of any type that implements
the `Countable` typeclass.

The `ante` extension method refers to the ordinal before `pen`, that is, the third-to-last `Ordinal` index.

### Arithmetic

Certain arithmetic operations are possible between `Ordinal` values and `Int`s, but many operations that exist
for cardinal numbers (such as multiplication and division) do not make sense for ordinal numbers.

Here are some valid operations.

A cardinal number may be added to an `Ordinal`:
```scala
val ordinal: Ordinal = Ter + 3    // Sen
val ordinal2: Ordinal = 3 + Quin  // Oct
```

A cardinal number may be subtracted from an `Ordinal`:
```scala
val ordinal: Ordinal = Den - 7  // Ter
```

One `Ordinal` may be subtracted from another:
```scala
val cardinal: Int = Non - Sept  // 2
```

## `Interval`s

A range of `Ordinal`s is represented by an `Interval`. In all cases, this is a closed or "inclusive" interval,
and is specified by its first `Ordinal` and its (included) final `Ordinal`. For example,
```scala
val interval: Interval = Ter to Sen
```
would represent the elements 2, 3, 5, 8 of the Fibonacci sequence, 1, 1, 2, 3, 5, 8, 13, 21, etc.

If, on the other hand, we had a finite sequence, `xs`, 2, 4, 6, 8, 10, 12, we could refer to all but the first
and last elements by writing `Sec to xs.pen`. This would produce an interval representing the ordinals,
`Sec`, `Ter`, `Quat`, `Quin`; omitting `Prim` and `Sen` (which would be `xs.ult`).

The size of an `Interval` is a cardinal number, thus an `Int`. We can get an `Interval`s size with the `size`
method.

### Iterating over `Interval`s

An `Interval` is most useful as a way of specifying a range of `Ordinal` values because we want to perform some
operation iteratively using each of the values. Two methods are provided: `foreach` and `foldLeft`, which
behave exactly as their familiar counterparts in Scala's standard collections library.
