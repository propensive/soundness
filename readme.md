[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/denominative/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/denominative/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Denominative

____

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

## Features

- provides an `Ordinal` type representing ordinal numbers
- `Ordinal`s should be used for values which zero-indexed or one-indexed cardinal numbers could be ambiguous
- `Ordinal`s are distinct from cardinal `Int`s
- conversions between `Ordinal`s and `Int`s may be made only by specifying zero- or one-indexing
- introduces a distinct namespace for the first ten ordinals

## Availability

Denominative has not yet been published as a binary.

## Getting Started

__Denominative__ introduces a distinction between the types of cardinal and ordinal numbers with the clear and
ambitious goal of avoiding off-by-one errors, without compromising performance.

## Status

Denominative is classified as ____. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Denominative is designed to be _small_. Its entire source code currently consists
of 79 lines of code.

## Building

Denominative can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Denominative are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/denominative/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Denominative easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Denominative was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name



In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo



## License

Denominative is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
