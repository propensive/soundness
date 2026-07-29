## Optional Values

### About

A value that might be absent is an `Optional`. Unlike an `Option`, an `Optional` adds no
wrapper: a present value is stored as itself, and absence is a single sentinel, `Unset`.
`Optional[Text]` and `Text` differ only in their type, not in how a present value is
written or held, so the common case — a value that is there — carries no cost and no
ceremony.

Because `Optional[value]` is exactly the union `Unset | value`, optionality is flat. Wrapping
an `Optional` in another collapses — `Optional[Optional[Int]]` is again `Optional[Int]` —
so the ambiguous state that `Option` allows, a present absence like `Some(None)`, cannot be
represented at all.

### On absence

Three ways to say "no value" are in common use, and each has a flaw. A `null` is invisible
in the type and blows up only when it is dereferenced. An `Option` is visible and safe, but
it boxes every present value in a `Some`, and it nests: `Option[Option[T]]` has two distinct
empties, and code must thread through both layers. Neither is comfortable for the frequent
case where a value is simply present.

An `Optional` keeps the safety of `Option` — absence is in the type, and the compiler makes
sure it is handled — while behaving like the value itself when present. It rests on Scala's
[union types](https://docs.scala-lang.org/scala3/reference/new-types/union-types.html):
`Unset | value` needs no wrapper, and because a union is a set, the same type never nests —
the [optionality](../philosophy/optionality.md) principle in its purest form. Everything
comes from the `soundness` package:

```scala
import soundness.*
```

### Representing absence

An optional value is typed `Optional[…]` and assigned exactly as a plain value would be; an
absent one is `Unset`:

```scala
val name: Optional[Text] = t"Ada"
val nickname: Optional[Text] = Unset
```

### Supplying a fallback

`or` yields the value if it is present and a fallback otherwise. The fallback is evaluated
only when it is needed:

```scala
name.or(t"anonymous")       // t"Ada"
nickname.or(t"anonymous")   // t"anonymous"
```

### Transforming

`let` applies a function to a present value and passes `Unset` through untouched. Because
optionality is flat, `let` serves where both `map` and `flatMap` would be needed with
`Option` — a function that itself returns an `Optional` does not add a layer:

```scala
name.let(_.upper)                 // Optional[Text], upper-cased when present
name.let(_.take(1)).let(initial)  // chains without nesting
```

`lay` handles both cases at once, giving a fallback for absence and a function for presence,
and so returns a plain value rather than an `Optional`:

```scala
name.lay(0)(_.length)   // the length when present, 0 when absent
```

### Testing presence

`present` and `absent` report which case a value is in, and `mask` discards a present value
that fails a predicate:

```scala
if name.present then greet(name.vouch)
name.mask(_.length > 64)   // Unset if unreasonably long, else unchanged
```

### Requiring a value

Where a value is known to be present, `vouch` asserts it, panicking if the assertion is
wrong. `presume` instead falls back to the type's default — an empty text, a zero, an empty
list — for types that declare one:

```scala
name.vouch       // the Text, or a panic if it was Unset
nickname.presume // t"" — the default Text
```

### Absence from collections

A collection of optional values compacts to the values that are present, dropping every
`Unset`:

```scala
List(t"a", Unset, t"c").compact   // List(t"a", t"c")
```

### Building an optional

Any value becomes optional by a condition. `unless` discards it when a predicate holds,
`puncture` discards one particular value, and `only` keeps a value only where a partial
function is defined:

```scala
count.unless(_ == 0)            // Unset when count is zero
input.puncture(t"")            // Unset for the empty string
value.only { case n if n > 0 => n*n }
```

### Working with `Option`

Where an `Option` is unavoidable — at the boundary with other libraries — the two convert
in both directions:

```scala
name.option                    // Some(t"Ada"): Option[Text]
Some(t"Ada").optional          // t"Ada": Optional[Text]
```

### What it costs

An `Optional[Text]` *is* a `Text` — there is no wrapper, so a present value costs nothing to hold,
nothing to pass, and nothing to read. That is the whole reason for the design: `Option` allocates
a `Some` for every present value, which in a data structure of a million records is a million
objects that exist only to say "yes, there is one".

The operations are inlined too, and `or` is optimized as it compiles: a chain of fallbacks
collapses rather than building intermediate optionals, and a fallback whose value is a constant
becomes a constant.

The cost of that representation is that `Optional[Optional[T]]` cannot exist — absence has one
representation, so it cannot nest. This is checked as the code compiles: a type that could not be
made optional unambiguously is rejected where it is written.

### Requiring an unambiguous type

Three type-level facts support that check, and are occasionally useful directly.

`Concrete` witnesses that a type is a definite type rather than an abstract one, which matters
because an abstract type might later be instantiated to something already optional. `Distinct`
witnesses that a type is not a member of a given union — the check that keeps `Unset` out of the
value type. `Mandatable` strips the optionality from a type, giving the type a present value has.

A generic method that takes an `Optional[value]` for an abstract `value` therefore demands
evidence that `value` is concrete, and the demand appears in its signature rather than as a
failure at a call site far away.

### Defaults

`presume` and `default` draw on a `Default` instance, which supplies the value a type falls back
to. Instances exist for the types with an obvious zero — empty text, zero, an empty collection —
and a type declares its own by defining one:

```scala
given Default[Volume] = Volume(0)

setting.presume   // Volume(0) where the setting is unset
```

A `Default` is a by-name thunk rather than a value, so a default that is expensive to compute is
computed only where it is used.
