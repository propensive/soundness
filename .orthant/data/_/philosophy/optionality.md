# Optionality

Soundness represents an absent value with a flat `Optional`, not with `null` and not
with `Option`. Unlike `null`, an `Optional` is visible in the type and cannot be
dereferenced by accident; unlike `Option`, it adds no wrapper around the value, so a
present value is simply the value itself and optionality does not stack into nested
layers. Absence is dealt with where it arises rather than threaded through the whole
program, which keeps the common case — a value that is present — direct and
unencumbered. In Soundness, `null` is treated as both unrepresentable and unnecessary.

## Absence without a wrapper

An `Optional[Text]` *is* a `Text`, or it is `Unset`. There is no `Some` to allocate and
no `Some` to unwrap:

```scala fuel
parse
##
val name: Optional[Text] = t"Hal"
val missing: Optional[Text] = Unset

name.or(t"anonymous")     // t"Hal"
missing.or(t"anonymous")  // t"anonymous"
```

The operations read as the questions they answer:

```tabular
column  Method
column  Does
column  Result type
row  `or`  supplies a fallback for an absent value  the value's type
row  `let`  transforms a present value, leaving an absent one alone  an `Optional`
row  `lay`  supplies a fallback and a transformation together  the transformation's type
row  `present`  asks whether there is a value  `Boolean`
row  `absent`  asks whether there is none  `Boolean`
```

And `assume` insists on presence where the surrounding code has already established it, throwing
an `Optional.Error` if that was wrong:

```scala fuel
parse
##
name.let(_.upper)              // Optional[Text]
name.lay(t"nobody")(_.upper)   // Text, either way
name.present                   // true
name.assume                    // Text, or an Optional.Error if that was wrong
```

## Why the wrapper costs more than it looks

`Option` allocates one object per present value. In a single expression that is
invisible; in a table of a million records with three optional columns it is three
million objects that exist only to say "yes, there is one" — and every read of one is a
pointer to chase.

The deeper cost is that wrappers *stack*. `Option[Option[T]]` is a legal type with three
inhabitants and no agreed meaning, and it arises whenever a lookup that may fail returns
a value that may itself be absent. Code that meets one usually flattens it and hopes the
two absences meant the same thing.

`Optional` cannot stack, and not by a rule that forbids it. `Optional[value]` is the union
`Unset | value`, and a union absorbs a repeated member, so `Optional[Optional[Int]]`
*is* `Optional[Int]` — the same type, arrived at by ordinary type equality rather than by
a check that rejects it.

The ambiguous type therefore never arises to be flattened away. There is no `Some(None)`
to disambiguate because there is nothing for it to be.

The three representations of absence, side by side:

```tabular
column  Representation
column  Visible in the type
column  Present value
column  Nested absence
row  `null`  no  the value itself  cannot be expressed
row  `Option`  yes  wrapped in a `Some`  `Option[Option[T]]`, with no agreed meaning
row  `Optional`  yes  the value itself  absorbed: `Optional[Optional[Int]]` is `Optional[Int]`
```

## What it costs

Two things, and they are worth stating plainly.

The first is the price of that absorption. It works because the compiler can see both
members of the union — and for an *abstract* type it cannot: a `value` later instantiated
to something already optional would merge two absences that were meant to be distinct. So
a generic method taking an `Optional[value]` needs evidence that `value` is a definite
type, and the evidence appears in the signature:

```scala fuel
parse
##
def firstOrElse[value: Concrete](values: List[Optional[value]], fallback: value): value =
  values.compact.head.or(fallback)
```

The second is that `Optional` is not a monad and does not pretend to be one. There is no
`flatMap` threading absence through a `for` comprehension. Absence is handled where it
arises — with `or`, `let` or `lay` — which is the point rather than an omission: a value
that might be absent is made definite close to where it was obtained, instead of being
carried through the program in a wrapper that every later step must accommodate.

## Meeting `Option` at the boundary

Other libraries return `Option`, and a Soundness value sometimes has to become one. The
conversion is explicit in both directions and confined to the boundary:

```scala fuel
parse
##
name.option              // Some(t"Hal"): Option[Text]
Some(t"Hal").optional    // t"Hal": Optional[Text]
```

Converting at the edge rather than adopting `Option` throughout is the same discipline
applied to `null`: a foreign representation is translated once, on arrival, and the
program's interior speaks one language.

See [zero cost](zero-cost.md) for the general principle this is an instance of, and
[impossible states](impossible-states.md) for why the non-nesting matters.
