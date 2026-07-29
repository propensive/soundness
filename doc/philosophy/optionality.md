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

```scala
val name: Optional[Text] = t"Ada"
val missing: Optional[Text] = Unset

name.or(t"anonymous")     // t"Ada"
missing.or(t"anonymous")  // t"anonymous"
```

The operations read as the questions they answer. `let` transforms a present value and
leaves an absent one alone; `lay` supplies a fallback and a transformation together;
`present` and `absent` ask directly; and `vouch` asserts presence where the surrounding
code has already established it:

```scala
name.let(_.upper)              // Optional[Text]
name.lay(t"nobody")(_.upper)   // Text, either way
name.present                   // true
name.vouch                     // Text, panicking if that was a lie
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

`Optional` cannot nest, because absence has exactly one representation. The compiler
enforces that rather than trusting to discipline: a type that could not be made optional
unambiguously is rejected where it is written, so the ambiguous type never arises to be
flattened away.

## What it costs

Two things, and they are worth stating plainly.

The first is that a *generic* method taking an `Optional[value]` needs evidence that
`value` is a definite type, since an abstract one might later be instantiated to
something already optional. That evidence appears in the signature:

```scala
def firstOrElse[value: Concrete](values: List[Optional[value]], fallback: value): value =
  values.compact.prim.or(fallback)
```

The second is that `Optional` is not a monad and does not pretend to be one. There is no
`flatMap` threading absence through a `for` comprehension. Absence is handled where it
arises — with `or`, `let` or `lay` — which is the point rather than an omission: a value
that might be absent is made definite close to where it was obtained, instead of being
carried through the program in a wrapper that every later step must accommodate.

## Meeting `Option` at the boundary

Other libraries return `Option`, and a Soundness value sometimes has to become one. The
conversion is explicit in both directions and confined to the boundary:

```scala
name.option              // Some(t"Ada"): Option[Text]
Some(t"Ada").optional    // t"Ada": Optional[Text]
```

Converting at the edge rather than adopting `Option` throughout is the same discipline
applied to `null`: a foreign representation is translated once, on arrival, and the
program's interior speaks one language.

See [zero cost](zero-cost.md) for the general principle this is an instance of, and
[impossible states](impossible-states.md) for why the non-nesting matters.
