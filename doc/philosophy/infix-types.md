# Infix Types

Soundness uses infix types throughout, so that a type reads as a phrase rather than a
tangle of brackets. Written as prepositions and verbs — `Path on Linux`, `Html of
"br"`, `Text is Decodable in Json`, `Quantity is Multiplicable by Duration`, a method
that `raises HttpError` or `logs ExecEvent` — they let a signature say in plain words
what a value is and what an operation needs, which is part of making code read like
[elegant prose](elegant-prose.md). Their real power is that they
[compose](composability.md): each adds one more clause to a type without nesting, so
`Element of "ul" over "li" in Whatwg` builds up exactly as an English description
would, and the same `of`, `in`, `by`, or `over` carries the same meaning wherever it
appears. A reader who has understood one such type can read the next by analogy, and a
writer can describe a precise type by naming its parts in turn rather than assembling a
deeply parameterised one.

## How they work

Each preposition is a type alias that refines one type member. `in` sets `Form`, `on`
sets `Plane`, `by` sets `Operand`, `to` sets `Result`:

```scala
infix type in   [refined, form]                             = refined { type Form = form }
infix type on   [refined <: { type Plane }, plane]          = refined { type Plane = plane }
infix type by   [refined <: { type Operand }, operand]      = refined { type Operand = operand }
infix type to   [refined <: { type Result }, result]        = refined { type Result = result }
```

That is the whole mechanism. Because each refines a *different* member, they can be
applied in succession without nesting, and the order does not matter:

```scala
Text is Decodable in Json
value is Streamable by Data over Credit
Element of "ul" over "li" in Whatwg
```

The set is small and fixed — `in`, `on`, `of`, `by`, `to`, `from`, `over`, `across`,
`onto`, `against`, `under`, `at` — and each keeps its meaning everywhere. `over` is
always the transport, `by` is always the operand, `on` is always the plane.

## What the alternative costs

The same types, written conventionally:

```scala
Decodable[Text, Json]
Streamable[value, Data, Credit]
Element["ul", "li", Whatwg]
```

Three problems, each of which the infix form avoids.

**Position carries the meaning.** In `Element["ul", "li", Whatwg]` a reader must know
that the first parameter is the tag, the second the permitted children and the third the
specification. In `Element of "ul" over "li" in Whatwg` the prepositions say so.

**Every parameter must be supplied.** A conventional type constructor takes all its
arguments or none, so a partially-specified type needs a wildcard for each unmentioned
position. The infix form mentions only what it constrains: `Text is Decodable` is a
perfectly good type, and `in Json` narrows it.

**Extending is a breaking change.** Adding a fourth type parameter to `Element[…]`
invalidates every use. Adding a fourth refinement adds a preposition that existing code
need not mention.

## Where they read badly

The convention is not free of failure modes, and two are worth knowing.

A type with many clauses can run long, and a long infix type wraps awkwardly across
lines — `(value is Streamable by Data over Credit) { type Transport = Credit }` is not
prose by anyone's standard. Where that happens, the answer is usually a named alias
rather than a longer phrase.

And a preposition is only as good as the analogy it invites. `over` meaning "transport"
reads naturally in `Stream[Data] over Credit` and less naturally in `Instant over Unix`,
where the timeline is not transporting anything. The vocabulary is small enough that such
strains are visible, and they are accepted rather than hidden — the alternative is a
larger vocabulary in which each word is used less often and remembered less well.

## What it costs

Infix types are unfamiliar. A reader arriving from ordinary Scala has met `A => B` and
`A & B` and not much else in infix position, so `Text is Decodable in Json` looks like
several things before it looks like a type. That cost is front-loaded and paid once.

Error messages are the more persistent cost: the compiler reports the *desugared*
refinement, so a mismatch is described in terms of `{ type Form = Json }` rather than
`in Json`, and the reader must perform the translation the syntax was meant to spare
them.

See [composability](composability.md) for the property this is in service of, and
[elegant prose](elegant-prose.md) for the reading experience it aims at.
