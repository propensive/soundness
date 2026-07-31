# Direct Style

Soundness is written in direct style: code reads as a straightforward sequence of
steps, not as a chain of combinators threading a value through a monad. Effects such
as failure, asynchrony, and context are carried by capabilities and by types rather
than by wrapper types that have to be mapped and flat-mapped together. The result
composes as ordinary code composes — with calls, blocks, and local values — so the
shape of a program follows the shape of the problem instead of the shape of an
abstraction laid over it.

The difference shows in composition. Direct-style expressions nest: a fallible call can
sit inside an argument list, inside an interpolated string, inside a condition —
anywhere an expression can go —

```scala
val greeting = t"Hello, ${name.as[EmailAddress].localPart}!"
```

whereas monadic values compose only through their combinators. The same line, with
failure as a wrapper type, must be unrolled into named intermediate steps:

```scala
val greeting = name.decodeEither.map(email => s"Hello, ${email.localPart}!")
```

and each further effect deepens the unrolling. For-comprehensions soften the syntax but
not the constraint: a `for` composes only values of *one* monad, so mixing failure with
asynchrony with iteration demands transformer stacks or manual plumbing, and ordinary
control flow — a `while`, an early return, a `try`/`finally` — has no direct place
inside it. Two monadic libraries with different wrapper types do not compose at all
without adapters.

Direct style dissolves the problem rather than managing it. Effects are contextual
capabilities, so combining them is having both in scope, not nesting their types; every
control structure of the language works unchanged; and the fallible, the asynchronous
and the pure call all look like calls. Where monadic values must be *sequenced*,
direct-style expressions merely *occur* — which is why the composition never stops
being ordinary.

## Ordinary control flow keeps working

The claim is easiest to check on the constructs a monadic encoding has to replace. Each
of these works unchanged around fallible, asynchronous or resource-holding code:

```scala
supervise:
  for url <- urls do                          // an ordinary loop
    if shouldFetch(url) then                  // an ordinary condition
      try async(url.fetch().receive[Text])
      finally record(url)                     // ordinary try/finally
```

No `traverse`, no `whenA`, no bracket combinator — and no question of which monad the
`for` is over, because it is not over one. A `while` loop, an early return, a `match`,
a nested function definition: all of them mean what they mean in Scala.

## Effects still appear in the types

Direct style is not effect-erasure. What an operation needs is in its signature, exactly
as it would be in the monadic encoding — the difference is that it appears as a
*requirement* rather than as a wrapper around the result:

```scala
def fetch(url: HttpUrl)(using Online, Monitor): Text raises HttpError
```

Network access, suspension, and failure are all visible. What is *not* imposed is a
change to the shape of the value: the method returns `Text`, so its result composes with
everything that accepts `Text`, and a caller that has discharged the requirements has an
ordinary value in hand.

This is the property the monadic encoding trades away. `IO[Either[HttpError, Text]]`
carries the same information and cannot be passed to anything expecting `Text`.

## What it costs

**Capabilities must be in scope, and that propagates.** A method calling something that
needs `Online` must either supply it or declare it. This is the same propagation a
monadic encoding has — there it is in the return type — so it is a cost of honesty rather
than of direct style, but it is not zero.

**The mechanism is contextual, and contextual code can surprise.** Which strategy is in
scope determines what a failure does, and a given imported at the top of a file affects
code far below it. That is the intended power — see
[declarative context](declarative-context.md) — but a reader who does not check the
imports can misread what a fallible call will do.

**Some reasoning is genuinely easier with an explicit value.** A monadic value can be
stored, retried, or passed to a combinator that runs it three times. A direct-style
fallible expression is not a value in the same way — which is why `defer` exists, to
hold an unapplied fallible computation where one is genuinely wanted. That it needs a
mechanism at all is an admission that the wrapper had a use.

**It leans on newer language features.** Context functions, capture checking and
`transparent inline` are doing the work, and the second of those is still maturing. The
monadic encoding needs none of them and works on any Scala.

See [delimited scopes](delimited-scopes.md) for how a capability is introduced, and
[error handling](error-handling.md) for the effect this matters most for.
