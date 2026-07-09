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
val greeting = t"Hello, ${name.decode[EmailAddress].localPart}!"
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
