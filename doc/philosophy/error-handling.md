# Error Handling

Soundness handles errors generically, separating code that can fail from the decision
of what to do when it does. An operation declares in its type the errors it may raise,
and the caller chooses a strategy — throw, recover with a default, accumulate several
failures, or treat the failure as a checked outcome — without the operation needing to
know which. This makes failure visible in the types, with the discipline of checked
exceptions but none of their rigidity, and it lets the same fallible code be used
unsafely while prototyping and then made totally safe later, with no rewrite. The
errors themselves are [expressive, immutable values](expressive-errors.md).

## The separation

An operation says what it can fail with, in its return type, and says nothing about what
should happen:

```scala
def loadConfiguration(path: Path on Linux): Configuration raises IoError raises JsonError
```

The caller supplies the response, as a contextual value. The same body serves every
response, because the body never mentioned one:

```scala
import strategies.throwUnsafely       // throw: fine while prototyping
```

```scala
safely(loadConfiguration(path)).or(Configuration.default)   // fall back
```

```scala
recover:
  case IoError(path, _, _) => Configuration.default
  case error: JsonError    => abort(StartupError(error))
. protect(loadConfiguration(path))                          // handle each case
```

```scala
accrue(Invalid(Nil))((all, error) => all.add(error)):
  case error => ()
. protect(validateEverything(form))                         // collect them all
```

Nothing in `loadConfiguration` changes between these. That is the property the whole
design exists for.

## Why not the alternatives

**Unchecked exceptions** are invisible. A signature says nothing about what might be
thrown, so a caller learns of a failure when it happens, in production, having written no
handler because there was nothing to prompt one.

**Java's checked exceptions** are visible and rigid. The error type is welded into every
signature along the call chain, so adding a failure mode to a leaf function edits every
caller between it and the handler — and because the compiler demands a response at each
step, the pressure is toward `catch (Exception e) {}`, which is worse than either.

**`Either` and its relatives** are visible and honest, but they change the *shape* of the
code: a fallible call cannot sit inside an argument list or an interpolated string, and
combining failure with asynchrony means stacking transformers. See
[direct style](direct-style.md) for why that matters more than it first appears.

The Soundness arrangement takes visibility from the checked-exception tradition, the
ability to choose a response from the functional one, and the shape of ordinary code from
neither.

## Raising, aborting, and the difference

Two verbs, and the distinction is what makes accrual possible.

`abort` stops the computation: there is no value to continue with. `raise` records the
error and continues with a supplied fallback — which is what lets a validation pass check
every field rather than stopping at the first:

```scala
def parseAge(text: Text): Int raises NumberError =
  text.as[Int]                       // aborts if it cannot parse

def lenientAge(text: Text): Int raises NumberError =
  raise(NumberError(text, Unparseable)) yet 0    // records, continues with 0
```

Under an accruing strategy the recorded errors are collected; under a throwing one the
first still throws. The operation states which of the two it means, and the strategy
decides what becomes of it.

## Ventures and guards

A fallback value flows onwards, and downstream code cannot tell it from a real one — a
consistency check fed a placeholder raises spurious *cascade* errors. `venture` and
`guard` make the dependency structure explicit instead:

```scala
def decodePerson(json: Json): Person raises Json.Error =
  val name: Venture[Text] = venture(json.name.as[Text])
  val role: Venture[Role] = venture(json.role.as[Role])

  venture(checkConsistency(name(), role()))

  guard:
    Person(name(), role())
```

A `venture` evaluates immediately — its errors accrue like any others — but remembers
whether anything went wrong. Forcing a failed venture with `name()` skips the enclosing
venture or `guard` block rather than producing a value, so the consistency check above
never runs on garbage; and because `role` was evaluated eagerly at its declaration, its
errors were collected even if `name` failed first. `guard` runs its block only when no
error has been recorded so far. Under a fail-fast strategy both constructs disappear:
an error escapes at the venture itself, and `guard` is the identity — the same body
serves every strategy, which is the point.

## Making the response total

`safely` and `unsafely` are the two escape hatches, and they are deliberately conspicuous.
`unsafely` discharges the obligation without handling it — legitimate at a program's
boundary, or in a test that has already established the value is fine, and a searchable
marker of the places where the guarantee stops.

The intended progression is that a prototype uses `throwUnsafely` throughout, and the
places that matter are converted to real handling as the program is hardened — with no
change to the fallible code itself, because it never depended on which was in scope.

## What it costs

**A capability must be in scope.** A method that calls fallible code must either handle
the failure or declare that it too can fail. That propagation is the mechanism working
correctly, but it means adding a failure mode to a widely-used function does touch the
functions that call it — the same cost checked exceptions have, without the rigidity of
being unable to choose the response.

**Inference has more to do.** `raises` clauses compose through type-level machinery, and
when the compiler cannot work out which error type is meant, the message is about an
implicit search rather than about the error. Importing `explainMissingContext` turns that
into a diagnosis naming the strategy imports that would satisfy it.

**There is a vocabulary to learn.** `raise`, `abort`, `safely`, `unsafely`, `recover`,
`mitigate`, `accrue`, `capture`, `attempt`, `venture`, `guard` — eleven words where
`try`/`catch` has two. Each earns its place, but the learning curve is real and
front-loaded.

See [expressive errors](expressive-errors.md) for what an error value carries, and
[honest signatures](honest-signatures.md) for the wider principle that a signature states
its true requirements.
