## Errors

### About

An error in Soundness is a value with a structured message, and the possibility of failure is
written into a method's type. A method that can fail declares the error it may produce with
`raises`, so a caller cannot ignore it: the code must either handle the failure or pass the
obligation on. How to handle it — throw, fall back, collect, recover — is chosen at the call
site, not fixed by the method that fails.

This gives failure the visibility of checked exceptions without their rigidity. The same
fallible operation can be run unsafely while prototyping and made totally safe later, by
changing the strategy in scope rather than rewriting the operation.

### On errors

Two traditions handle failure, and each gives something up. Unchecked exceptions are
invisible: a method's type says nothing about what it might throw, so a caller learns of a
failure only when it happens. Java's checked exceptions make failure visible but rigid — the
error type is welded into every signature along the call chain, and there is one fixed
response. And in both, the error is often reduced to a string, which loses the structure that
would let a program inspect or react to it.

Soundness keeps the failure in the type and the detail in the value. An error is a value
carrying a typed [message](../philosophy/expressive-errors.md); a method advertises the errors it can raise
in its return type; and the decision of what to do belongs to the caller, expressed as a
contextual strategy. Because the strategy is an ordinary given, one body of fallible code
serves every response — which is the whole of the
[error-handling](../philosophy/error-handling.md) principle, and the reason fallible calls
still read as ordinary [direct-style](../philosophy/direct-style.md) code.

The names come from the `soundness` package, with a diagnostics choice that decides whether
errors capture stack traces:

```scala
import soundness.*
import errorDiagnostics.stackTracesDiagnostics
```

The examples that follow revolve around one fallible operation — connecting to a port, which
fails when the port is taken — standing in for any operation that can fail:

```scala
case class Connection(port: Int)

def available(port: Int): Boolean = port > 1024
```

### Defining an error

An error is a case class extending `Error`, with a message written using the `m"…"`
interpolator. The message carries the values that explain the failure, kept as typed fields
rather than baked into a string:

```scala
case class PortError(port: Int)(using Diagnostics)
extends Error(m"the port $port is not available")
```

The `m"…"` interpolator builds a `Message` — a structured value, not a `String` — and its
substitutions are typed, so only a value that knows how to render itself into a message can be
interpolated. That structure is what lets an error be rendered richly, inspected, or matched on
later.

What the structure preserves is the boundary between the fixed and the variable parts of the
message. A `Message` may embed other `Message`s, and those embeddings remain identifiable rather
than being flattened into the surrounding text, so the same message renders as
[Markdown](markdown.md), [HTML](html.md) or [styled terminal text](terminal.md) with the
interpolated values distinguished from the words around them. `text` flattens it to plain text
where that is all that is wanted. Anything with a `Communicable` instance can be embedded, which
covers the primitives, `Text`, and anything `Show`able; writing a `Communicable` instance by hand
is how a type contributes more structure than its plain rendering would.

An error that is not meant to be caught is a `Panic`. It takes a message explaining why the
situation was believed to be impossible, and reaching one means a programmer's understanding was
wrong — so it is not a failure to handle, and does not appear in any `raises` clause.

### Declaring that code can fail

A method that can fail says so with `raises`, naming the error in its return type. Within such
a method, `raise` reports an error, and `abort` reports one and stops:

```scala
def connect(port: Int): Connection raises PortError =
  if available(port) then Connection(port) else abort(PortError(port))
```

The requirement propagates. A method that calls `connect` must itself either declare `raises
PortError` or handle the failure, so the obligation cannot be dropped silently. A method that
can fail in more than one way names each error with its own `raises`.

`raises` is only a syntactic alias: `Connection raises PortError` *is* the context function type
`Tactic[PortError] ?=> Connection`, which in turn is the same thing as a `(using
Tactic[PortError])` parameter. So a method that declares `raises` is a method taking the
error-handling policy as an argument, and `abort` is what calls into it. That is the whole
mechanism, and it explains the shape of everything else here: a fallible method does not decide
how failure is handled, it delegates that decision back to whoever called it — which means one
implementation serves every strategy at once.

### Choosing a strategy

At the point a fallible operation is used, a *strategy* in scope decides what a failure does.
`throwUnsafely` raises an exception:

```scala
import strategies.throwUnsafely

connect(8080)   // Connection(8080)
```

With that strategy, `connect(80)` throws a `PortError` as an ordinary exception.

`safely` runs a block and turns any failure into an absent result, so a failure becomes `Unset`
rather than an exception; `unsafely` asserts that no failure will occur; and `capture` returns
the error itself, for testing that the right failure is produced:

```scala
safely(connect(80))                    // Unset
capture[PortError](connect(80)).port   // 80
```

*Tactic* and *strategy* are the same type, `Tactic`, distinguished only by the scope they are
meant for. A tactic is local — the `protect` block of a `recover` or `mitigate`, or a `safely` or
`unsafely` block — while a strategy is imported once and applies broadly, as `throwUnsafely` does.
The distinction is a matter of intent rather than of implementation, and it is worth naming
because the two words appear throughout, and in the `strategies` package the choice is deliberate.

Two further declarations widen the choice beyond a scope. An error marked `Unchecked` may be
thrown without being handled — it is a marker typeclass with no members, so the given can be
`erased`:

<!-- doccheck: skip -->
```scala
case class AsciiError(char: Char)(using Diagnostics)
extends Error(m"the character $char is not ASCII")

erased given AsciiError is Unchecked
```

An error declared `Fatal` ends the process, and says with what status. This suits failures during
initialization, where there is no meaningful way to continue:

```scala
case class InitError(reason: Text)(using Diagnostics)
extends Error(m"initialization failed: $reason")

given InitError is Fatal = error => Exit.Fail(127)
```

### Recovering

`recover` handles chosen errors by supplying a replacement value, applied to a block with
`protect`:

```scala
recover:
  case PortError(port) => Connection(8080)
. protect:
    connect(80)
// Connection(8080)
```

A handler matching a union of error types registers for each member, so one `recover` can
answer several kinds of failure. `mitigate` instead replaces one error with another, translating
a low-level failure into the error a caller expects before it propagates. Both compose: a
`mitigate` inside a `recover` turns one error into another and then handles it:

<!-- doccheck: skip -->
```scala
case class ServiceError(detail: Text)(using Diagnostics)
extends Error(m"the service could not start: $detail")

def start(port: Int): Connection raises ServiceError =
  mitigate:
    case PortError(port) => ServiceError(t"port $port unavailable")
  . protect:
      connect(port)

capture[ServiceError](start(80)).detail   // t"port 80 unavailable"
```

### Accumulating failures

By default the first error stops the work, but some tasks — validating a form, parsing a
document — should gather every failure and report them together. `raise` (unlike `abort`)
records an error and continues, and `accrue` folds the recorded errors into one:

```scala
case class Invalid(problems: List[Text])(using Diagnostics)
extends Error(m"${problems.size} problems")

case class Form(name: Text, email: Text, age: Int)

def validateName(form: Form): Unit raises PortError =
  if form.name == t"" then raise(PortError(1))

def validateAge(form: Form): Unit raises PortError =
  if form.age < 0 then raise(PortError(2))

val form = Form(t"", t"nobody", -1)

capture[Invalid]:
  accrue(Invalid(Nil)) { (all, error) => Invalid(all.problems :+ error.message.text) }
    { case error: PortError => () }
  . protect:
      validateName(form)
      validateAge(form)
. problems.size   // 2
```

Each field is checked even if an earlier one failed, and the accumulated `Invalid` carries all
the problems at once. The first block folds each raised error into the accumulation; the second
says which errors the accrual handles.

The difference between the two is in their return types. `abort` returns `Nothing`: there is no
value to give back, so it leaves. `raise` returns a value — an *ersatz* value, a stand-in — so
execution can carry on locally while the error is registered against the tactic. Once anything
has been raised, the final result of the enclosing scope is discarded whatever it turns out to
be, and the aggregation of the recorded errors is produced instead.

That discarding is what makes ersatz values safe, but only as far as it goes: it covers the
result, not the side effects. An ersatz value should be *inconsequential* — nothing downstream
should depend on it, and in particular it should not provoke further errors of its own. A
placeholder that flows into a validating constructor produces a complaint about a value nobody
ever supplied, which is worse than no report at all.

### Ventures and guards

`venture` and `guard` remove that hazard by making the dependency structure explicit rather than
hoping ersatz values stay harmless.

`venture(…)` evaluates its block immediately, under the ambient tactic, so every error it raises
accrues exactly as it would outside. What it yields is a `Venture[Value]`: the computed value if
the block recorded nothing, or the `Failed` marker if it did. An `abort` inside a venture is
*delimited* — it registers its error and abandons only that venture, not the whole aggregation
scope — so sibling ventures each contribute their full error set independently:

```scala
import dynamicAccess.dynamicJson

case class Person(name: Text, age: Int)

def decodePerson(json: Json): Person raises Json.Error =
  val name = venture(json.name.as[Text])
  val age = venture(json.age.as[Int])

  venture:
    if name().length > age() then abort(Json.Error(Json.Error.Reason.Absent))

  guard:
    Person(name(), age())
```

Forcing a venture with `name()` requires a *skip-scope* in context — an enclosing `venture` or
`guard`, witnessed by a contextual `Guard` capability. Forcing a failed venture escapes to that
scope without registering anything further, since its errors are already in the accrual: above,
if `name` failed then the consistency check is skipped entirely, while `role`, evaluated eagerly
at its declaration, still contributed its errors. Forcing outside any skip-scope is a compile
error, because there would be nowhere well-defined to skip to.

`guard` runs its block only if the ambient tactic is *clean* — nothing recorded so far in the
aggregation scope. If the tactic is tainted, the block is skipped and the scope surrenders to the
errors it has already gathered.

None of this costs anything when accrual is not in play: under a fail-fast tactic any error has
already escaped, so `guard` is the identity and `venture` is transparent eager evaluation.

### Saying where a failure was

An accumulated list of errors is only useful if each one says where it came from. A *focus* is the
position within a structure that the code is currently working on, and `track` maintains it as a
traversal descends. Under `validate`, a recorded error is paired with the focus at the moment it
was raised:

```scala
Pointer(t"a")(t"b")(t"c").text   // t"a.b.c"
```

The pointer type is chosen by the structure being traversed — a `JsonPointer` for
[JSON](json.md), a path for [XML](xml.md), a column for [CSV](csv.md) — so the position is
reported in the vocabulary of the data rather than as an index into something internal. This is
the machinery beneath every "which field failed" report in the format modules.

### Inspecting a failure directly

Three related operations turn a failure into a value rather than a control-flow event.

`capture` runs a block that is expected to fail and returns the error, which is what a test
asserting on a failure needs. A block that succeeds is itself an error — an `Expectation.Error` —
since the test was checking the wrong thing:

```scala
capture[PortError](connect(80)).port   // 80
```

`attempt` makes no such demand, returning an `Attempt` that is either a success carrying the value
or a failure carrying the error, for code that must branch on both:

```scala
attempt[PortError](connect(80))   // Attempt.Success(…) or Attempt.Failure(…)
```

`amalgamate` combines several fallible computations, gathering their errors into one.

### Translating and escalating

`Mitigable` is the typeclass behind `mitigate`: an instance says how one error type becomes
another, and importing `strategies.mitigation` applies it automatically where a caller expects the
outer type. A low-level failure therefore reaches an API boundary already translated into that
boundary's vocabulary, without every call site restating the translation.

Two further strategies sit at the ends of the range. `throwUnsafely` throws, with no capability
required, which suits prototyping and scripts. `throwSafely` also throws, but demands a `CanThrow`
capability, so the possibility remains visible in the types. Between them sit the recovering,
accruing and optional strategies, and the choice is always at the call site.

### Diagnostics

An `Error` can capture a stack trace or omit it, decided by the `Diagnostics` given in scope:
`stackTracesDiagnostics` records the trace for debugging, while `emptyDiagnostics` drops it,
which is cheaper where an error is expected and handled rather than investigated.

That choice is available because none of this is built on throwing. `abort` and `raise` use
Scala's `boundary` and `break`, so leaving a computation with an error does not construct a stack
trace unless one was asked for, and an error is no more expensive to build than any other
immutable value. Failure being a normal part of a program's behavior rather than an exceptional
one, it should not cost more than the code that succeeds.

### When a contextual value is missing

Much of the above relies on a given being in scope, and the compiler's ordinary report of a
missing one — "No given instance of type … was found" — says only that the search failed, not
where. With `import soundness.*` a diagnostic hook is in scope that explains the failure instead:
the message names the value being resolved, the candidate instances that were considered, and for
each the nested requirement that could not be met, as a tree. A decoder that fails to derive
because one field's type has no instance is therefore reported at that field, and a strategy
missing from a fallible call is named as the strategy, with the choice packages that supply one.
The hook does nothing when resolution succeeds, so it costs nothing in a compiling program.
