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
carrying a typed [message](expressive-errors.md); a method advertises the errors it can raise
in its return type; and the decision of what to do belongs to the caller, expressed as a
contextual strategy. Because the strategy is an ordinary given, one body of fallible code
serves every response. The names come from the `soundness` package, with a diagnostics choice
that decides whether errors capture stack traces:

```scala
import soundness.*
import errorDiagnostics.stackTracesDiagnostics
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

### Declaring that code can fail

A method that can fail says so with `raises`, naming the error in its return type. Within such
a method, `raise` reports an error, and `abort` reports one and stops:

```scala
def connect(port: Int): Connection raises PortError =
  if available(port) then open(port) else abort(PortError(port))
```

The requirement propagates. A method that calls `connect` must itself either declare `raises
PortError` or handle the failure, so the obligation cannot be dropped silently. A method that
can fail in more than one way names each error with its own `raises`.

### Choosing a strategy

At the point a fallible operation is used, a *strategy* in scope decides what a failure does.
`throwUnsafely` raises an exception:

```scala
import strategies.throwUnsafely

connect(8080)   // returns a Connection, or throws PortError
```

`safely` runs a block and turns any failure into an absent result, so a failure becomes `Unset`
rather than an exception; `unsafely` asserts that no failure will occur; and `capture` returns
the error itself, for testing that the right failure is produced:

```scala
safely(connect(8080))          // Optional[Connection] — Unset on failure
capture[PortError](connect(80)).port   // 80
```

### Recovering

`recover` handles chosen errors by supplying a replacement value, applied to a block with
`protect`:

```scala
recover:
  case PortError(port) => fallbackConnection
. protect:
    connect(8080)
```

`mitigate` instead replaces one error with another, translating a low-level failure into the
error a caller expects before it propagates. Both compose: a `mitigate` inside a `recover`
turns one error into another and then handles it.

### Accumulating failures

By default the first error stops the work, but some tasks — validating a form, parsing a
document — should gather every failure and report them together. `raise` (unlike `abort`)
records an error and continues, and `accrue` folds the recorded errors into one:

```scala
accrue(Invalid(Nil)) { (all, error) => all.add(error) }:
  case error => ()
. protect:
    validateName(form)
    validateEmail(form)
    validateAge(form)
```

Each field is checked even if an earlier one failed, and the accumulated `Invalid` carries all
the problems at once.

### Diagnostics

An `Error` can capture a stack trace or omit it, decided by the `Diagnostics` given in scope:
`stackTracesDiagnostics` records the trace for debugging, while `emptyDiagnostics` drops it,
which is cheaper where an error is expected and handled rather than investigated.
