## Errors

### About

An error is a value with a structured message, and the possibility of failure is written into a
method's type. A method that can fail declares the error with `raises`, and a caller cannot
ignore the declaration: its code must either handle the failure or declare it in turn.

*How* a failure is handled is chosen where the method is called, not where it fails. The caller
may have it thrown, replaced with a fallback, collected with others, or recovered from.

This gives failure the visibility of checked exceptions without their rigidity. The same
fallible operation can be run carelessly in a prototype and made completely safe later, by
changing the [strategy] in scope, without rewriting the operation.

### On errors

There are two traditions of handling failure, and each gives something up:

```tabular
column  Tradition
column  A failure is
column  What is lost
row  unchecked exceptions  invisible in a method's type  the caller learns of a failure only when it happens
row  Java's checked exceptions  visible in every signature along the call chain  flexibility: there is one fixed response, to throw
```

In both, the error is often reduced to a string, which loses the structure that would let a
program inspect it or react to it.

Here the failure stays in the type, and the detail stays in the value:

- An error is a value which carries a typed [message](../philosophy/expressive-errors.md).
- A method advertises the errors it can raise in its return type, which makes for an
  [honest signature].
- The decision about what to do belongs to the caller, and is expressed as a
  [contextual value] called a strategy.

Because the strategy is an ordinary [given], one body of fallible code serves every response.
That is the whole of the [error-handling](../philosophy/error-handling.md) principle, and it is
why fallible calls still read as ordinary [direct style](../philosophy/direct-style.md) code.

Everything comes from the `soundness` package. One more choice decides whether an error records
a stack trace, and the last import lets the samples print their results:

```scala
import soundness.*
import errorDiagnostics.stackTracesDiagnostics
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

The samples revolve around one fallible operation: connecting to a port, which fails when the
port is not available. It stands in for any operation which can fail:

```scala
case class Connection(port: Int)

def available(port: Int): Boolean = port > 1024
```

### Defining an error

An error is a case class which extends `Error`. Its message is written with the `m"…"`
[interpolator], and the values which explain the failure are kept as typed fields:

```scala
case class PortError(port: Int)(using Diagnostics)
extends Error(m"the port $port is not available")
```

The `m"…"` interpolator builds a `Message`, which is a structured value and not a `String`. Its
substitutions are typed: only a value which knows how to appear in a message can be
substituted.

A `Message` keeps the boundary between its fixed words and its substituted values. A message
may embed other messages, and each embedding stays identifiable instead of being flattened into
the surrounding text. The same message can therefore be rendered as [Markdown](markdown.md),
[HTML](html.md) or [styled terminal text](terminal.md), with the substituted values set apart
from the words around them. `text` flattens it to plain text where that is all that is wanted.

Anything with a `Communicable` instance can be embedded. That covers the primitive types,
`Text`, and any type with a `Showable` instance. A type contributes more structure than its
plain rendering by having a `Communicable` instance written for it.

An error which is not meant to be caught is a `Panic`. It takes a message explaining why the
situation was believed to be impossible. Reaching a panic means a programmer's understanding
was wrong, so it is not a failure to handle, and it appears in no `raises` clause.

### Declaring that code can fail

A method which can fail says so with [`raises`][raises], naming the error in its return type.
Within such a method, `raise` reports an error, and `abort` reports one and stops:

```scala
def connect(port: Int): Connection raises PortError =
  if available(port) then Connection(port) else abort(PortError(port))
```

The requirement propagates. Code which calls `connect` must either declare `raises PortError`
itself, or handle the failure. Code which does neither does not compile:

```scala fuel
compile
errors show
##
val connection = connect(80)
```

A method which can fail in more than one way names each error with its own `raises`.

`raises` is only an alias. `Connection raises PortError` *is* the context function type
`Tactic[PortError] ?=> Connection`, which is in turn the same as a parameter
`(using Tactic[PortError])`. So a method which declares `raises` is a method that takes the way
its errors are handled as an argument, the [tactic], and `abort` is what calls it.

That is the whole mechanism, and it explains the shape of everything below. A fallible method
does not decide how failure is handled. It hands that decision back to whoever called it, and so
one implementation serves every strategy at once.

### Choosing a strategy

Where a fallible operation is used, a *strategy* in scope decides what a failure does. The
`throwUnsafely` strategy throws the error as an ordinary exception:

```scala
import strategies.throwUnsafely
```

```scala fuel
run
##
Out.println(connect(8080).inspect)
```

```scala fuel
run
errors show
##
connect(80)
```

A strategy applies to everything in its scope. Other ways of handling a failure apply to one
block. `safely` runs a block and turns any failure into `Unset`, giving an
[optional](optional-values.md) value. `capture` gives the error itself, which is what a test
needs to check that the right failure is produced:

```scala fuel
run
##
Out.println(safely(connect(8080)).inspect)
Out.println(safely(connect(80)).inspect)
Out.println(capture[PortError](connect(80)).port.show)
Out.println(capture[PortError](connect(80)).message.show)
```

These are three of the ways to run fallible code. All of them, including those described in the
sections below, compare as follows:

```tabular
column  Name
column  Used as
column  A failure
row  `throwUnsafely`  a strategy, imported from `strategies`  is thrown, with no capability required
row  `throwSafely`  a strategy, imported from `strategies`  is thrown, given a `CanThrow` capability
row  `safely`  a block  becomes `Unset`
row  `unsafely`  a block  is thrown, the block asserting that none will occur
row  `capture`  a block  is returned; a success is an `Expectation.Error`
row  `attempt`  a block  is returned in an `Attempt`, beside a success
row  `amalgamate`  a block  is returned in place of the value, as a union
row  `tolerate`  a method of the ambient tactic  becomes `Unset`, and what was recorded is rolled back
```

*Tactic* and *strategy* are the same type, `Tactic`. They are told apart only by the scope they
are meant for. A tactic is local: the `protect` block of a `recover` or `mitigate`, or a
`safely` or `unsafely` block. A strategy is imported once and applies broadly, as
`throwUnsafely` does. The difference is one of intent, not of implementation, but both words
appear throughout the documentation, and the name of the `strategies` package is deliberate.

#### Unchecked and fatal errors

Two further declarations change what an error demands, wherever it is raised. An error marked
`Unchecked` may be thrown without being handled. `Unchecked` is a [typeclass] with no members,
so its given can be `erased`:

```scala fuel
typecheck
##
case class AsciiError(char: Char)(using Diagnostics)
extends Error(m"the character $char is not ASCII")

erased given AsciiError is Unchecked = new Unchecked { type Self = AsciiError }
```

An error declared `Fatal` ends the process, and says with what exit status. This suits a failure
during initialization, where there is no meaningful way to continue:

```scala
case class InitError(reason: Text)(using Diagnostics)
extends Error(m"initialization failed: $reason")

given InitError is Fatal = error => Exit.Fail(127)
```

The four kinds of failure differ in whether they must be handled, and in what happens when one
occurs:

```tabular
column  A failure which is
column  Must be handled
column  When it occurs
row  an `Error`  yes, or declared with `raises`  the tactic in scope decides
row  a `Panic`  no  it is not meant to be caught
row  an `Error` with an `Unchecked` given  no  it is thrown, under `strategies.uncheckedErrors`
row  an `Error` with a `Fatal` given  no  the process ends with the given's status, under `strategies.fatalErrors`
```

### Recovering

`recover` handles chosen errors by supplying a replacement value. It is applied to a block with
`protect`:

```scala
val fallback =
  recover:
    case PortError(port) => Connection(8080)
  . protect:
      connect(80)
```

```scala fuel
run
##
Out.println(fallback.inspect)
```

A handler which matches a union of error types handles each member, so one `recover` can answer
several kinds of failure.

`mitigate` replaces one error with another. It translates a low-level failure into the error a
caller expects, before the failure propagates:

```scala
case class ServiceError(detail: Text)(using Diagnostics)
extends Error(m"the service could not start: $detail")

def start(port: Int): Connection raises ServiceError =
  mitigate:
    case PortError(port) => ServiceError(t"port $port unavailable")
  . protect:
      connect(port)
```

```scala fuel
run
##
Out.println(capture[ServiceError](start(80)).message.show)
```

The two compose. A `mitigate` inside a `recover` turns one error into another, and then handles
it.

#### Inside an inline method

`recover` cannot be used in the body of an `inline def`. No inline call is expanded while such
a body is typed, so the handler's error types are not yet known where `protect`'s block needs
them. There, `deferRecover` names the error types as a type argument, and is otherwise the same:

```scala fuel
typecheck
##
inline def reconnect(inline port: Int): Connection =
  deferRecover[PortError]:
    case PortError(_) => Connection(8080)
  . protect:
      connect(port)
```

`deferMitigate`, `deferHandle` and `deferAccrue` do the same for `mitigate`, `handle` and
`accrue`.

### Accumulating failures

By default the first error stops the work. Some tasks should instead gather every failure and
report them together: validating a form, or parsing a document. `raise`, unlike `abort`, records
an error and continues, and `accrue` folds the recorded errors into one:

```scala
import dysasymptotics.linearSize

case class Invalid(problems: List[Text])(using Diagnostics)
extends Error(m"${problems.size} problems")

case class Form(name: Text, email: Text, age: Int)

def validateName(form: Form): Unit raises PortError =
  if form.name == t"" then raise(PortError(1))

def validateAge(form: Form): Unit raises PortError =
  if form.age < 0 then raise(PortError(2))

val form = Form(t"", t"nobody", -1)

def collect(all: Invalid, error: Exception): Invalid = error match
  case error: PortError => Invalid(all.problems :+ error.message.text)
  case _                => all

val invalid =
  capture[Invalid]:
    accrue(Invalid(Nil))(collect) { case error: PortError => () }
    . protect:
        validateName(form)
        validateAge(form)
```

Both fields are checked, though the first has already failed, and the one `Invalid` carries
every problem:

```scala fuel
run
##
invalid.problems.each(Out.println(_))
```

The two validators raise `PortError` only to keep the sample to one error type. A real form
would have an error of its own. In the call to `accrue`, the function `collect` folds each
raised error into the accumulation, and the final block says which errors are handled.

`abort` and `raise` differ in their return types. `abort` returns `Nothing`: it has no value to
give back, so it leaves. `raise` returns a value, called an *ersatz* value, which is a stand-in
that lets execution carry on locally while the error is recorded against the tactic. Once
anything has been raised, the final result of the enclosing scope is discarded, whatever it
turns out to be, and the accumulated errors are produced in its place.

Discarding the result is what makes ersatz values safe, but it covers only the result, not side
effects. An ersatz value should be *inconsequential*: nothing downstream should depend on it,
and above all it should not provoke further errors of its own. A placeholder which flows into a
validating constructor produces a complaint about a value nobody ever supplied, which is worse
than no report at all.

### Ventures and guards

`venture` and `guard` remove that hazard, by making explicit which computations depend on which.

`venture(…)` evaluates its block immediately, under the ambient tactic, so every error it raises
accumulates exactly as it would outside. Its result is a `Venture[Value]`: the computed value if
the block recorded nothing, or the marker `Failed` if it did. An `abort` inside a venture is
*delimited*. It records its error and abandons only that venture, not the whole scope, so
sibling ventures each contribute all their errors independently:

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

`name()` *forces* a venture, which gives its value. Forcing is allowed only inside an enclosing
`venture` or `guard`, called a *skip-scope*. Forcing a failed venture escapes to that scope, and
records nothing more, since the venture's errors have already been recorded. In the sample, if
`name` failed then the consistency check is skipped entirely, while `age`, which was evaluated
at its declaration, has still contributed its errors. Forcing a venture outside any skip-scope
is a compiletime error, because there would be nowhere well-defined to skip to.

`guard` runs its block only if the ambient tactic is *clean*, with nothing recorded so far. If
anything has been recorded, the block is skipped, and the scope gives up in favor of the errors
already gathered.

None of this costs anything when errors are not being accumulated. Under a tactic which stops at
the first error, any error has already escaped, so `guard` simply runs its block and `venture`
simply evaluates its own.

[JSON](json.md#collecting-every-error) uses this machinery to report every fault in a document
at once.

### Tolerating a failure

A tactic can also run a block and discard whatever it raises. `tolerate` gives the block's
value, or `Unset` if the block recorded an error or aborted. Unlike `safely`, it works *within*
the ambient tactic instead of installing a new one, so an accumulating tactic rolls back what
the block recorded:

```scala fuel
typecheck
##
def tolerant(json: Json): Optional[Int] raises Json.Error =
  summon[Tactic[Json.Error]].tolerate(json.as[Int])
```

This is how a codec decodes an optional field leniently. The field's decoder was bound to the
ambient tactic when it was resolved, so the tactic itself is the only place where a fault in it
can be intercepted.

Together, these are the operations for reporting and containing a failure while errors are
being accumulated:

```tabular
column  Operation
column  Yields
column  After an error
row  `abort(error)`  `Nothing`  leaves the scope, or only the venture it is in
row  `raise(error)`  an ersatz value  continues, with the error recorded
row  `venture(…)`  a `Venture[Value]`, `Failed` if the block recorded anything  its siblings still run
row  `guard(…)`  the block's value  the block is skipped if anything was recorded before it
row  `tactic.tolerate(…)`  an `Optional` value, `Unset` on failure  what the block recorded is rolled back
```

### Saying where a failure was

A list of accumulated errors is useful only if each one says where it came from. A *focus* is
the position, within a structure, which the code is currently working on, and `track` maintains
it as a traversal descends. Under `validate`, a recorded error is paired with the focus at the
moment it was raised:

```scala fuel
run
##
Out.println(Pointer(t"a")(t"b")(t"c").text)
```

The type of the pointer is chosen by the structure being traversed, so a position is reported
in the vocabulary of the data, not as an index into something internal:

```tabular
column  Format
column  Focus
column  Locates by
row  [JSON](json.md)  `Json.Focus`  a `JsonPointer`
row  [XML](xml.md)  `Xml.Focus`  an `XPath`
row  [YAML](yaml.md)  `Yaml.Focus`  a `YamlPath`
row  [TEL](tel.md)  `Tel.Focus`  a `Telp`
row  [CSV](csv.md)  `CellRef`  a row and a column name
```

This is the machinery beneath every report of "which field failed" in the data formats.

### Inspecting a failure directly

Three related operations turn a failure into a value.

`capture` runs a block which is expected to fail, and gives the error. A block which succeeds is
itself an error, an `Expectation.Error`, since the code was checking for the wrong thing:

```scala fuel
run
errors show
##
capture[PortError](connect(8080))
```

`attempt` makes no such demand. It gives an `Attempt`, which is either a success carrying the
value or a failure carrying the error, for code which must branch on both:

```scala
def describe(port: Int): Text = attempt[PortError](connect(port)) match
  case Attempt.Success(connection) => t"connected on port ${connection.port}"
  case Attempt.Failure(error)      => t"failed: ${error.message}"
```

```scala fuel
run
##
Out.println(describe(8080))
Out.println(describe(80))
```

`amalgamate` gives either the block's value or the first error it raises, typed as the union of
the two.

[Testing](testing.md) uses `capture` to assert that code fails in the way it should.

### Translating and escalating

`Mitigable` is the typeclass behind `mitigate`. An instance says how one error type becomes
another, and importing `strategies.mitigation` applies it automatically wherever a caller
expects the outer type. A low-level failure therefore reaches the boundary of an API already
translated into that API's vocabulary, without each call restating the translation.

Two strategies sit at the ends of the range. `throwUnsafely` throws, with no capability
required, which suits prototypes and scripts. `throwSafely` also throws, but demands a
`CanThrow` [capability], so the possibility stays visible in the types. Between them sit the
recovering, accumulating and optional forms, and the choice is always made by the caller.

### Diagnostics

An `Error` can record a stack trace or omit it. The `Diagnostics` given in scope decides:

```tabular
column  Import
column  An error
row  `errorDiagnostics.stackTracesDiagnostics`  records a stack trace, for debugging
row  `errorDiagnostics.emptyDiagnostics`  records none, which is cheaper where an error is expected and handled
```

The choice exists because none of this is built on throwing. `abort` and `raise` use Scala's
`boundary` and `break`, so leaving a computation with an error constructs no stack trace unless
one was asked for, and an error is no more expensive to build than any other immutable value.
Failure is a normal part of a program's behavior, so it should not cost more than success.
[Stack traces](stack-traces.md) describes how a recorded trace is read and displayed.

### When a contextual value is missing

Much of the above relies on a given being in scope. The compiler's ordinary report of a missing
one, "No given instance of type … was found", says only that the search failed, not where. With
`import soundness.*`, a diagnostic hook is in scope which explains the failure instead. Its
message names the value being resolved and the candidate instances which were considered, and
shows as a tree the nested requirement each of them could not meet.

A decoder which cannot be [derived](derivation.md#when-derivation-fails) because one field's
type has no instance is therefore reported at that field. A strategy missing from a fallible
call is named as the strategy, with the choice packages which supply one. The hook does nothing
when resolution succeeds, so it costs nothing in a program that compiles.
