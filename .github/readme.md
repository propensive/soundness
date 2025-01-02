[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/contingency/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/contingency/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Contingency

__Versatile error handling for every scenario.__

__Contingency__ is an experimental library for abstracting over error handling
strategies. In particular, it gives developers a choice between throwing
exceptions, returning errors in a variety of datatypes, and accumulating
several validation-style errors. Code must be written to accomodate
Contingency's generic error handling, but the changes from exception-throwing
code are trivial.

## Features

- error are checked as _capabilities_
- choose global and localized strategies for error-handling
- fully-typesafe error handling
- selectively ignore errors considered "impossible"
- aggregate multiple errors, like a _validation_
- recover from specific errors with success values
- mitigate specific errors into more general errors


## Availability

Contingency has not yet been published. The medium-term plan is to build it with
[Fury](https://github.com/propensive/fury) and to publish it as a source build
on [Vent](https://github.com/propensive/vent). This will enable ordinary users
to write and build software which depends on Contingency.

Subsequently, Contingency will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).




## Getting Started

All Contingency terms and types are defined in the `contingency` package:
```amok
syntax  scala
##
import contingency.*
```
and are exported to `soundness`. So alternatively,
```amok
syntax  scala
##
import soundness.*
```

_Contingency_ provides a number of different strategies and tactics for
handling errors in Scala, for libraries which opt into its advanced
capabilities.

Contingency's approach builds upon the new `boundary`/`break` infrastructure in
Scala 3 to provide comprehensive errors-handling functionality which is:
 - composable: write in a direct-style, and compose expressions seamlessly
 - typesafe: error handling is statically checked
 - performant: avoid costly construction of stack traces
 - versatile: choose different tactics for different circumstances, or global
   strategies

## Examples

Here is a quick tour of how error handling in Scala can be versatile,
composable, typesafe and performant with Contingency.

Let's start by declaring a partial method with a `raises` clause in its return
type, and abort under certain conditions:
```amok
syntax  scala
transform
  replace   Bytes  Bytes raises AsciiError
##
def convert(message: Text): Bytes =
  if message.exists(_.toInt > 127) then abort(AsciiError())
  message.bytes
```

We cannot call that method unless its `AsciiError` is handled in some way. This
code will _not_ compile:
```amok
syntax  scala
##
val data = convert(t"Hello world")
```

One solution is to import a _strategy_ to handle any possible errors by
throwing them. This works well for code that is still at the "prototype" stage
of development:
```amok
syntax  scala
##
import strategies.throwUnsafely
val data = convert(t"Hello world")
```

But we can get the same effect more _locally_ by wrapping the invocation in
`unsafely`,
```amok
syntax  scala
##
val data: Bytes = unsafely(convert(t"Hello world"))
```
or `safely`,
```amok
syntax  scala
##
val data: Optional[Bytes] = safely(convert(t"Hello world"))
```
which will return the optional `Unset` value in the event of an error—the
error object itself will be discarded, though.

In some circumstances, we might decide that it is acceptable to _throw_ an
`AsciiError` (without saying anything about other error types), and we can do
so by by declaring an `Unchecked` instance for it, like so:
```amok
syntax  scala
##
erased given AsciiError is Unchecked
```

(This is only a "marker" typeclass, so it can be `erased`.)

We could go further and declare `AsciiError`s as "fatal", shutting down the
entire JVM upon the first occurrence,
```amok
syntax  scala
##
given AsciiError is Fatal = _ => ExitStatus.Fail(1)
val data: Bytes = convert(t"Hello world")
```
though this might be more typical for errors that are raised during
initialization.

Now imagine we want to combine three methods, `Json.parse`, `Json#as` and
`convert`, with signatures,
```amok
syntax  scala
##
object Json:
  def parse(text: Text): Json raises ParseError

class Json():
  def as[ResultType]: ResultType raises AccessError

// implementation details not shown
```
in a single method, `processEvent`. We would be required to handle
`ParseError`s, `AccessError`s and `AsciiError`s. We could write,
```amok
syntax  scala
##
def processEvent(event: Text)
        : Bytes raises ParseError raises AccessError raises AsciiError =
  convert(Json.parse(event).as[Event].message)
```
but multiple `raises` clauses are cumbersome: not only does the method need to
declare each error type, any method which invokes it must also handle _all_ of
these errors.

Instead, we can _tend_ them into an `EventError`:

```amok
syntax  scala
##
def eventData(event: Text): Bytes raises EventError =
  tend:
    case ParseError()  => EventError()
    case AccessError() => EventError()
    case AsciiError()  => EventError()
  .within:
    convert(Json.parse(event).as[Event].message)
```

This has the effect that any exception matching one of the `tend` cases will
be transformed into the right-hand side of the case—in this case, a new
`EventError`.

This may be more typical for production code. But note how the main expression,
`sendAscii(Json.parse(event).as[Event].message)`, remains the same as it would
if we had used the `throwUnsafely` strategy. This is the beauty of direct-style
Scala: the "happy path" can be written with the same elegance, concision and
aesthetics, even after enhancing the safety of the code.

Using alternative definitions of `ParseError`, `AccessError`, `AsciiError` and
`EventError`s as immutable datatypes _with parameters_, we could channel details
from one type to the other, like so:

```amok
syntax  scala
##
def processEvent(event: Text): Unit raises EventError =
  tend:
    case ParseError(line) => EventError(m"invalid JSON at line $line")
    case AccessError(key) => EventError(m"key $key was missing")
    case AsciiError()     => EventError(m"the message contained invalid ASCII")
  .within:
    send(convert(Json.parse(event).as[Event].message))
```

In this example, every error on the right-hand side has the same type, and while
that is a common use-case, it's not a requirement. The only constraint is that
the type of each right-hand side case is (independent of the other cases) an
`Exception` type that has a handler. In this example, the `raises EventError`
in the return type ensures that handler.

Sometimes, however, in the event of certain errors, we want to _return_ a
value—some sort of "fallback" value—instead of continuing along an
error-recovery path. For this, we can use `mend` instead of `tend`, and the
right-hand side of each case will represent the return value:
```amok
syntax  scala
##
def processEvent(event: Text): Unit raises EventError = send:
  mend:
    case ParseError(_)  => Bytes(0, 1)
    case AccessError(_) => Bytes(0, 2)
    case AsciiError(_)  => Bytes(0, 3)
  .within:
    convert(Json.parse(event).as[Event].message)
```

Here, we _mend_ the subexpression
`convert(Json.parse(event).as[Event].message)` and produce a two-byte message
(such as `Bytes(0, 1)`, which could be a representation of the failure). Either
this, or the successful evaluation of the subexpression will be passed to the
`send` method.

## Core concepts

A _partial method_ is a method which may not produce a result for certain
inputs, i.e. it is not _total_. Partial methods are already familiar in Scala
(and Java), but the way they handle the _absence_ of a result is to
throw a traditional exception, determined directly or indirectly by the code in
the method's implementation.

Note that Functional Programming requires all functions to be _total_. (If they
are not, then they're not even considered functions.) Partiality can be
encoded in this strict definition of a function in a variety of ways, but
invariably they return, in their _total_ encoding, values representing the
absence of a return value in their unencoded _partial_ form.

Without exceptions, if it's not possible to return a successful value from a
method, we need to "abort" execution somehow, or return a "non-value". Here's a
trivial example of a method where that's necessary,
```amok
syntax  scala
##
def second[ElementType](list: List[ElementType]): ElementType =
  if list.length >= 2 then list(1) else ???
```
where `???` indicates the code we are unable to implement.

Contingency provides the `abort` method for indicating a failure, which can be
thought of as similar to the `throw` keyword: it "escapes" from the current
method with an error, instead of returning a value.

In the implementation above, we can replace `???` with `abort(TooShort(2))`,
assuming a `TooShort` exception type such as:
```amok
syntax  scala
##
case class TooShort(minimum: Int)
extends Exception(s"A minimum length of $minimum is required")
```

Note that the error type must be a subtype of `Exception` because some
strategies may need to throw it.

However, we can _only_ call `abort` with an error if we have a `Tactic` in
scope for its error type. In this case, we _must_ have a contextual
`Tactic[TooShort]` available.

One way to provide it is to change the method signature to require it, like so,
```amok
syntax  scala
##
def second[ElementType](list: List[ElementType])
    (using Tactic[TooShort])
        : ElementType =
  if list.length >= 2 then list(1)
  else abort(TooShortError(2))
```
which may be more easily expressed using the infix `raises` type:
```amok
syntax  scala
##
def second[ElementType](list: List[ElementType])
        : ElementType raises TooShortError =
  if list.length >= 2 then list(1)
  else abort(TooShortError(2))
```

The infix `raises` type is just a syntactic alias. The type
`ElementType raises TooShortError` is equivalent to the context function type,
`Tactic[TooShortError] ?=> ElementType`, which is equivalent to specifying the
`using Tactic[TooShortError]` parameter.

In practice, though, we can usually just append `raises ErrorType` to the
return type. This just defers the problem, though; calling any method declared,
`raises ErrorType`, needs an `ErrorType` in-scope at the callsite. We can
continue adding more `raises ErrorType` declarations, but at some point it is
necessary to _handle_ the error, which requires an instance of
`Tactic[ErrorType]`.

It is the `Tactic` instance which determines exactly how the error is handled:
whether it is thrown, logged, aggregated, or something else. And by passing it
in as a parameter to the method, we are delegating the handling choice back up
to the methods that called it.

In other words, we have just implemented the method for all error-handling
strategies.

### Non-terminal Errors

In all cases, `abort` will stop execution and pass control up the stack to the
point where the error is handled. (Safely-checked exceptions ensure that there
must be such a place.) But sometimes, we want to _accommodate_ the possibility
that, even though failure is inevitable, execution may continue for a while,
with one purpose in mind: to accrue additional errors.

A typical use-case is validating a form containing several fields. Any one of
the values provided for the field may yield an error, but if the form contains
several errors, we would like to see all of them _together_; not just the first.

This becomes possible with certain implementations of `Tactic`, but it requires
cooperation from the implementation. That is provided through an
alternative to `abort`, called `raise`.

When we call `abort`, it allows us to "exit" a method without returning a value.
It is the absence of any value to return which requires this, and this is
reflected in `abort`'s return type: `Nothing`. But `raise` _does_ return a
value—an _ersatz_ or _substitute_ value—which can let execution continue
(using that value) locally, while registering the error and asserting that
an error will be produced, a little later.

As execution continues, additional `raise` invocations may be encountered,
corresponding to more errors. Each of these will be registered by the `Tactic`,
and execution may complete all the way to return a final result for at the point
where the errors are handled. But having registered at least one error during
execution, that final result will be considered invalid and is discarded. And
instead, a new error corresponding to the aggregation of each recorded error
will be produced.

So, by proceeding with execution within the bounds of error checking, it becomes
possible to accrue several "trivial" errors before they manifest into an error
that requires handling.

But this only works under certain conditions. When we provide an ersatz value
in place of an error, that value must be _inconsequential_. That means there
should not be additional code which depends upon its value. We are somewhat
protected from consequentiality by the knowledge that a single `raise`d error
cannot affect the end result, because that value is guaranteed to be discarded.
But this guarantee does not apply to side-effects, including additional errors
which may be raised as a consequence of the ersatz value. Ideally, ersatz values should be innocuous and independent.

### Performance

One advantage of using Scala's `boundary` and `break` infrastructure instead of
_throwing_ exceptions is that the costly construction of a stack trace can be
avoided (optionally), and construction of an error is no more expensive than
any other immutable datatype.

### Strategies

Each error must be handled by a `Tactic`, which will typically be constrained
to a limited scope—often the `within` block of a `mend` or `tend`, or a
`safely` or `unsafely` block. They are _tactics_ in the sense that they apply
within a limited scope.

Tactics contrast with a _strategy_, whose implied scope is wider or global. In
terms of implementation, they are no different: instances of `Tactic`. But we
can informally call them _strategies_ when used globally. For example, the
`throwUnsafely` strategy which provides a universal `Tactic` instance that just
throws any error that's raised.

Other strategies might be `Fatal` instances defined in package-level scope, for
example,
```amok
syntax  scala
##
package app

given InitError is Fatal = error =>
  Log.info(m"Error during initialization: $error")
  ExitStatus(127)
```
which specifies that any `InitError` should cause the JVM to exit, after
logging the error.


## Status

Contingency is classified as __embryotic__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Contingency is designed to be _small_. Its entire source code currently consists
of 642 lines of code.

## Building

Contingency will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Contingency?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Contingency's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Contingency and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `contingency`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Contingency's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Contingency are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/contingency/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Contingency
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Contingency was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

_Contingency_ (the library) provides various forms of mitagation and
contingency in the event that an exception occurs at runtime.

In general, Soundness project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows three tickets, each of which has been _validated_.

## License

Contingency is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

