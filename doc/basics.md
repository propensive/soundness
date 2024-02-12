All Contingency terms and types are defined in the `contingency` package:
```scala
import contingency.*
```

_Contingency_ provides a number of different ways to work with errors in Scala,
for libraries which opt in to its advanced error-handling capabilities.

Contingency builds upon Scala 3's _safer exceptions_ with its new
`boundary`/`break` control flow syntax to abstract over exception handling.

We will look at how this works at the call site first, and then from the
implementor's perspective.

### Partial methods

A _partial method_ is a method which may not produce a result for certain
inputs, i.e. it is not _total_. Partial methods are already familiar in Scala
(and Java), but the way they handle the absence of a result is invariably to
throw an exception, determined directly or indirectly by the code in the
method's implementation. The method's signature _may_ also specify the types of
exception it can throw, if it has been written with _safer exceptions_ in mind.

For Contingency's purposes, a partial method is one which declares a `raises`
clause in its type, such as,
```scala
import fulminate.{msg, Error}
import rudiments.Bytes

case class ReadError() extends Error(msg"the data could not be read")

def readFromDisk(): Bytes raises ReadError =
  Bytes() // Needs implementation
```

The `raises ReadError` clause is equivalent to taking an additional contextual
type parameter of type `Raises[ReadError]`, like so,
```scala
def readFromDisk2()(using Raises[ReadError]): Bytes =
  Bytes() // Needs implementation
```
and this latter form is more typical for methods which may raise more than one
different type of error.

### Raising

Contingency introduces the terminology of _raising_ as a generalization of
_throwing_ which is dependent on callsite context. If the callsite context
calls for throwing, then _raising an error_ will mean _throwing an error_. But
there are alternative interpretations of raising which don't involve throwing.

Raising is modeled as a _capability_, represented by a `Raises` value that is
implicitly passed to the `readFromDisk` method. So the presence of a contextual
`Raises[ReadError]` instance indicates the _capability_ of raising a
`ReadError`.

Code which calls a partial method without contextual `Raises` instances of the
appropriate types to satisfy the `using` parameters of the method will be a
type error. Thus, it is impossible to call a partial method which needs the
capability to raise certain error types unless those capabilities exist in the
callsite context.

This principal is critical for robust and safe error handling, since it
leverages the type system to oblige the programmer to handle the exceptional
cases of partial methods.

### Simple Error Handling

If we just don't care about error handling, for example when building a
prototype, we can effectively `turn off` error handling by providing a global
contextual value which "handles" all errors by throwing them as exceptions,
```scala
import contingency.errorHandling.throwUnsafely
```
which is equivalent to Scala's default behavior: errors will be unchecked, and
will be thrown like traditional exceptions, bubbling through the stack until
caught in a `catch` block, or reaching the bottom of the stack.

A similar, but more fine-grained use case applies if we know that (or at least
make an informed judgement that) a call to a partial method will definitely
return a value, and we decide that there is no point in handling an error case
that will not happen in practice.

Such an expression or block of statements can be wrapped in a call to
`unsafely`, which will provide a general `Raises` instance necessary for
compilation, like `errorHandling.throwUnsafely`, but expected to never be used,
and constrained to just that expression or those statements.

```scala
@main
def run(): Unit = unsafely:
  val bytes = readFromDisk()
  Out.println(bytes.length)
```

Similar to `unsafely` is `safely`, which can also wrap expressions or
statements. But unlike `unsafely`, which can throw exceptions, `safely` will
return an `Unset` value if an error is raised. This effectively turns any
expression which would return some `ReturnType` into an expression which
returns `Optional[ReturnType]`.

One happy benefit of this is that the relatively expensive performance cost of
constructing and throwing an exception, only to discard it for an `Unset`
value, is saved. The exception instance is never constructed because Contingency
knows from the callsite context (i.e. inside the `safely` wrapper) that it can
just return `Unset` instead.

### Mitigation

It is good practice for methods which do different things to raise different
types of error. It makes it much easier to diagnose a problem when the error's
type gives a strong indication about what went wrong.

It also encourages composition or sequencing of partial methods, confident that
an error raised in the resultant expression or block will carry enough
information to disambiguate it from other possible errors in the same code.

This compositionality is helpful until we reach a method boundary where we need
to declare every possible error that could be raised from that method call.

Indeed, if we were defining a partial method called `publish` which writes to
disk, invokes a shell command, and then sends an HTTP request, it would be
frustrating for users of that method to have to handle `WriteError`s,
`ShellError`s and `HttpError`s when they are interested primarily in knowing
that _publishing_ failed, and only secondarily in the underlying cause.

So rather than defining it as,
```scala
def publish(): Unit raises WriteError raises ShellError raises HttpError =
  write()
  invoke()
  sendRequest()
```
we would prefer to introdue a new error type, `PublishError`, containing the
detail of the issue, like so:
```scala
enum PublishIssue:
  case Disk, Shell, Internet

case class PublishError(cause: PublishIssue)
extends Error(msg"publishing failed because of $cause")

def publish2(): Unit raises PublishError = ???
```

However, the calls to `write()`, `invoke()` and `sendRequest()` each raise
different types of error, and the body of `publish2` only has the capability of
raising `PublishError`s, by virtue of its `Unit raises PublishError` return
type.

The solution is to introduce _mitigations_.

A mitigation is a given instance which transforms an error of one type into an error of a different type. In the example above, we need mitigations to transform `WriteError`s, `ShellError`s and `HttpError`s into `PublishError`s.

We can write these as follows:
```scala
given (PublishError mitigates WriteError) = PublishError(PublishIssue.Disk).waive
given (PublishError mitigates ShellError) = PublishError(PublishIssue.Shell).waive
given (PublishError mitigates HttpError) = PublishError(PublishIssue.Internet).waive
```

Note that the `waive` method from
[Rudiments](https://github.com/propensive/rudiments/) is used to transform the
value into a lambda whose variable is discarded.

Each given definition provides a new `Mitigation` instance, which will be used
to construct a `Raises[WriteError]`, `Raises[ShellError]` or
`Raises[HttpError]` as necessary, using a contextual `Raises[PublishError]`.
These may be defined locally to the method, or in a more universal scope.

A full example might look like this:
```scala
def publish3(): Unit raises PublishError =
  given (PublishError mitigates WriteError) = PublishError(PublishIssue.Disk).waive
  given (PublishError mitigates ShellError) = PublishError(PublishIssue.Shell).waive
  given (PublishError mitigates HttpError) = PublishError(PublishIssue.Internet).waive
  
  write()
  invoke()
  sendRequest()
```

