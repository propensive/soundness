### `safely` and `unsafely`

When `language.experimental.saferExceptions` is enabled, any expression which
may throw an exception must be handled with a `try`/`catch`, or the exception
declared in its signature.  Sometimes, we know enough about the execution state
to circumvent this.

If we know that the exception will not be thrown (or we are confident that it
doesn't matter!), then we can wrap the expression in `unsafely`, for example,
invoking a `send()` method which could throw a `SendError`,
```scala
def send(): Response throws SendError = ...
```
can be performed without any error handling with:
```scala
import digression.*
import language.experimental.saferExceptions

val response: Response = unsafely(send())
```

The `unsafely` method works by providing a contextual `CanThrow[Exception]`
instance in its parameter.

The `safely` method can be used in the same way, but will catch thrown
exceptions and convert them to the `Unset` value, effectively changing the
return type from `ReturnType` to `Maybe[ReturnType]`.

This is useful in cases where an exception may be expected, but enough is known
about it to handle it without examining it. An additional use-case is
_fire-and-forgat_ method calls where we care neither about the normal nor an
exceptional result.

### `Codepoint`

A method may request a `Codepoint` value in `using` clause, like so:
```scala
def callMe()(using codepoint: Codepoint): Unit
```

The `codepoint` parameter will contain, _at runtime_, two values which will be
computed _during compilation_ at the callsite: the source file of the callsite,
and the line number of that file.

So, compiling a file called `src/tests.scala` which contains calls to
`callMe()` on lines `138` and `211` will provide the instances,
`Codepoint("src/tests.scala", 138)` and `Codepoint("src/tests.scala", 211)` as
`using` parameters to these invocations.

Subsequent recompilations may, of course, potentially change the line numbers
if the invocations move within the file.

### The `Error` type and `err""` messages

Error messages for exceptions are usually constructed as a single string. For
most purposes, this is sufficient, but by eagerly converting the exception's
parameters to strings, it makes a compromise on the flexibility of the message
at a later time when it may be presented to a user.

A typical exception will include one or more parameters, which capture,
somehow, the conditions in which the exception was thrown. An error message
will present those parameters in context, in a human-readable form, for
(usually) a programmer to decipher. However, converting those parameters to
strings at the point of costruction makes it impossible to inspect them later,
and capturing the parameters in the exception independently of the message
would duplicate them unnecessarily. One particular compromise made is the
inability to distinguish between the parts of the message that are static for
all instances of the exception, and those which vary depending on its state.

The essence of an uncompromising, structured error message would be a `Tuple` of
_n_ parameters, with _n + 1_ fragments of text interleaving it. For example, an
error message about a missing file could be structured,
```scala
case class MissingFile(dir: Directory, child: Text) extends Exception
```
with a message that reads:
```scala
t"The directory $dir did not contain the file $child."
```

By representing this as the tuple, `(dir: Directory, child: Text)` and the
text fragments, `List(t"The directory ", t" did not contain the file ", t".")`,
we would store precisely the state we want, and is provided in
[Rudiments](https://github.com/propensive/rudiments/)' `ErrorMessage` type:
```scala
import rudiments.*
val dir: Directory = ...
val child: Text = ...
val message = ErrorMessage[(Directory, Text)](List(t"The directory ",
    t" did not contain the file ", t"."), (dir, child))
```

Unfortunately, this is a very inconvenient way to write a message.

The `err""` interpolator constructs an instance of `ErrorMessage` without such
convoluted code:
```scala
val message = err"The directory $directory did not contain the file $child."
```

Combining this with Digression's `Error` class, distinct from
`java.lang.Error`, provides a convenient way of defining a new exception type:
```scala
import digression.*
case class MissingFile(dir: Directory, child: Text)
extends Error(err"The directory $directory did not contain the file $child.")
```

### Rewritten stack traces

Digression provides an immutable representation of an exception's stack trace,
called `StackTrace`, constructed from a `Throwable`, but with stack frames
rewritten to make it easier to relate Java's raw method names with the Scala
code which created them.

This includes the following rewrites:
 - symbolic names are decoded
 - class vs object calls are distinguished with `#` or `.`
 - primitives are written with full names
 - package file objects and extenion methods are indicated as such
 - lambdas and anonymous classes are identified
 - `Function` types are written inline

[Escapade](https://github.com/propensive/escapade) provides a contextual
`AnsiShow` instance for `StackTrace`, and by extension, `Exception`.

