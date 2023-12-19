[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/digression/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/digression/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Digression

__Make Scala stack traces more understandable__

Stack traces correspond to the classes, interfaces and methods as they exist on
the running JVM. The Scala definitions from which they originate may have
undergone various name changes during compilation, and features of Scala which
do not exist in Java may have been encoded in method and class names, such that
the original Scala names and features are harder to discern. _Digression_
believes that a Scala developer (particularly a beginner, who will still be
exposed to stack traces) should not have to understand how Scala is encoded in
order to work with the language.

## Features

- decodes Scala encodings in stack traces
- disambiguates between objects and types in stack frames
- syntax highlights stack traces
- tabulates stack traces to make them easier to read
- introduces a `Codepoint` contextual value containing the sourcefile and line


## Availability

Digression has not yet been published as a binary.

## Getting Started

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


## Status

Digression is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Digression is designed to be _small_. Its entire source code currently consists
of 220 lines of code.

## Building

Digression can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Digression are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/digression/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Digression easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Digression was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

A _digression_ is a deviation from the main subject, much like an exception departs from the main path.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo is an explosion, indicative of the most disastrous of exceptions.

## License

Digression is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
