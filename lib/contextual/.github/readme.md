[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/contextual/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/contextual/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.com/invite/MBUrkTgMnA)
<img src="/doc/images/github.png" valign="middle">

# Contextual

__Statically-checked string interpolation__

__Contextual__ makes it simple to write typesafe, statically-checked interpolated strings.

Contextual is a Scala library which allows you to define your own string interpolators—prefixes for
interpolated string literals like `url"https://propensive.com/"`—which specify how they should be checked
at compiletime and interpreted at runtime, writing very ordinary user code with no user-defined macros.

## Features

- user-defined string interpolators
- introduce compiletime failures on invalid values, such as `url"htpt://example.com"`
- compiletime behavior can be defined on _literal_ parts of a string
- runtime behavior can be defined on literal and interpolated parts of a string
- types of interpolated values can be context-dependent

## Availability







## Getting Started

### About Interpolators

An interpolated string is any string literal prefixed with an alphanumeric string, such as
`s"Hello World"` or `date"15 April, 2016"`. Unlike ordinary string literals, interpolated strings
may also include variable substitutions: expressions written inline, prefixed with a `$` symbol,
and—if the expression is anything more complicated than an alphanumeric identifier—requiring braces
(`{`, `}`) around it. For example,
```scala
val name = "Sarah"
val string = s"Hello, $name"
```
or,
```scala
val day = 6
val string2 = s"Tomorrow will be Day ${day + 1}."
```

Anyone can write an interpolated string using an extension method on `StringContext`, and it will be
called, like an ordinary method, at runtime.

But it's also possible to write an interpolator which is called at compiletime, and which can
identify coding errors _before_ runtime.

Contextual makes it easy to write such interpolators.

### Contextual's `Verifier` type

An interpolated string may have no substitutions, or it may include many substitutions, with a
string of zero or more characters between at the start, end, and between each adjacent pair.

So in general, any interpolated string can be represented as _n_ string literals, whose values are
known at compiletime, and _n - 1_ variables (of various types), whose values are not known until
runtime.

Contextual provides a simple `Verifier` interface for the simplest interpolated
strings—those which do not allow any substitutions.

A new verifier needs just a a type parameter for the return type of the
verifier, and a single method, `verify`, for example, a binary reader:
```scala
import contextual.*
import anticipation.Text

object Binary extends Verifier[IArray[Byte]]:
  def verify(content: Text): IArray[Byte] = ???
    // read content as 0s and 1s and produce an IArray[Byte]
```

This defines the verifier, but has not yet bound it to a prefix, such as `bin`.
To achieve this, we need to provide an extension method on `StringContext`,
like so:
```scala
extension (inline ctx: StringContext)
  inline def bin(): IArray[Byte] = ${Binary.expand('ctx)}
```

Note that this definition must appear in a separate source file from the definition of the verifier.

This simple definition makes it possible to write an expression such as
`bin"0011001011101100"`, and have it produce a byte array.

#### More advanced interpolation

For string interpolations which support substitutions of runtime values into
the string, Contextual provides the `Interpolator` type.

Contextual's `Interpolator` interface provides a set of five abstract
methods—`initial`, `parse`, `insert`, `skip` and `complete`—which are invoked,
in a particular order, once at compiletime, _without_ the substituted values
(since they are not known when it runs!), and again at runtime, _with_ the
substituted values (when they are known).

The method `skip` is used at compiletime, and `insert` at runtime.

The methods are always invoked in the same order: first `initial`; then alternately `parse` and
`insert`/`skip`, some number of times, for each string literal and each substitution (respectively);
and finally `complete` to produce a result. `insert` may never be invoked if there are no
substitutions, but `parse` will always be invoked once more than `insert`.

For example, for a string with two substitutions, the invocation order would be:
```
initial -> parse -> insert -> parse -> insert -> parse -> complete
```
at runtime, or,
```
initial -> parse -> skip -> parse -> skip -> parse -> complete
```
at compiletime.

An object encoding the interpolator's state is returned by each of these method calls, and is passed
as input to the next—with the exception of `complete`, which should return the final value that the
interpolated string will evaluate to. This is where final checks can be carried out to check that
the interpolated string is in a complete final state.

In other words, each segment of an interpolated string is read in turn, to incrementally build up
a working representation of the incomplete information in the interpolated string. And at the end,
it is converted into the return value.

The separation into `parse` and `insert`/`skip` calls means that the static parts of the
interpolated string can be parsed the same way at both compiletime or runtime, while the dynamic
parts may be interpreted at runtime when they're known, and their absence handled in some way at
compiletime when they're not known.

Of course, `skip` could be implemented to delegate to `insert` using a dummy value.

Here are the signatures for each method in the `Interpolator` type:
```scala
trait Interpolator[Input, State, Result]:
  def initial: State
  def parse(state: State, next: Text): State
  def insert(state: State, value: Input): State
  def skip(state: State): State
  def complete(value: State): Result
```

Three abstract types are used in their definitions: `State` represents the information passed from
one method to the next, and could be as simple as `Unit` or `Text`, or could be some complex
document structure. `Input` is a type chosen to represent the types of all substitutions. `Text`
would be a common choice for this, but there may be utility in using richer types, too. And `Return`
is the type that the interpolated string will ultimately evaluate to.

In addition to `parse`, `insert`, `skip` and `complete` taking `State` instances as input, note that
`parse` always takes a `Text`, and `insert` takes an `Input`.

Any of the methods may throw an `InterpolationError` exception, with a message. At compiletime,
these will be caught, and turned into compile errors. Additionally, a range of characters may be
specified to highlight precisely where the error occurs in an interpolated string.

Any interpolator needs to choose these three types, and implement these four methods.

For example, the interpolated string,
```
url"https://example.com/$dir/images/$img"
```
could be interpreted by a Contextual interpolator, in which case it would be checked at
compiletime with the composed invocation,
```scala
val result = complete(parse(insert(parse(insert(parse(initial, "https://example.com/"), None),
    "/images/"), None), ""))
```
and at runtime with something which is essentially this:
```scala
val runtimeResult = complete(parse(insert(parse(insert(parse(initial, "https://example.com/"), Some(dir)),
    "/images/"), Some(img)), ""))
```

### Compile Errors

Throwing exceptions provides the flexibility to raise a compilation error just by examining the
`state` value and/or the other inputs.

For example, insertions could be permitted only in appropriate positions, i.e. where the `state`
value passed to the `insert` method indicates that the insertion can be made. That is knowable at
compiletime, without even knowing the inserted value, and can be generated as a compile error by
throwing an `InterpolationError` in the implementation of `insert`.

The compile error will point at the substituted expression.

Likewise, throwing an `InterpolationError` in `parse` will generate a compile error. The optional
second parameter of `InterpolationError` allows an offset to be specified, relative to the start of
the literal part currently being parsed, and a third parameter allows its length to be specified.

For example, if we were parsing `url"https://example.ocm/$dir/images/$img"`, and wanted to highlight
the mistake in the invalid TLD `.ocm`, we would throw, `InterpolationError("not a valid TLD", 15, 4)`
during the first invocation of `parse`, and the Scala compiler would highlight `.ocm` as the error
location: in this example, `15` is the offset from the start of this part of the string to the
error location, and `4` is the length of the error.

### Binding an interpolator

A small amount of boilerplate is needed to bind an `Interpolator` object, for example `Abc`, to a
prefix, i.e. the letters `abc` in the interpolated string, `abc""`:
```scala
extension (inline ctx: StringContext)
  transparent inline def abc(inline parts: Any*): Return =
    ${Abc.expand('ctx, 'parts)}
```

This boilerplate should be modified as follows:
 - the method name, `abc`, should change to the desired prefix,
 - the method's return type, `Return`, should be changed to the return type of the `complete` method, and,
 - the interpolator object, `Abc`, should be specified.

In particular, the type of `parts`, `Any*`, should be left unchanged. This does not mean that `Any`
type may be substituted into an interpolated string; Contextual provides another way to constrain
the set of acceptable types for insertions.

### Insertions

Contextual uses a typeclass interface to support insertions of different types. An insertion of a
particular type, `T`, into an interpolator taking a value of type `I` requires a corresponding
given `Insertion[I, T]` instance in scope.

This means that the set of types which may be inserted into an interpolated string can be defined
ad-hoc. There is only the requirement that any inserted type, `T`, may be converted to an `I`, since
`I` is a type known to the `Interpolator` implementation.

So, if an interpolator's general `Input` type is `List[Text]`, and we wanted to permit insertions
of `List[Text]`, `Text` and `Int`, then three given instances would be necessary:

```scala
given Insertion[List[Text], Text] = List(_)
given Insertion[List[Text], List[Text]] = identity(_)
given Insertion[List[Text], Int] = int => List(int.show)
```

### Substitutions

A `Substitution` is a typeclass that's almost identical to `Insertion` (and is, indeed, a subtype of
`Insertion`), but takes an additional type parameter: a singleton `Text` literal. The behavior of
a given `Substitution` will be identical to a given `Insertion` at runtime, but differs at
compiletime:

During macro expansion, instead of invoking `skip`, the `substitute` method will be called instead,
passing it the `Text` value taken from the additional type parameter to `Substitution`.

For example the given definitions,
```scala
given Substitution[XInput, Text, "\"\""] = str => StrInput(str)
given Substitution[XInput, Int, "0"] = int => IntInput(int)
```
would mean that an `Int`, `int`, and a `Text`, `str`, substituted into an interpolated string
would result in invocations of, `substitute(state, "0")` and `substitute(state, "\"\"")`
respectively.

By default, the `substitute` method simply delegates to `parse`, which takes the same parameters,
and will parse the substituted strings in a predictable way. Any user-defined `substitute` method
implementation will therefore need the `override` modifier, but can provide its own implementation
that is distinct from `parse`.

The benefit of `Substitution` over `Insertion` is that the compiletime interpretation of the
interpolated string may be dependent on the types inserted, distinguishing between types on the
basis of the singleton `String` literal included in the given's signature. This compares to the
`skip` method which offers no more information about a substitution than its existence.

### A First Interpolator

Here is a trivial interpolator which can parse, for example, `hex"a948b0${x}710bff"`, and return an
`IArray[Byte]`:
```scala
import rudiments.*
import anticipation.*

object Hex extends Interpolator[Long, Text, IArray[Byte]]:
  def initial: Text = ""

  def parse(state: Text, next: Text): Text =
    if next.forall(hexChar(_)) then state+next
    else throw InterpolationError("not a valid hexadecimal character")
  
  def insert(state: Text, value: Option[Long]): Text =
    value match
      case None       => s"${state}0".tt
      case Some(long) => s"${state}${long.toHexString}".tt
  
  def complete(state: Text): IArray[Byte] =
    IArray.from(convertStringToByteArray(state))
  
  private def hexChar(ch: Char): Boolean =
    ch.isDigit || 'a' <= ch <= 'f' || 'A' <= ch <= 'F'
```

Having defined this interpolator, we can bind it to the prefix, `hex` with:
```scala
extension (ctx: StringContext)
  transparent inline def hex(inline parts: Any*): IArray[Byte] =
    ${Hex.expand('ctx, 'parts)}
```

Note that this should be defined in a different source file from the object `Hex`.





## Status

Contextual is classified as __maturescent__. For reference, Soundness projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Contextual is designed to be _small_. Its entire source code currently consists
of 154 lines of code.

## Building

Contextual will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Contextual?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Contextual's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Contextual and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `contextual`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Contextual's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Contextual are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/contextual/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Contextual
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Contextual was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

Contextual takes its name from its ability to provide context-aware substitutions in interpolated strings.

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

The logo is of a quote symbol, alluding to Contextual's subject matter of quoted strings.

## License

Contextual is copyright &copy; 2025 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

