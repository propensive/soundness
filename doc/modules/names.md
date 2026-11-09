## Names

### About

A filename, a DOM id, a CSS class, a Git branch: each is text, but not just any text. Each
lives in a namespace, with its own rules about what a valid name looks like. Such a name is
represented as a `Name`, whose rules are part of its type.

A `Name[DomId]` is known to satisfy the rules for DOM ids. A literal which breaks them does not
compile. Text which arrives at [runtime] becomes a `Name` only by passing the same validation,
and a failure is a typed error which names the exact rule that was broken. To possess a `Name`
is therefore to have proof that it is valid, which is
[safety by construction](../philosophy/safety-by-construction.md).

The rules themselves are types, such as `MustNotContain["/"]`, `MustNotEqual[".."]` and
`MustMatch[…]`, combined by intersection. A namespace's constraints are written once, can be
read directly from the code, and are enforced wherever a name of that namespace is required.

### On names

Namespaces disagree about validity. A POSIX filename may not contain a slash. A DOM id may not
contain whitespace. A DNS label has a limited length and a restricted alphabet. Code which
carries all of these as plain strings enforces none of it. A name which is invalid in its
namespace travels until something external rejects it, such as a filesystem, a browser or a
server, at a distance from the mistake.

Here the namespace is attached to the name. Each namespace, called a *plane*, declares its
rules as a type. A `Name` for that plane exists only if its text satisfies them, which is
checked at [compiletime] for a literal, and by validation at runtime. This is the mechanism
beneath the element names of [paths](paths.md), and it is open to any namespace a program
defines.

Everything comes from the `soundness` package. The samples throw any error as an exception, and
the last import lets them print their results:

```scala
import soundness.*
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Defining a plane

A plane is a marker type, with a `Nominative` given which states its rules as an intersection:

```scala
sealed trait Tag

inline given Tag is Nominative under
    MustNotStart["0"] & MustNotContain["."] & MustNotEqual[""] = !!
```

A `Name[Tag]` now means text which does not start with `0`, contains no `.`, and is not empty.
Those facts are carried by the type, and need not be checked again by every consumer.

### Writing a name

The `n"…"` [interpolator] writes a name, and checks it at compiletime against the planes in
scope. The name takes the type of every plane whose rules it satisfies, so one literal serves
wherever any of them is required:

```scala
val tag: Name[Tag] = n"release"
```

A literal which breaks a rule of the plane it is needed for does not compile:

```scala fuel
compile
errors show
##
val bad: Name[Tag] = n"0hello"
```

### Validating at runtime

Text from outside becomes a name through the same rules. A failure raises a `Name.Error`, which
names the rule that was violated:

```scala fuel
run
##
Out.println(Name[Tag](t"release").show)
```

```scala fuel
run
errors show
##
Name[Tag](t"0hello")
```

The precision of the error matters. A user who is told *which* rule their input broke can fix
it, where "invalid name" invites guesswork. The error is handled like any other, so
[`safely`](errors.md#choosing-a-strategy) turns an invalid name into `Unset`, and `capture`
gives the error itself:

```scala fuel
run
##
Out.println(capture[Name.Error](Name[Tag](t"v1.0")).message.show)
```

### Built-in planes

Some namespaces are already defined:

```tabular
column  Plane
column  A name must be
row  `DomId`  a valid [HTML](html.md) id
row  `CssClass`  a valid [CSS](css.md) identifier
row  `Required`  not empty, for the common case where that is the whole rule
```

```scala fuel
run
##
Out.println(Name[CssClass](t"main-nav").show)
Out.println(capture[Name.Error](Name[DomId](t"a b")).message.show)
```

The filesystem [paths](paths.md) of each platform define the rules for their elements in the
same way, which is how a path element which contains a slash fails where it is written.

### The rules

These rules cover most namespaces. Each knows how to describe itself, which is where the error
messages, and the compile errors, get their words:

```tabular
column  Rule
column  Parameter
column  Description
row  `MustContain`  text  must contain it
row  `MustNotContain`  text  must not contain it
row  `MustStart`  text  must start with it
row  `MustNotStart`  text  must not start with it
row  `MustEnd`  text  must end with it
row  `MustNotEnd`  text  must not end with it
row  `MustNotEqual`  text  must not equal it; `MustNotEqual[""]` reads "must not be empty"
row  `MustMatch`  a regular expression  must match it
row  `MustNotMatch`  a regular expression  must not match it
row  `JavaIdentifier`  a description  must be *the description*; checks for a Java identifier
```

A rule is two things: a check, and a phrase which describes what it demands. Writing a rule
means supplying both:

```scala
object MustEnd extends Rule({ text => m"must end with $text" }, _.ends(_))
```

The description is part of the rule, and is not written at each use. The compile error for a
bad literal and the runtime error for bad input therefore say the same thing, in the same
words.

`JavaIdentifier` shows the other shape a rule can take. It does not take text to compare
against. Its parameter is the *description*, and it checks against a predicate of its own. A
namespace with a rule which none of the others can express defines one in this way, as `DomId`
does.

### Combining rules

A plane's rules are all required: a name must satisfy every one of them. The first rule which
is violated is the one reported, so an error names a single concrete fault, not a list of
everything that is wrong. To write a plane is therefore to state its rules in the order in
which they should be reported.

A plane is a type, so one plane's names are not another's. A `Name[Tag]` cannot be passed where
a `Name[Branch]` is expected, even if the two happen to have identical rules. That is the
point: the rules may diverge later, and the two namespaces were never the same thing.

### Length limits

A `Nominative` carries a `Limit` beside its `Self`, which is how a plane states a maximum
length: a filesystem whose names may not exceed 255 bytes, a protocol field of fixed width, a
database column. The limit is part of the plane's type, so it is checked where a name is
constructed, not where it is finally written out.

### Related

- [Paths](paths.md) builds filesystem paths from names which are valid on a particular
  platform.
- [HTML](html.md) and [CSS](css.md) use `Name[DomId]` and `Name[CssClass]` for ids and classes.
- [Interpolation](interpolation.md#returning-a-precise-type) explains how `n"…"` gives a
  literal the type of the planes it satisfies.
- [Forms](forms.md) names its fields, and [concurrency](concurrency.md) its tasks, with validated names.
