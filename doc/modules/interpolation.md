## Interpolation

### About

The checked literals used throughout Soundness — a `url"…"` that must be a URL, an `sh"…"` whose
quoting must balance, a `json"…"` that must parse — are all built on one piece of machinery for
defining custom [string interpolators](https://docs.scala-lang.org/scala3/book/string-interpolation.html).
An interpolator defined with it validates its literal content as the code compiles, reporting an
error at the exact character at fault, and accepts substitutions only of the types it declares,
each checked for the position it occupies. This is the principal instrument of
[safety by construction](../philosophy/safety-by-construction.md).

Defining one does not mean writing a macro by hand: the compilation-time plumbing — capturing the
literal parts and their positions in the source file — is done once, and an interpolator's author
supplies ordinary typeclass instances that say how the parts are parsed and what may be
substituted where.

### On interpolators

Scala's built-in `s"…"` interpolation is untyped gluing: any value converts via `toString`, and
the literal parts mean nothing to the compiler. Yet a string literal very often *is* something —
a URL, a path, a regular expression, a fragment of SQL — with a syntax that could be checked the
moment it is written, and positions where only certain values make sense.

Soundness treats an interpolated string as a small program to be compiled. The literal parts are
parsed at compiletime by the interpolator's own logic, so a malformed literal never reaches a
running program; substitutions resolve through typeclasses, so what may be spliced, and how it is
rendered safely for its position, is decided by instances in scope rather than by `toString`; and
because the source positions of the parts are captured, an error message points at the offending
character, not at the whole expression. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Anatomy of an interpolator

Three pieces make an interpolator. A prefix method on `StringContext` names it and hands its
content to the machinery:

```scala
extension (inline context: StringContext)
  transparent inline def media: Interpolation = interpolation[MediaType](context)
```

An `Interpolable` instance for the target type does the parsing and construction — it receives the
literal parts, with their source positions, and produces the value, reporting any fault against
the character that caused it. And `Insertion` instances declare which types may be substituted
into a hole, and how each becomes part of the result.

With those in place, `media"text/plain"` constructs a checked `MediaType`, and
`media"text/plian"` — misspelled — is a compile error where it is written.

### Substitutions

An `Insertion` maps a substituted value into the interpolator's own input type, so the
interpolator controls how foreign values enter. The shell interpolator's insertions show the
idea — text becomes one argument, a list becomes several, and any type with the right instance
splices safely:

```scala
given Insertion[Parameters, Text] = value => Parameters(value)
given Insertion[Parameters, List[Text]] = xs => Parameters(xs*)
```

Because insertion is a typeclass, a new type becomes substitutable by declaring one instance,
without touching the interpolator; and a type with no instance simply cannot be spliced, which is
how an interpolator forbids what it cannot make safe.

Where different holes accept different things — a URL's port position takes a number, its path
position takes text — a `Substitution` carries an extra label distinguishing the positions, so
each hole is checked against what belongs there.

A `Substitution` is an `Insertion` with one more type parameter: a singleton `Label`. At runtime
the two behave identically; the difference is at compiletime. Where an insertion tells the
interpolator's parser only that *something* was substituted here, a substitution passes it that
label, so the parser can proceed as though the hole contained a representative token of the
substituted type — `"0"` for a number, `""""` for a string:

```scala
given Substitution[SqlInput, Int, "0"] = IntInput(_)
given Substitution[SqlInput, Text, "\"\""] = StrInput(_)
```

The interpolator's compiletime interpretation of the literal can then depend on what was inserted,
which is what allows an interpolator to check the *shape* of a literal containing holes rather
than giving up wherever a hole appears.

### Patterns

The same machinery runs in reverse. An `Extrapolable` instance lets an interpolated literal serve
as a pattern in a `match`, deconstructing a value and binding its holes — which is how `json"…"`
and `html"…"` patterns work. One definition gives both the construction and the match.

### Compiletime and runtime

An interpolator's parsing logic is ordinary code, and it runs in both worlds: at compiletime for
literals, giving compile errors; and at runtime for dynamically-built input, giving typed errors.
The two cannot drift apart, because they are the same code — a guarantee no hand-written macro
plus separate runtime parser can make.

### Parts and origins

`interpolate` receives two type parameters that carry what the compiler knows about the literal.
`parts` is a tuple of the literal fragments as singleton string types, so the interpolator sees
the text of each; `origins` carries where each fragment began in the source file, so an error
reported against a character within a fragment resolves to that character's position in the file
rather than to the start of the expression:

```scala
inline given interpolable: MediaType is Interpolable:
  transparent inline def interpolate[parts <: Tuple, origins <: Tuple](inline insertions: Any*)
  :   MediaType =

    ${internal.mediaInterpolator[parts]('insertions)}
```

That is what makes a compile error underline the misspelled subtype in `media"text/plian"`
instead of the whole literal.

### Returning a precise type

The prefix method is `transparent inline`, so an interpolator may return something more specific
than its declared type — this is how a literal can carry what it *is* into the type system rather
than merely validating itself. A path literal returns a path on a particular platform; a name
literal returns a name proved to be valid for its plane; a schema-checked document literal returns
a document typed by its schema.

The cost of that precision is that the result type depends on the literal's content, so the
literal must be known at compiletime. Where it is not, the corresponding runtime parser — the same
code — returns the general type and a typed error.

### Choosing between compiletime and runtime

An interpolator is the right tool where a literal has a syntax and that syntax can be checked. It
is the wrong tool where the content is genuinely dynamic: there is nothing to check as the code
compiles, and the interpolator's machinery buys nothing over the runtime parser it delegates to.

The convention throughout is therefore to offer both — `url"…"` for a literal and `as[Url]` for
text obtained at runtime — with the same code behind each, so a program can move a value from one
to the other without a change in behaviour.
