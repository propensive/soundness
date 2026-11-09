## Interpolation

### About

Checked literals are used throughout Soundness: a `url"…"` which must be a URL, an `sh"…"`
whose quoting must balance, a `j"…"` which must be JSON. All of them are built on one piece of
machinery for defining custom
[string interpolators](https://docs.scala-lang.org/scala3/book/string-interpolation.html).

An [interpolator] defined with it does two things at [compiletime]. It validates the literal's
content, and reports an error at the exact character at fault. And it accepts substitutions
only of the types it declares, each checked for the position it occupies. This is the principal
instrument of [safety by construction](../philosophy/safety-by-construction.md).

Defining an interpolator does not mean writing a macro by hand. The work of capturing the
literal's parts, and their positions in the source file, is done once. The author of an
interpolator supplies ordinary [typeclass] instances, which say how the parts are parsed and
what may be substituted where.

### On interpolators

Scala's built-in `s"…"` interpolation glues strings together without types. Any value is
converted with `toString`, and the literal parts mean nothing to the compiler. Yet a string
literal very often *is* something: a URL, a path, a regular expression, a fragment of SQL. It
has a syntax which could be checked the moment it is written, and positions where only certain
values make sense.

Here an interpolated string is treated as a small program to be compiled:

- The literal parts are parsed at compiletime, by the interpolator's own logic, so a malformed
  literal never reaches a running program.
- Substitutions are resolved through typeclasses. What may be substituted, and how it is made
  safe for its position, is decided by instances in scope, not by `toString`.
- The source positions of the parts are captured, so an error message points at the offending
  character, not at the whole expression.

Everything comes from the `soundness` package:

```scala
import soundness.*
```

A [media type](media-types.md) literal shows the result. A well-formed one is a `MediaType`,
and a misspelled one does not compile:

```scala
val plain: MediaType = media"text/plain"
```

```scala fuel
compile
errors show
##
media"text/plian"
```

### Anatomy of an interpolator

An interpolator is made of three pieces. The first is a prefix method on `StringContext`, which
names the interpolator and hands its content to the machinery:

```scala
extension (inline context: StringContext)
  transparent inline def media: Interpolation = interpolation[MediaType](context)
```

The second is an `Interpolable` instance for the target type, which does the parsing and the
construction. It receives the literal parts with their source positions, and produces the
value, reporting any fault against the character which caused it.

The third is a set of `Insertion` instances, which declare the types that may be substituted,
and how each becomes part of the result.

### Substitutions

An `Insertion` maps a substituted value into the interpolator's own input type, so the
interpolator controls how foreign values enter. The insertions of the
[shell interpolator](processes.md) show the idea. Text becomes one argument, and a list becomes
several:

```scala
given Insertion[Sh.Parameters, Text] = value => Sh.Parameters(value)
given Insertion[Sh.Parameters, List[Text]] = xs => Sh.Parameters(xs*)
```

Insertion is a typeclass, so a new type becomes substitutable by declaring one instance,
without touching the interpolator. A type with no instance cannot be substituted at all, which
is how an interpolator forbids what it cannot make safe.

Sometimes different positions accept different things. A URL's port takes a number, and its
path takes text. A `Substitution` is an `Insertion` with one more type parameter, a singleton
`Label`, which tells the positions apart.

At [runtime] the two behave identically. The difference is at compiletime. An insertion tells
the interpolator's parser only that *something* was substituted. A substitution passes the
parser its label, and the parser proceeds as though the position held a representative token of
the substituted type: `"0"` for a number, `""""` for a string.

```scala
enum SqlInput:
  case IntInput(value: Int)
  case StrInput(value: Text)

given Substitution[SqlInput, Int, "0"] = SqlInput.IntInput(_)
given Substitution[SqlInput, Text, "\"\""] = SqlInput.StrInput(_)
```

The interpolator's compiletime reading of the literal can then depend on what was substituted.
It can check the *shape* of a literal which contains substitutions, instead of giving up
wherever one appears.

### Patterns

The same machinery runs in reverse. An `Extrapolable` instance lets an interpolated literal
serve as a pattern in a `match`, taking a value apart and binding its substitutions. This is
how the patterns of [`j"…"`](json.md#matching-json) and [`html"…"`](html.md) work. One
definition gives both the construction and the match.

The four typeclasses, side by side:

```tabular
column  Typeclass
column  Given for
column  Says
row  `Interpolable`  the result type  how the literal parts are parsed into a value
row  `Insertion[input, value]`  a substitutable type  how a value enters the interpolator's input
row  `Substitution[input, value, label]`  a substitutable type  the same, and which token the parser sees in its place
row  `Extrapolable`  the matched type  how a value is taken apart by a literal pattern
```

### Compiletime and runtime

An interpolator's parsing logic is ordinary code, and it runs in both worlds. At compiletime it
checks literals, and gives compile errors. At runtime it parses input which is built
dynamically, and gives typed [errors](errors.md).

The two cannot drift apart, because they are the same code. A hand-written macro with a
separate runtime parser can make no such guarantee.

### Parts and origins

`interpolate` receives two type parameters, which carry what the compiler knows about the
literal:

```tabular
column  Type parameter
column  Carries
row  `parts`  a tuple of the literal's fragments, as singleton string types, so the interpolator sees the text of each
row  `origins`  where each fragment began in the source file, so an error against a character resolves to that character's position
```

The media-type interpolator's instance, in outline, hands both to a macro:

```scala fuel
parse
##
import gesticulate.internal

inline given interpolable: MediaType is Interpolable:
  transparent inline def interpolate[parts <: Tuple, origins <: Tuple](inline insertions: Any*)
  :   MediaType =

    ${internal.mediaInterpolator[parts]('insertions)}
```

That is what makes the compile error for `media"text/plian"` point at the misspelled subtype,
not at the whole literal. [Metaprogramming](metaprogramming.md) describes the macros
themselves.

### Returning a precise type

The prefix method is `transparent inline`, so an interpolator may return something more
specific than its declared type. A literal can thereby carry what it *is* into the type system,
beyond merely being valid:

- A [path](paths.md) literal returns a path on a particular platform.
- A name literal returns a name which is proved valid for its kind.
- A document literal checked against a schema returns a document typed by that schema.

The cost of that precision is that the result type depends on the literal's content, so the
literal must be known at compiletime. Where it is not, the corresponding runtime parser, which
is the same code, returns the general type and a typed error.

### Choosing between compiletime and runtime

An interpolator is the right tool where a literal has a syntax, and the syntax can be checked.
It is the wrong tool where the content is truly dynamic. There is then nothing to check as the
code compiles, and the interpolator's machinery buys nothing over the runtime parser it
delegates to.

The convention throughout is therefore to offer both: `url"…"` for a literal, and
[`as[Url]`](decoding.md#decoding-a-value) for text obtained at runtime. The same code is behind
each, so a program can move a value from one to the other without any change in behavior.

### Related

Interpolators defined this way include those for [text](text.md#writing-text),
[JSON](json.md#building-json), [XML](xml.md) and [HTML](html.md),
[URLs and other network addresses](network-addresses.md), [paths](paths.md),
[regular expressions](patterns.md), [shell commands](processes.md),
[media types](media-types.md) and [error messages](errors.md#defining-an-error).
