# Safety by Construction

Soundness makes invalid values impossible to construct in the first place, checking
them as the code compiles. A guiding conviction is that any static analysis a
programmer can do, the compiler can do better — and any analysis the compiler can do,
it should. A value that exists is therefore known to be well-formed, and the code that
receives it need not check again.

Interpolators express this most clearly. Each little language embedded in a program —
URLs, paths, regular expressions, dates, media types, JSON — has a grammar, and its
literals are parsed against that grammar at compiletime, by the same parser that will
handle runtime input. A malformed literal is a compile error pointing at the offending
character:

```scala
url"https://example.com/api"    // checked against the URL grammar
r"[a-z]+(suffix"                // does not compile: unclosed group
media"application/jsom"         // does not compile: did you mean application/json?
v"1.2"                          // does not compile: not a semantic version
```

The same principle reaches beyond literals. A date that cannot exist — the 31st of a
30-day month — is rejected where it is written; a `Money in "EUR"` cannot be added to a
`Money in "GBP"`; a database relation the schema does not declare fails to compile; and
an HTML `<br>` given children is a compile error because the specification says so.
In each case the check is not a runtime validation moved earlier, but a construction
that only admits valid values — so the invalid state has no representation at all, and
downstream code inherits the guarantee for free.

## One parser, two moments

The property that makes this trustworthy is that the compiletime and runtime paths are
*the same code*. An interpolator's parsing logic is ordinary Scala; it runs at
compiletime against a literal, giving a compile error, and at runtime against dynamic
input, giving a typed error:

```scala
url"https://example.com/api"          // checked as the code compiles
text.as[HttpUrl]                      // checked as the program runs
```

The two cannot drift, because there is nothing to keep in step. A hand-written macro plus
a separate runtime parser is the arrangement this avoids: two implementations of one
grammar, differing in exactly the edge cases nobody tested.

The convention is therefore that every checked literal has a runtime counterpart, and a
value can move between the two — a configuration read from a file and a default written
in source produce the same type, with the same guarantees.

## Precision in the return type

An interpolator's prefix method is `transparent inline`, so it may return something more
specific than its declared type. This is what allows a literal to carry what it *is* into
the type system rather than merely validating itself:

```scala
p"/home/user/data.csv"                // a Path, on a specific platform
n"worker"                             // a Name, valid for its plane
% / "foo" / "bar"                     // Path of ("bar", "foo") — the elements in the type
```

The last is the strongest form: the path's element names are in its type, so code that
receives it knows exactly where it points. That precision is only available for literals,
which is the honest limit of the technique — text obtained at runtime yields the general
type, because nothing more is known about it.

## What can and cannot be checked this way

The technique applies where validity is a property of the *value*, decidable from the
value alone. A URL's syntax, a date's existence, a media type's registration, a regular
expression's balance, a schema's shape: all decidable, all checked.

It does not apply where validity depends on the world. Whether `example.com` resolves,
whether a file exists, whether a port is free, whether a certificate is still valid —
none is knowable when the code compiles, and a type claiming otherwise would be lying.
Those become typed errors at the point of use, which is [total
transitions](total-transitions.md) doing the work instead.

The line between the two is worth drawing carefully, because the temptation is to over-claim.
A `Hostname` promises syntactic validity and nothing more, and the documentation says so
rather than letting a reader infer that a well-typed hostname is a reachable one.

## What it costs

**Compiletime.** Every literal is parsed by the compiler, and a file dense with them
compiles measurably more slowly. Macro expansion is the single largest contributor to
Soundness's own build times.

**Error messages are the macro's responsibility.** A compile error from a failed literal is
only as good as the message the interpolator produces. Getting a caret onto the offending
character — rather than underlining the whole expression — takes deliberate work with the
source positions the interpolator is given, and an interpolator that skimps on it produces
worse diagnostics than a runtime parser would have.

**Literals must be literals.** A URL assembled from parts at compiletime cannot use the
interpolator's checking unless the parts are themselves literals. The runtime path is
always available, but the guarantee is weaker, and the shape of the code differs — which
occasionally pushes a design toward keeping something literal that would otherwise have
been computed.

See [impossible states](impossible-states.md) for the rule this serves, and
[interpolation](../modules/interpolation.md) for how such a literal is built.
