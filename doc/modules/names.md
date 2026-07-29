## Names

### About

A filename, a DOM id, a CSS class, a Git branch: each is text, but not just any text. Each lives
in a namespace with its own rules about what a valid name looks like, and Soundness represents
such a name as a `Name` whose rules are part of its type. A `Name[DomId]` is known to satisfy the
rules for DOM ids; a literal that breaks them is a compile error; and text arriving at runtime
becomes a `Name` only by passing the same validation, with a typed error naming the exact rule it
broke.

The rules themselves are types — `MustNotContain["/"]`, `MustNotEqual[".."]`, `MustMatch[…]` —
composed by intersection, so a namespace's constraints are written once, read directly from the
code, and enforced everywhere a name of that namespace is required.

### On names

Namespaces disagree about validity. A POSIX filename may not contain a slash; a DOM id may not
contain whitespace; a DNS label has a length limit and a restricted alphabet. Code that carries
all of these as plain strings enforces none of it, so a name that is invalid in its namespace
travels until something external — a filesystem, a browser, a server — rejects it at a distance
from the mistake.

Soundness attaches the namespace to the name. Each namespace — a *plane* — declares its rules as
a type, and a `Name` for that plane exists only if its text satisfies them: at compiletime for a
literal, at runtime by validated decoding. This is the mechanism beneath the element names of
[paths](paths.md), and it is open to any namespace a program defines. Everything comes from the
`soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Defining a plane

A plane is a marker type with a `Nominative` given stating its rules as an intersection:

```scala
sealed trait Tag

inline given Tag is Nominative under
    MustNotStart["0"] & MustNotContain["."] & MustNotEqual[""] = !!
```

A `Name[Tag]` now means text that does not start with `0`, contains no `.`, and is not empty —
facts carried by the type, not re-checked by every consumer.

### Writing a name

The `n"…"` interpolator writes a name and checks it against the planes in scope as the code
compiles. The name takes the type of every plane whose rules it satisfies, so one literal serves
wherever any of them is required, and a literal that satisfies no plane does not compile:

```scala
val tag: Name[Tag] = n"release"

val bad: Name[Tag] = n"0hello"   // does not compile: must not start with 0
```

### Validating at runtime

Text from outside becomes a name through the same rules, raising a `NameError` that names the
rule violated:

```scala
Name[Tag](t"release")   // a Name[Tag]

capture[NameError](Name[Tag](t"0hello")).message.show
// t"the name 0hello is not valid because it must not start with 0"
```

The error's precision matters: a user told *which* rule their input broke can fix it, where
"invalid name" invites guesswork.

### Built-in planes

Some namespaces come already defined. `DomId` and `CssClass` validate the identifiers of
[HTML](html.md) and [CSS](css.md); `Required` means only "not empty", for the common case where
that is the whole rule:

```scala
Name[CssClass](t"main-nav")   // a valid CSS identifier

capture[NameError](Name[DomId](t"a b")).message.show
// t"the name a b is not valid because it must be a valid DOM id"
```

The filesystem [paths](paths.md) of each platform define their element rules the same way, which
is how a path element containing a slash fails where it is written.

### The rules

`MustContain`, `MustNotContain`, `MustStart`, `MustNotStart`, `MustEnd`, `MustNotEnd`,
`MustEqual`'s negation `MustNotEqual`, and the regular-expression pair `MustMatch` and
`MustNotMatch` cover most namespaces, and each knows how to describe itself — which is where the
error messages, and the compile errors, get their words. A namespace with a rule none of these
express defines its own, as `DomId` does, by pairing a predicate with its description.

A rule is therefore two things: a check, and a phrase describing what it demands. Writing one is
supplying both:

```scala
object MustEnd extends Rule({ text => m"must end with $text" }, _.ends(_))
```

Because the description is part of the rule rather than written at each use, the compile error for
a bad literal and the runtime error for bad input say the same thing, in the same words.

`JavaIdentifier` shows the other shape a rule can take: rather than parameterizing on text to
compare against, it parameterizes on the *description*, and checks against a predicate of its own —
which is what a rule expressible as a predicate but not as a comparison needs.

### Combining rules

A plane's rules conjoin: a name must satisfy all of them, and the first violated is the one
reported, so an error names a single concrete fault rather than a list of everything wrong.
Writing a plane is therefore stating its rules in the order they should be reported.

Because a plane is a type, one plane's names are not another's. A `Name[Tag]` cannot be passed
where a `Name[Branch]` is expected even if the two happen to have identical rules — which is the
point, since the rules may diverge later and the two namespaces were never the same thing.

### Length limits

A `Nominative` carries a `Limit` alongside its `Self`, which is how a plane states a maximum
length: a filesystem whose names may not exceed 255 bytes, a protocol field of fixed width, a
database column. The limit is part of the plane's type, so it is checked where a name is
constructed rather than where it is finally written out.
