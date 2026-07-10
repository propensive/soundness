## Paths

### About

A path is a sequence of names leading through a hierarchy, and Soundness keeps the facts about
it in its type. An absolute path is a `Path`, rooted on some value; a relative path is a
`Relative`, a number of steps up followed by a descent; and both are typed by the *platform* they
belong to, so a `Path on Linux` and a `Path on Windows` are distinct. The elements of a path
built in source are known to the compiler, so path arithmetic — descending, ascending, finding the
route from one to another — is checked as the code compiles.

These are abstract paths, about the *shape* of a path rather than a file on disk. The same
machinery underlies filesystem paths, URLs, and any other hierarchy; reading and writing actual
files is the business of the [filesystem](filesystem.md).

### On paths

"Absolute or relative", "which platform", "does this `..` climb above the root" are the questions
a path raises, and in most libraries none of them is in the type. A path is a string, or a class
that erases the distinctions, and a mistake — joining two absolute paths, taking the parent of a
root, mixing a Windows path into a POSIX one — is found, if at all, at runtime.

Soundness records each distinction in the type. Absolute and relative are different types, so one
cannot be mistaken for the other; the platform is a type parameter, so its separators and rules
travel with the path; and where a path's elements are known, they are known to the compiler,
which computes the result of a join or a relativisation statically. Everything comes from the
`soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Building a path

An absolute path descends from a root with `/`. The root `%` is the POSIX filesystem root; a
Windows path descends from a `Drive`:

```scala
% / "home" / "work"      // an absolute path
Drive('C') / "Windows"   // a Windows path
```

The `p"…"` interpolator writes a path literally and checks it as the code compiles, resolving it
as a POSIX or Windows path by its shape:

```scala
p"/home/user/data.csv"                    // Path on Posix
p"""C:\Windows\System32\file.txt"""       // Path on Windows
```

### Relative paths

A relative path is built from `?`, the current location, and `^`, a step upward, then a descent.
Adding a relative path to an absolute one applies the ascent and descent to give a new absolute
path:

```scala
val up = ? / ^ / "sibling"          // up one, then down into "sibling"
(% / "home" / "work") + up          // % / "home" / "sibling"
```

### Platforms

A path built with `/` is at first platform-neutral. `on` binds it to a platform, checking each
element against that platform's rules — a name containing a slash, or a reserved name, is rejected:

```scala
(% / "home" / "work").on[Linux]   // Path on Linux
```

`Linux`, `Posix`, `MacOs` and `Windows` are the platforms, and `Local` is whichever the running
machine uses.

### Between two paths

`toward` computes the relative path from one location to another, and `conjunction` finds their
closest common ancestor:

```scala
val a = % / "home" / "work" / "data"
val b = % / "home" / "work" / "more"

a.toward(b)        // ? / ^ / "more"
a.conjunction(b)   // % / "home" / "work"
```

### Text and paths

A path renders to text with `encode`, and text parses to a path with `decode`, naming the platform
whose rules should apply. A path that breaks those rules fails to decode:

```scala
(Drive('D') / "Foo").encode          // t"D:\\Foo"
t"/home/work".decode[Path on Linux]  // % / "home" / "work"
```

### Deconstructing

A path matches in a pattern with the `/:` extractor, peeling names from the front, so a path can
be taken apart as readily as it is built:

```scala
path match
  case root /: t"home" /: rest => rest
  case _                       => path.relative
```
