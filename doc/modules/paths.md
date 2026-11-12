## Paths

### About

A path is a sequence of names which leads through a hierarchy, and the facts about a path are
kept in its type:

- An absolute path is a `Path`, rooted on some value.
- A relative path is a `Relative`: a number of steps up, followed by a descent.
- Both are typed by the *platform* they belong to, so a `Path on Linux` and a `Path on Windows`
  are distinct types. The `on` clause is one of the
  [infix types](../philosophy/infix-types.md) used throughout.

The elements of a path which is built in source code are known to the compiler. Arithmetic on
paths, such as descending, ascending, and finding the route from one path to another, is
therefore checked at [compiletime].

These are abstract paths. They are about the *shape* of a path, not about a file on a disk.
The same machinery underlies filesystem paths, URLs and any other hierarchy. Reading and
writing actual files is the business of the [filesystem](filesystem.md).

### On paths

A path raises three questions: is it absolute or relative, which platform is it for, and does
this `..` climb above the root? In most libraries, none of the answers is in the type. A path
is a string, or a class which erases the distinctions. A mistake, such as joining two absolute
paths, taking the parent of a root, or mixing a Windows path into a POSIX one, is found at
[runtime], if at all.

Here each distinction is recorded in the type. Absolute and relative paths are different
types, so one cannot be mistaken for the other. The platform is a type parameter, so its
separators and rules travel with the path. And where a path's elements are known, they are
known to the compiler, which computes the result of joining two paths, or of finding the route
between them, statically.

Everything comes from the `soundness` package. The samples throw any error as an exception,
and the last import lets them print their results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Building a path

A path is written with a handful of symbols, each described below:

```tabular
column  Symbol
column  Means
row  `%`  the POSIX filesystem root
row  `Drive('C')`  the root of a Windows drive
row  `/`  a step down, to a named child
row  `?`  the current location, from which a relative path starts
row  `^`  a step up
row  `+`  a relative path applied to an absolute one
row  `/:`  in a pattern, a name peeled from the front
```

An absolute path descends from a root with `/`. The root `%` is the root of a POSIX
filesystem, and a Windows path descends from a `Drive`:

```scala
val work = % / "home" / "work"
val windows = Drive('C') / "Windows"
```

```scala fuel
run
##
Out.println(work.inspect)
Out.println(windows.inspect)
```

The `p"…"` [interpolator] writes a path literally, and checks it at compiletime. It resolves
the literal as a POSIX or a Windows path, by its shape:

```scala
val data = p"/home/user/data.csv"
val system = p"""C:\Windows\System32\file.txt"""
```

The elements of a literal are part of its type, with the last element first:
`data` is a `Path of ("data.csv", "user", "home") on Posix`. A path built with `/` likewise
carries each element it was given.

### Relative paths

A relative path is built from `?`, the current location, and `^`, a step upward, followed by a
descent. Adding a relative path to an absolute one applies the ascent, and then the descent,
to give a new absolute path:

```scala
val up = ? / ^ / "sibling"
```

```scala fuel
run
##
Out.println((work + up).inspect)
```

### Platforms

A path built with `/` is at first neutral between platforms. `on` binds it to a platform, and
checks each element against that platform's rules, which reject a name containing a slash, or
a reserved name:

```scala
val linux: Path on Linux = (% / "home" / "work").on[Linux]
```

`Linux`, `Posix`, `MacOs` and `Windows` are the platforms, and `Local` is whichever of them the
running machine uses. [Names](names.md) describes how each platform's rules for an element
are stated and checked.

### Between two paths

`toward` computes the relative path from one location to another, and `conjunction` finds
their closest common ancestor:

```scala
val a = % / "home" / "work" / "data"
val b = % / "home" / "work" / "more"
```

```scala fuel
run
##
Out.println(a.toward(b).inspect)
Out.println(a.conjunction(b).inspect)
```

### Text and paths

`encode` writes a path as text. `as` parses text to a path, naming the platform whose rules
apply:

```scala fuel
run
##
Out.println((Drive('D') / "Verona").encode)
Out.println(t"/home/work".as[Path on Linux].encode)
```

Text which breaks the platform's rules is not a path, and does not
[decode](decoding.md#decoding-a-value):

```scala fuel
run
errors show
##
t"home/work".as[Path on Linux]
```

### Deconstructing

A path is matched in a pattern with the extractor `/:`, which peels names from the front. A
path can therefore be taken apart as readily as it is built:

```scala
val path = t"/home/work".as[Path on Linux under %.type]

val remainder = path match
  case root /: t"home" /: rest => rest
  case _                       => path.relative
```

```scala fuel
run
##
Out.println(remainder.inspect)
```

The root is matched too. That matters where a platform has more than one root, such as a
Windows drive letter or a UNC share, since the root is part of what a path *is*, and not a
prefix of its text.

### Paths in the type

A path's element names may be carried in its type, so that a path which is known at
compiletime is known precisely:

```scala
val exact: Path of ("capulet", "verona") = % / "verona" / "capulet"
```

The names appear innermost first, which matches how the path is constructed. A path which is
built entirely from literals therefore has a type which describes exactly where it points.
That is what lets a [classpath](classpath.md) resource, or a file relative to a project, be
checked as the code compiles. A path whose elements do not match the type it is given does
not compile:

```scala fuel
compile
errors show
##
val wrong: Path of ("capulet", "verona") = % / "verona" / "montague"
```

Where the elements are not known, as for a path built from text at runtime, or one read from
a [filesystem](filesystem.md) listing, the type simply says `Path on Linux`, and the operations
are the same.

### What a relative path knows

A relative path carries its platform too, so a `Relative on Linux` cannot be applied to a
Windows path. It also carries how far up it reaches, in its `Relation`. A relative path which
would ascend above a root can therefore be rejected where it is written, instead of resolving
to nowhere at runtime.

### Related

- [Filesystem](filesystem.md) reads, writes and lists the files which paths name.
- [Network addresses](network-addresses.md#urls) describes URLs, which have paths of their
  own.
- [Classpath](classpath.md) names resources by path.
- [Names](names.md) checks the elements of a path against a platform's rules.
