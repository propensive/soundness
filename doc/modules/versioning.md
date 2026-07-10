## Versioning

### About

A [semantic version](https://semver.org/) is a structured claim — this major, this minor, this
patch, perhaps a prerelease — with precise rules for validity and ordering, and `Semver` implements
them exactly. A version literal is checked as the code compiles, versions compare by the
specification's precedence rules, and text parses with a typed error naming what was malformed.
Alongside versions, JAR *manifests* read and write as typed values, their standard attributes
carrying real types — a `Main-Class` is an [Fqcn](stack-traces.md), not a string.

### On version numbers

Version strings invite string thinking: split on dots, compare lexically, and get `1.10.0` sorting
before `1.9.0`. The semantic-versioning specification pins down both the grammar — where leading
zeros are illegal, what a prerelease identifier may contain — and the precedence rules, including
the subtle ones: numeric identifiers compare numerically, `1.0.0-alpha` precedes `1.0.0`, and build
metadata never affects ordering. Rules that precise belong in a type.

Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Versions

The `v"…"` interpolator writes a version, checked as the code compiles; text parses to the same
type, raising a `SemverError` for a malformed version:

```scala
val version = v"1.4.2"
t"2.0.0-rc.1".decode[Semver]

v"1.2"   // does not compile: not a semantic version
```

Ordering follows the specification exactly:

```scala
v"1.0.0-alpha" < v"1.0.0-alpha.1"   // true
v"1.0.0-beta.2" < v"1.0.0-beta.11"  // true — numeric identifiers compare numerically
v"1.0.0-rc.1" < v"1.0.0"            // true — a release outranks its prereleases
```

A version knows whether it is still in initial `development` — major zero, where anything may
change — and `release` strips the prerelease and build parts.

### Manifests

A JAR's `META-INF/MANIFEST.MF` reads and writes as a `Manifest` of typed attributes. The standard
attributes are named values, and reading one gives its proper type:

```scala
import manifestAttributes.*

val manifest = Manifest
  ( ManifestVersion(),
    MainClass(fqcn"com.example.Main"),
    CreatedBy(t"Soundness") )

manifest(MainClass)   // an Optional[Fqcn]
```

A manifest read from a JAR parses through the same type, so tooling that inspects or rewrites
manifests — [packaging](packaging.md) does both — works with values rather than the format's
fussy line-wrapping rules.
