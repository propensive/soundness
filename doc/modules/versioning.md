## Versioning

### About

A [semantic version](https://semver.org/) is a structured claim: this major version, this
minor, this patch, and perhaps a prerelease. The specification has precise rules for validity
and for ordering, and `Semver` implements them exactly. A version literal is checked at
[compiletime], versions compare by the specification's rules of precedence, and text is parsed
with a typed error.

JAR *manifests* are handled here too. They are read and written as typed values, whose standard
attributes have real types: a `Main-Class` is an [Fqcn](stack-traces.md), not a string.

### On version numbers

Version strings invite thinking in strings: split on dots, compare the characters, and
`1.10.0` sorts before `1.9.0`. The semantic versioning specification pins down both the grammar
and the rules of precedence, including the subtle ones:

- Leading zeros are illegal, and a prerelease identifier may contain only certain characters.
- Numeric identifiers compare numerically.
- `1.0.0-alpha` precedes `1.0.0`.
- Build metadata never affects ordering.

Rules that precise belong in a type. A version which cannot be constructed invalid, and which
compares as the specification says, is
[safety by construction](../philosophy/safety-by-construction.md).

Everything comes from the `soundness` package. The samples throw any error as an exception, and
the last import lets them print their results:

```scala
import soundness.*
import strategies.throwUnsafely
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Versions

The `v"…"` [interpolator] writes a version, and checks it at compiletime:

```scala fuel
compile
##
val version = v"1.4.2"
```

A literal which is not a semantic version does not compile:

```scala fuel
compile
errors show
##
v"1.2"
```

Text is [decoded](decoding.md#decoding-a-value) to the same type with `as[Semver]`, and a
malformed version raises a `Semver.Error`:

```scala fuel
run
##
Out.println(t"2.0.0-rc.1".as[Semver].show)
```

```scala fuel
run
errors show
##
t"2.0".as[Semver]
```

### Ordering

Ordering follows the specification exactly. A longer prerelease follows a shorter one, numeric
identifiers compare as numbers, and a release outranks its own prereleases:

```scala fuel
compile
run
##
Out.println((v"1.0.0-alpha" < v"1.0.0-alpha.1").inspect)
Out.println((v"1.0.0-beta.2" < v"1.0.0-beta.11").inspect)
Out.println((v"1.0.0-rc.1" < v"1.0.0").inspect)
Out.println((v"1.9.0" < v"1.10.0").inspect)
```

A version knows whether it is still in initial `development`, with a major version of zero,
when anything may change. `release` strips the prerelease and build parts.

### Manifests

A JAR's `META-INF/MANIFEST.MF` is read and written as a `Manifest` of typed attributes. The
standard attributes are named values, and reading one gives a value of its proper type:

```scala
import manifestAttributes.*

val manifest = Manifest
  ( ManifestVersion(()),
    MainClass(fqcn"com.example.Main"),
    CreatedBy(t"Soundness") )

val main: Optional[Fqcn] = manifest(MainClass)
```

A manifest read from a JAR is parsed through the same type. Tools which inspect or rewrite
manifests, as [Packaging](packaging.md) does, therefore work with values, not with the format's
fussy rules for wrapping lines. [Archives](archives.md) reads and writes the JAR itself.
