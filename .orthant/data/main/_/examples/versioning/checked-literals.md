# A version the compiler has read

A semantic version has a grammar: three numbers, none with a leading zero, and perhaps a
prerelease. A `v"…"` literal is read by that grammar as the code compiles, so a version which
the specification forbids is never a value.

```scala fuel
compile
frame
  label Two numbers
  errors show
  code
      val folio = v"1.2"
frame
  label A leading zero
  errors show
  code
      val folio = v"1.02.0"
frame
  label Three numbers, and a prerelease
  highlight -rc.1  a prerelease
  code
      val folio = v"1.2.0-rc.1"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(folio.minor.show)
Out.println(folio.release.show)
```

[Versions](../../soundness/modules/versioning.md#versions) reads the same grammar at runtime,
and [safety by construction](../../philosophy/safety-by-construction.md) explains the
literal.
