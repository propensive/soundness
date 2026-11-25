# Versions in their own order

Versions are compared with `<`, as numbers are, but by the rules of the specification, which
are not those of numbers or of text. Ten follows nine, a release follows its own release
candidate, and build metadata counts for nothing.

```scala fuel
compile
frame
  label Numbers compare as numbers
  highlight <  ten is more than nine
  code
      val minor: Boolean = v"1.9.0" < v"1.10.0"
frame
  label A release outranks its prereleases
  highlight v"1.0.0-rc.1"..v"1.0.0"  the candidate comes first
  code
      val minor: Boolean = v"1.9.0" < v"1.10.0"
      val candidate: Boolean = v"1.0.0-rc.1" < v"1.0.0"
frame
  label Build metadata is not compared
  highlight ==  the same version, built twice
  code
      val minor: Boolean = v"1.9.0" < v"1.10.0"
      val candidate: Boolean = v"1.0.0-rc.1" < v"1.0.0"
      val rebuilt: Boolean = v"1.0.0+quarto" == v"1.0.0+folio"
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(minor.inspect)
Out.println(candidate.inspect)
Out.println(rebuilt.inspect)
```

All three are true. [Ordering](../../soundness/modules/versioning.md#ordering) has more of the
rules of precedence.
