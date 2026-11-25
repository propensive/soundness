# The provider decides which algorithms exist

The algorithms come from a provider, which is chosen by an import. The JDK has no BLAKE3, so
under its provider a BLAKE3 digest is not a runtime failure but a compile error.

```scala fuel
compile
frame
  label The JDK's algorithms
  errors show
  code
      import providers.javaBaseProvider, codepages.utf8Codepage

      val fingerprint = t"Hello world".digest[Blake3]
frame
  label The algorithms written in Scala
  highlight soundnessProvider  pure Scala, on every platform
  code
      import providers.soundnessProvider, codepages.utf8Codepage

      val fingerprint = t"Hello world".digest[Blake3]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
import alphabets.hexLowerCase

Out.println(fingerprint.serialize[Hex])
```

[Hashing a value](../../soundness/modules/hashing.md#hashing-a-value) has the table of
providers, and [BLAKE3](../../soundness/modules/hashing.md#blake3) the modes of this one.
