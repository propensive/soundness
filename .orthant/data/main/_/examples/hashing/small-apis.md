# One method hashes any value

Nothing is serialized by hand before it is hashed. `digest` is given the algorithm as a type,
and works on text, on bytes, and on another digest.

```scala fuel
compile
frame
  label Text
  highlight digest[Sha2[256]]  the algorithm, as a type
  code
      import providers.javaBaseProvider, codepages.utf8Codepage, alphabets.hexLowerCase

      val line = t"The rest is silence"
      val spoken = line.digest[Sha2[256]]
frame
  label Bytes, with another algorithm
  highlight digest[Sha2[512]]  another input, another algorithm
  code
      import providers.javaBaseProvider, codepages.utf8Codepage, alphabets.hexLowerCase

      val line = t"The rest is silence"
      val spoken = line.digest[Sha2[256]]
      val written = line.in[Data].digest[Sha2[512]]
frame
  label A digest of a digest
  highlight spoken.digest[Sha2[256]]  a digest is digestible too
  code
      import providers.javaBaseProvider, codepages.utf8Codepage, alphabets.hexLowerCase

      val line = t"The rest is silence"
      val spoken = line.digest[Sha2[256]]
      val written = line.in[Data].digest[Sha2[512]]
      val twice = spoken.digest[Sha2[256]]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(spoken.serialize[Hex])
Out.println(twice.serialize[Hex])
```

[Hashing a value](../../soundness/modules/hashing.md#hashing-a-value) lists the algorithms.
