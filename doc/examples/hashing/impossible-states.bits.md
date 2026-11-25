# Only the sizes which SHA-2 defines

SHA-2 is a family of four digests, of 224, 256, 384 and 512 bits. The size is a number in the
type, and a number the standard does not define is not a hash that can be asked for.

```scala fuel
compile
frame
  label A size which exists
  highlight Sha2[384]  one of the four
  code
      import providers.javaBaseProvider, codepages.utf8Codepage

      val fingerprint = t"Hello world".digest[Sha2[384]]
frame
  label A size which does not
  errors show
  code
      import providers.javaBaseProvider, codepages.utf8Codepage

      val fingerprint = t"Hello world".digest[Sha2[300]]
##
```

[Hashing a value](../../soundness/modules/hashing.md#hashing-a-value) has the table of
algorithms and their sizes.
