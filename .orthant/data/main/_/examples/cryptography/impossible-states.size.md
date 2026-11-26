# Only the key sizes a cipher has

AES-GCM has keys of 128 and 256 bits. The size is a number in the key's type, so a key of a
size the cipher does not define cannot be generated, and no check for it is made as the
program runs.

```scala fuel
compile
frame
  label A key of 256 bits
  highlight AesGcm[256]  a size the cipher defines
  code
      import cloaks.heapCloak, providers.javaBaseProvider

      val key = SymmetricKey.generate[AesGcm[256]]()
frame
  label A key of 200 bits
  errors show
  code
      import cloaks.heapCloak, providers.javaBaseProvider

      val key = SymmetricKey.generate[AesGcm[200]]()
##
```

[Weak algorithms](../../soundness/modules/cryptography.md#weak-algorithms) lists the sizes of
every cipher.
