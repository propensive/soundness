# An unauthenticated cipher needs a permit

AES in CBC mode keeps a message secret, and does not detect a message which was altered.
Encrypting with it needs a permit in scope, so each concession to older cryptography is an
import which can be searched for.

```scala fuel
compile
frame
  label AES-CBC, with nothing to permit it
  errors show
  code
      import cloaks.heapCloak, providers.javaBaseProvider, codepages.utf8Codepage
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7

      val key = SymmetricKey.generate[Aes[256]]()

      val letter = key.uncloak:
        t"Meet me at the tomb".encrypt(InitializationVector.random)
frame
  label The permit
  highlight permitUnauthenticatedCrypto  a searchable concession
  code
      import cloaks.heapCloak, providers.javaBaseProvider, codepages.utf8Codepage
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7
      import cryptoPermits.permitUnauthenticatedCrypto

      val key = SymmetricKey.generate[Aes[256]]()

      val letter = key.uncloak:
        t"Meet me at the tomb".encrypt(InitializationVector.random)
##
```

[Weak algorithms](../../soundness/modules/cryptography.md#weak-algorithms) lists what each
permit allows.
