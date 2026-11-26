# A key which is lent to a block

A key's bytes are never handed out. `uncloak` lends the key to a block, as the capability to
encrypt and decrypt, and outside the block there is nothing to encrypt with.

```scala fuel
compile
frame
  label Encryption, with no key lent
  errors show
  code
      import cloaks.heapCloak, providers.javaBaseProvider, codepages.utf8Codepage
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7
      import cryptoPermits.permitUnauthenticatedCrypto

      val key = SymmetricKey.generate[Aes[256]]()

      val letter = t"Meet me at the tomb".encrypt(InitializationVector.random)
frame
  label Inside the block which lends it
  focus key.uncloak:
  code
      import cloaks.heapCloak, providers.javaBaseProvider, codepages.utf8Codepage
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7
      import cryptoPermits.permitUnauthenticatedCrypto

      val key = SymmetricKey.generate[Aes[256]]()

      val letter = key.uncloak:
        t"Meet me at the tomb".encrypt(InitializationVector.random)
##
```

[Symmetric encryption](../../soundness/modules/cryptography.md#symmetric-encryption) decrypts
the message again, and [delimited scopes](../../philosophy/delimited-scopes.md) describes the
block.
