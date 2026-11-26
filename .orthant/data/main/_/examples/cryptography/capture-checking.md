# A key encrypts only inside its block

`uncloak` lends a key to a block, as the capability to encrypt and decrypt. A ciphertext made in
the block is data and may leave. A function which would go on encrypting afterwards carries the
capability with it, and the compiler stops it at the edge of the block.

```scala fuel
compile
feature experimental.captureChecking
frame
  label A function which keeps the key in use
  errors show
  highlight text.encrypt..random)  would encrypt after the block had ended
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7
      import cryptoPermits.permitUnauthenticatedCrypto, codepages.utf8Codepage

      def seal(key: SymmetricKey[Aes[256]]) = key.uncloak:
        (text: Text) => text.encrypt(InitializationVector.random)
frame
  label A message, encrypted while the key is lent
  highlight .encrypt
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7
      import cryptoPermits.permitUnauthenticatedCrypto, codepages.utf8Codepage

      def seal(key: SymmetricKey[Aes[256]]) = key.uncloak:
        t"I am not what I am".encrypt(InitializationVector.random)
##
```

[Symmetric encryption](../../soundness/modules/cryptography.md#symmetric-encryption) describes
keys, modes and paddings, and [delimited scopes](../../philosophy/delimited-scopes.md) the idea
of lending a capability to a block.
