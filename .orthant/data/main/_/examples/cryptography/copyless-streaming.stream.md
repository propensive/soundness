# A stream encrypted a chunk at a time

`encrypt` applies to a stream as it does to a value, and gives a stream of ciphertext. Each chunk
is encrypted as it passes, so data too large to hold is never assembled, and what was encrypted
in pieces decrypts as a whole.

```scala fuel
compile
frame
  label A whole value, encrypted and decrypted
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7, soundness.textDecodable
      import cryptoPermits.permitUnauthenticatedCrypto, codepages.utf8Codepage
      import charsets.utf8Charset, textSanitizers.skipSanitizer

      def oath(key: SymmetricKey[Aes[256]]): Text = key.uncloak:
        t"I am not what I am".encrypt(InitializationVector.random).decrypt.as[Text]
frame
  label The same, through a stream of ciphertext
  highlight .stream.encrypt
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import blockCipherModes.cbc, blockCipherPaddings.pkcs7, soundness.textDecodable
      import cryptoPermits.permitUnauthenticatedCrypto, codepages.utf8Codepage
      import charsets.utf8Charset, textSanitizers.skipSanitizer

      def oath(key: SymmetricKey[Aes[256]]): Text = key.uncloak:
        val secret = t"I am not what I am".in[Data]
        secret.stream.encrypt(InitializationVector.random).memoize.decrypt.as[Text]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(oath(SymmetricKey.generate[Aes[256]]()))
```

[Symmetric encryption](../../soundness/modules/cryptography.md#symmetric-encryption) describes
keys and the block in which they are used.
