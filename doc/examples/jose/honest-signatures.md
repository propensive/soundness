# A private key leaves only where the code says so

A public key is written as a JWK with no ceremony. A private key is written only when the
`Divulgence` token is passed with it, so every place where a secret is serialized can be
found by searching for one word.

```scala fuel
compile
frame
  label The public half
  highlight key.public
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable
      import formatting.compactJsonFormatting

      val key = PrivateKey.generate[Ecdsa[256]]()

      val published: Jwk = Jwk(key.public)
frame
  label The private half, by accident
  errors show
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable
      import formatting.compactJsonFormatting

      val key = PrivateKey.generate[Ecdsa[256]]()

      val published: Jwk = Jwk(key)
frame
  label The private half, on purpose
  highlight Divulgence
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable
      import formatting.compactJsonFormatting

      val key = PrivateKey.generate[Ecdsa[256]]()

      val published: Jwk = Jwk(key.public)
      val divulged: Jwk = Jwk(key, Divulgence)
##
```

[Keys as JSON](../../soundness/modules/jose.md#keys-as-json) describes a `Jwk`, and
[honest signatures](../../philosophy/honest-signatures.md) the principle.
