# The key's type chooses the algorithm

A signed object's header names its algorithm, and nothing in the code says which. It is
computed from the type of the key: an `Ecdsa[256]` key signs `ES256`, an `Ed25519` key signs
`EdDSA`, and an HMAC secret over SHA-256 signs `HS256`.

```scala fuel
compile
frame
  label An elliptic-curve key
  highlight Ecdsa[256]
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable

      val line = t"Brevity is the soul of wit"

      val curve: Jws = Jws(line, PrivateKey.generate[Ecdsa[256]]())
frame
  label An Edwards-curve key
  highlight Ed25519
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable

      val line = t"Brevity is the soul of wit"

      val curve: Jws = Jws(line, PrivateKey.generate[Ecdsa[256]]())
      val edwards: Jws = Jws(line, PrivateKey.generate[Ed25519]())
frame
  label A shared secret
  highlight HmacCipher[Sha2[256]]
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable

      val line = t"Brevity is the soul of wit"

      val curve: Jws = Jws(line, PrivateKey.generate[Ecdsa[256]]())
      val edwards: Jws = Jws(line, PrivateKey.generate[Ed25519]())
      val secret: Jws = Jws(line, SymmetricKey.generate[HmacCipher[Sha2[256]]]())
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(curve.header.algorithm.or(t"none"))
Out.println(edwards.header.algorithm.or(t"none"))
Out.println(secret.header.algorithm.or(t"none"))
```

[Signing](../../soundness/modules/jose.md#signing) has the table of keys and algorithms.
