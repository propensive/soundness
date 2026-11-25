# A secret is not a public key

The best-known attack on signed tokens presents a public key as though it were an HMAC
secret, and relies on the verifier doing what the header says. Here the header is only
compared with the algorithm of the key, and the refusal says which two disagreed.

```scala fuel
compile
frame
  label A token signed with an elliptic-curve key
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable

      val key = PrivateKey.generate[Ecdsa[256]]()
      val compact: Text = Jws(t"Brevity is the soul of wit", key).in[Text]
frame
  label Verified with a secret
  highlight verify(secret)
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable

      val key = PrivateKey.generate[Ecdsa[256]]()
      val compact: Text = Jws(t"Brevity is the soul of wit", key).in[Text]

      val secret = SymmetricKey.generate[HmacCipher[Sha2[256]]]()
      val refusal = capture[Jose.Error](compact.as[Jws].verify(secret))
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(refusal.message.text)
```

[On choosing the algorithm](../../soundness/modules/jose.md#on-choosing-the-algorithm)
describes both attacks.
