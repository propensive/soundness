# A payload which cannot be read unverified

A `Jws` read from text is only a claim that someone signed something. It has no method which
gives the payload without a key, so code which forgets to check the signature has nothing to
read.

```scala fuel
compile
frame
  label The payload, taken on trust
  errors show
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable

      val key = PrivateKey.generate[Ecdsa[256]]()
      val compact: Text = Jws(t"Brevity is the soul of wit", key).in[Text]

      val line: Text = compact.as[Jws].payload.read[Text]
frame
  label The payload, of a signature which verifies
  highlight verify(key.public)
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable

      val key = PrivateKey.generate[Ecdsa[256]]()
      val compact: Text = Jws(t"Brevity is the soul of wit", key).in[Text]

      val line: Text = compact.as[Jws].verify(key.public).read[Text]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(line)
```

[Signing](../../soundness/modules/jose.md#signing) describes `verify`, and
[impossible states](../../philosophy/impossible-states.md) the idea.
