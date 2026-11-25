# A claim which was not made

None of a token's registered claims is required, so each is an `Optional`. A token which
names no subject has an absent one, and the code which greets its bearer must say what to
call a stranger.

```scala fuel
compile
frame
  label The subject, used as though it were there
  errors show
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable
      import formatting.compactJsonFormatting

      val key = PrivateKey.generate[Ecdsa[256]]()
      val token: Text = Jwt(Jwt.Claims(issuer = t"globe")).sign(key).in[Text]

      val name: Text = Jwt.verify(token, key.public).claims.subject
frame
  label The subject, or a stranger
  highlight or(t"stranger")
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable
      import formatting.compactJsonFormatting

      val key = PrivateKey.generate[Ecdsa[256]]()
      val token: Text = Jwt(Jwt.Claims(issuer = t"globe")).sign(key).in[Text]

      val name: Text = Jwt.verify(token, key.public).claims.subject.or(t"stranger")
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(name)
```

[Tokens](../../soundness/modules/jose.md#tokens) lists the claims, and
[optionality](../../philosophy/optionality.md) explains `or`.
