# A token which carries a case class

The registered claims of a JWT are a `Jwt.Claims`, and an application's own are any value
which JSON can encode. The token is signed with a key from the cryptography library, its
expiry is an instant from the time library, and the claims come back as the case class.

```scala fuel
compile
frame
  label The claims
  highlight Role(t"Hamlet",..t"Hamlet")
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable
      import formatting.compactJsonFormatting

      case class Role(part: Text, play: Text)

      val key = PrivateKey.generate[Ecdsa[256]]()
      val claims = Jwt.Claims(issuer = t"globe", subject = t"burbage", expiry = now() + 1*Hour)

      val jwt: Jwt = Jwt(claims, Role(t"Hamlet", t"Hamlet"))
frame
  label Signed, as text
  highlight sign(key).in[Text]
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable
      import formatting.compactJsonFormatting

      case class Role(part: Text, play: Text)

      val key = PrivateKey.generate[Ecdsa[256]]()
      val claims = Jwt.Claims(issuer = t"globe", subject = t"burbage", expiry = now() + 1*Hour)

      val token: Text = Jwt(claims, Role(t"Hamlet", t"Hamlet")).sign(key).in[Text]
frame
  label Verified, and read back
  highlight json.as[Role]
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable
      import formatting.compactJsonFormatting

      case class Role(part: Text, play: Text)

      val key = PrivateKey.generate[Ecdsa[256]]()
      val claims = Jwt.Claims(issuer = t"globe", subject = t"burbage", expiry = now() + 1*Hour)

      val token: Text = Jwt(claims, Role(t"Hamlet", t"Hamlet")).sign(key).in[Text]
      val role: Role = Jwt.verify(token, key.public).json.as[Role]
##
```

```scala fuel
run
##
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap

Out.println(role.inspect)
```

[Tokens](../../soundness/modules/jose.md#tokens) describes what `verify` checks, and
[composability](../../philosophy/composability.md) why the libraries fit.
