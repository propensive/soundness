# A verified token, inside the block it guards

`bearerAuth` runs its block only for a request whose token verifies, and within the block the
token is in scope as `jwt`. Outside it there is no token to read, so a response cannot be
built from claims which nobody checked.

```scala fuel
compile
frame
  label Claims, where no token was verified
  errors show
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable

      def greet(key: PublicKey[Ecdsa[256]])(using Http.Connection): Http.Response =
        Http.Response(Http.Ok)(t"Welcome, ${jwt.claims.subject.or(t"stranger")}")
frame
  label Claims, within the guard
  highlight bearerAuth..t"groundlings")
  code
      import cloaks.heapCloak, providers.javaBaseProvider, strategies.throwUnsafely
      import codepages.utf8Codepage, charsets.utf8Charset, textSanitizers.skipSanitizer
      import errorDiagnostics.stackTracesDiagnostics, soundness.textDecodable

      def greet(key: PublicKey[Ecdsa[256]])(using Http.Connection): Http.Response =
        bearerAuth(key, t"globe", audience = t"groundlings"):
          Http.Response(Http.Ok)(t"Welcome, ${jwt.claims.subject.or(t"stranger")}")
##
```

[Bearer authentication](../../soundness/modules/jose.md#bearer-authentication) describes what
a request without a valid token is sent, and
[delimited scopes](../../philosophy/delimited-scopes.md) the idea.
