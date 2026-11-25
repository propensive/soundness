# The cipher which an identity is verified with

An ID token says in its own header how it was signed, and a verifier which believed it could
be told anything. `Oidc.identify` takes the cipher as a type, reads the provider's keys as
keys of that cipher, and accepts its one signature algorithm.

```scala fuel
compile
alternative RSA
  highlight Rsa[2048]  accepts RS256, and nothing else
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics
      import providers.javaBaseProvider, cloaks.heapCloak

      def subject(authorization: Authorization, keys: Jwks): Optional[Text] =
        Oidc.identify[Rsa[2048]](authorization, keys, t"https://accounts.example", t"globe")
        . claims.subject
alternative ECDSA
  highlight Ecdsa[256]  accepts ES256, and nothing else
  code
      import strategies.throwUnsafely, errorDiagnostics.stackTracesDiagnostics
      import providers.javaBaseProvider, cloaks.heapCloak

      def subject(authorization: Authorization, keys: Jwks): Optional[Text] =
        Oidc.identify[Ecdsa[256]](authorization, keys, t"https://accounts.example", t"globe")
        . claims.subject
##
```

[Identifying the user](../../soundness/modules/oauth.md#identifying-the-user) lists what else
is checked, and [JOSE](../../soundness/modules/jose.md#on-choosing-the-algorithm) explains why
the key's type decides.
