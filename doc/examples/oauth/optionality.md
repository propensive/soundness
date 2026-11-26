# A client with no secret

A client which cannot keep a secret, such as an application on a user's own machine, proves
itself with PKCE instead. Its secret is `Unset`, which is the same absent value as everywhere
else, and not an empty text which a server might accept.

```scala fuel
compile
frame
  label A client which holds a secret
  highlight t"s3cret"
  code
      import strategies.throwUnsafely

      val issuer = Issuer(url"https://issuer.example/token", t"globe", t"s3cret",
          url"https://issuer.example/authorize", url"https://globe.example/callback")

      val destination = issuer.authorizationUrl(List(t"read"), t"a7f3c9")
frame
  label A client which holds none
  highlight Unset
  highlight pkce
    occurrence 2
  code
      import strategies.throwUnsafely

      val issuer = Issuer(url"https://issuer.example/token", t"globe", Unset,
          url"https://issuer.example/authorize", url"https://globe.example/callback")

      val pkce = Pkce()
      val destination = issuer.authorizationUrl(List(t"read"), t"a7f3c9", pkce)
##
```

[Obtaining a token](../../soundness/modules/oauth.md#obtaining-a-token) describes both steps
of the flow, and [optionality](../../philosophy/optionality.md) the absent value.
