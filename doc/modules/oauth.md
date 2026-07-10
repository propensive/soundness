## OAuth

### About

[OAuth 2.0](https://en.wikipedia.org/wiki/OAuth) lets an application act on a user's behalf at
another service, with the user's consent and without the user's password. Soundness implements the
authorization-code flow as a wrapper around an [HTTP](http-server.md) handler: code that requires
an authorization simply declares the scopes it needs, and the redirect dance — sending the user to
the provider, exchanging the returned code for tokens, refreshing an expired access token —
happens around it.

An `Authorization` carries its scopes in its type, so a handler that requires a scope cannot run
without an authorization that grants it, and the access token flows into outgoing requests as a
standard bearer header.

### On OAuth

The protocol is a chain of redirects and exchanges, each with its own failure modes: the initial
redirect with the right parameters, the callback carrying a single-use code, the back-channel
exchange of code for tokens, expiry tracking, and the refresh exchange when an access token ages
out. Implemented ad hoc, the steps scatter across handlers, and the question "is this request
authorized for what it is about to do?" is answered by convention.

Soundness structures the flow around the handler that needs it. The provider is an `Issuer` value;
sessions carry the per-user state; and scope requirements are types, checked where the protected
code is written. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Defining an issuer

An `Issuer` names the provider's endpoints — where users authorize, where codes are exchanged —
and the application's client credentials and callback:

```scala
val issuer = Issuer
  ( init     = url"https://provider.example/oauth/authorize",
    exchange = url"https://provider.example/oauth/token",
    redirect = url"https://app.example/callback",
    client   = clientId,
    secret   = clientSecret )
```

### Protecting a handler

`oauth` wraps an HTTP handler, intercepting the provider's callback and performing the exchanges;
inside it, `require` states the scopes a block needs. A request without a valid authorization is
redirected to the provider; one with it runs the block, the `Authorization` in scope:

```scala
val readUser = Scope(t"read:user")

issuer.oauth:
  issuer.require(readUser):
    Http.Response(Http.Ok)(profilePage)
```

An expired access token refreshes automatically through the refresh token, so a long-lived session
does not send its user back through the consent screen.

### Using the authorization

The `Authorization` in scope carries the access token and granted scopes, and supplies the
standard `Authorization: Bearer …` header to outgoing [HTTP](http-client.md) requests, so calling
the provider's API on the user's behalf is an ordinary fetch with the authorization applied.
Asking an authorization for a scope it does not hold raises an `OAuthError`, whose reasons also
cover the flow's other failures — a denied consent, an unexpected status from the provider, a
response that would not parse — each named for diagnosis.
