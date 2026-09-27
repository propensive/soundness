## OAuth 2

### About

[OAuth 2](https://www.rfc-editor.org/rfc/rfc6749) is how a client is authorized to call an API on
a user's behalf, or on its own: it obtains an access token from the API's authorization server
and presents it with each request. Orthodoxy provides the client side — the grants which obtain a
token, PKCE for the authorization-code flow, and the `Credential` by which a scheme's token or key
reaches a typed client — and, for a web application, the authorization-code flow as
[scintillate](http-server.md) middleware.

### On tokens

A token is more than a string: it grants particular scopes, it expires, and it may come with a
refresh token. `Authorization` carries all four, so that what a token can do is known, and a
`Scope` can be checked against it. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
import internetAccess.online
import httpBackends.javaNetHttp
import logging.silentLogging
```

### Obtaining a token

An `Issuer` is an authorization server as one client sees it: its token endpoint, the client's
identity and secret, and — for the authorization-code flow — its authorization endpoint and the
client's redirect URI. Each grant posts to the token endpoint and reads the `Authorization` it
returns; a refusal raises `OAuth.Error`.

<!-- doccheck: skip -->
```scala
val issuer = Issuer(url"https://issuer.example/token", t"client-1", t"s3cret")

val token: Authorization = issuer.clientCredentials(t"read", t"write")
token.key                                   // the access token
token.grants(List(t"read"))                 // true
val fresh = issuer.refresh(token)           // through its refresh token
```

For the authorization-code flow, `authorizationUrl` builds the URL a user is sent to, with the
scopes, a `state` to check on return and, with a `Pkce`, the S256 challenge; `exchangeCode`
then exchanges the code the user brings back:

<!-- doccheck: skip -->
```scala
val flow = Issuer(url"https://issuer.example/token", t"client-1", Unset,
                  url"https://issuer.example/authorize", url"https://app.example/callback")

val pkce = Pkce()
flow.authorizationUrl(List(t"read"), state, pkce)   // send the user here
flow.exchangeCode(code, pkce)                       // when they come back
```

### Credentials for a typed client

A `Credential` names a security scheme, as an API's specification names it, and holds the value
the scheme calls for: a `Text` for an API key, an `Auth` for HTTP authentication, an
`Authorization` for OAuth 2. The [OpenAPI client](openapi.md) finds the credential for each
scheme an operation requires by its name, and presents it as the scheme dictates:

<!-- doccheck: skip -->
```scala
given key: ("api_key" is Credential to Text) = Credential(t"…")
given token: ("petstore_auth" is Credential to Authorization) = issuer.clientCredentials()
```

### In a web application

For an application which acts for its users, the `orthodoxy.server` module wraps a request
handler in the authorization-code flow: `issuer.oauth { … }` handles the redirect back from the
issuer, exchanging the code (or refreshing an expired token), and `issuer.require(scope) { … }`
sends a user who has not yet authorized the application to the issuer, and otherwise runs the
handler with an `Authorization of scope` in scope. An `Authorizations` holds each session's state.
