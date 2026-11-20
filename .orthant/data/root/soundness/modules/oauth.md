## OAuth 2

### About

[OAuth 2](https://www.rfc-editor.org/rfc/rfc6749) is how a client is authorized to call an
API, on a user's behalf or on its own. The client obtains an access token from the API's
authorization server, and presents it with each request.

The client's side of that is provided here: the grants which obtain a token, PKCE for the
authorization-code flow, and the `Credential` by which a scheme's token or key reaches a typed
client. For a web application, the authorization-code flow is also provided as middleware for
an [HTTP server](http-server.md).

### On tokens

A token is more than a string. It grants particular scopes, it expires, and it may come with
a refresh token. An `Authorization` holds all four, so that what a token can do is known, and
a `Scope` can be checked against it.

Everything comes from the `soundness` package. Obtaining a token means making
[HTTP requests](http-client.md#what-a-request-needs-in-scope), so the imports which those
need are in scope:

```scala
import soundness.*
import strategies.throwUnsafely
import internetAccess.online
import httpBackends.javaNetHttp
import logging.silentLogging
```

The samples on this page contact an authorization server, so they are compiled but not run.

### Obtaining a token

An `Issuer` is an authorization server, as one client sees it. It has the server's token
endpoint, and the client's identity and secret. For the authorization-code flow, it also has
the server's authorization endpoint, and the client's redirect URI.

Each grant posts to the token endpoint, and reads the `Authorization` which comes back. A
refusal raises an `OAuth.Error`.

```tabular
column  Method
column  Grant
column  Gives
row  `clientCredentials`  client credentials (RFC 6749 §4.4)  the client's own authorization, for the scopes named
row  `refresh`  refresh (RFC 6749 §6)  a new authorization, from a refresh token or an `Authorization` carrying one
row  `authorizationUrl`  authorization code, first step (RFC 6749 §4.1.1)  the URL a user is sent to
row  `exchangeCode`  authorization code, second step (RFC 6749 §4.1.3)  the authorization, for the code the user brings back
```

A service which acts for itself uses the client-credentials grant:

```scala fuel
typecheck
##
val issuer = Issuer(url"https://issuer.example/token", t"client-1", t"s3cret")

val token: Authorization = issuer.clientCredentials(t"read", t"write")

val accessToken = token.key
val mayRead: Boolean = token.grants(List(t"read"))
val fresh: Authorization = issuer.refresh(token)
```

`key` is the access token itself. `grants` says whether the token covers the scopes named, and
`refresh` obtains a new authorization through the token's refresh token.

An application which acts for a user uses the authorization-code flow, which has two steps.
`authorizationUrl` builds the URL which the user is sent to. It has the scopes, a `state` to
check on return and, with a `Pkce`, the S256 challenge. `exchangeCode` then exchanges the code
which the user brings back:

```scala fuel
typecheck
##
val flow = Issuer(url"https://issuer.example/token", t"client-1", Unset,
                  url"https://issuer.example/authorize", url"https://app.example/callback")

val pkce = Pkce()
val state = t"a7f3c9"
val destination = flow.authorizationUrl(List(t"read"), state, pkce)

def callback(code: Text): Authorization =
  flow.exchangeCode(code, pkce)
```

The secret is `Unset` here, since a client which uses PKCE need not hold one.

### Credentials for a typed client

A `Credential` names a security scheme, as an API's specification names it, and holds the
value which the scheme calls for:

```tabular
column  Scheme
column  Value
row  an API key  `Text`
row  HTTP authentication  `Auth`
row  OAuth 2  `Authorization`
```

The [OpenAPI client](openapi.md) finds the credential for each scheme which an operation
requires, by its name, and presents it as the scheme dictates:

```scala fuel
typecheck
##
given key: ("api_key" is Credential to Text) = Credential(t"…")
given token: ("petstore_auth" is Credential to Authorization) =
  Credential(issuer.clientCredentials())
```

The scheme's name is part of the credential's type, so a credential cannot be presented for a
scheme other than its own.

### In a web application

For an application which acts for its users, the `orthodoxy.server` module wraps a request
handler in the authorization-code flow:

```tabular
column  Wrapper
column  Does
row  `issuer.oauth { … }`  handles the redirect back from the issuer, exchanging the code, or refreshing an expired token
row  `issuer.require(scope) { … }`  sends a user who has not yet authorized the application to the issuer, and otherwise runs the handler with an `Authorization of scope` in scope
```

An `Authorizations` holds the state of each session.

### Related

- [HTTP client](http-client.md) makes the requests which a token authorizes.
- [OpenAPI](openapi.md) generates a typed client which presents credentials for itself.
- [Cryptography](cryptography.md) covers the keys and signatures beneath other schemes of
  authentication.
