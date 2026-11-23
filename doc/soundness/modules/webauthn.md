## WebAuthn: passwordless sign-in

### About

[Web Authentication](https://www.w3.org/TR/webauthn-2/) lets a user sign in to a site with a
key instead of a password. An *authenticator*, which may be a security key, a phone or the
secure hardware of a laptop, makes a key pair for the site, keeps the private key, and signs a
challenge with it each time the user signs in. A credential of this kind which is synchronized
between a user's devices is a *passkey*.

The server's side, which the standard calls the *relying party*, is provided here. It makes
the options a page passes to the browser, and checks what the browser sends back. The keys
and certificates are those of [cryptography](cryptography.md).

### On ceremonies

There is nothing for a server to remember but a public key, and nothing a user can be
tricked into typing. What takes care is the checking. An answer must be to the challenge
which was sent, and not an old one. It must come from the site's own origin, and not from a
page which resembles it. It must be made for this relying party, with the user present. The
signature must be the credential's.

Each of the two *ceremonies* is one call to make the options and one to check the answer,
and the second makes every check the standard requires.

Everything comes from the `soundness` package. A provider of algorithms is in scope, as it
is for any cryptography:

```scala
import soundness.*

import codepages.utf8Codepage
import errorDiagnostics.stackTracesDiagnostics
import providers.javaBaseProvider
import strategies.throwUnsafely
```

A relying party is identified by a domain, and names the origins its pages are served from:

```scala
val globe = WebAuthn.RelyingParty(t"globe.example", t"The Globe", List(t"https://globe.example"))
```

### Registration

A credential is registered to an account, which is a `WebAuthn.User`. Its `id` is a handle
of up to 64 bytes which identifies the account. An authenticator stores it, so it should say
nothing about the person:

```scala
val burbage = WebAuthn.User(t"account-1567".in[Data], t"burbage", t"Richard Burbage")
```

A ceremony begins with a fresh `WebAuthn.Challenge`, which the server keeps until the answer
arrives. `WebAuthn.registration` makes the options, as the JSON a page passes to
`PublicKeyCredential.parseCreationOptionsFromJSON` and then to `navigator.credentials.create`:

```scala
val challenge = WebAuthn.Challenge()
val options: Json = WebAuthn.registration(globe, burbage, challenge)
```

The page sends back the credential's `toJSON()`, and `WebAuthn.register` checks it. What it
returns is the `WebAuthn.Credential` to store for the account:

```scala fuel
typecheck
##
def enrol(answer: Json): WebAuthn.Credential =
  WebAuthn.register(answer, challenge, globe)
```

A credential has an `id`, by which it is found again, the public `key`, and a signature
`counter`. Two things are left to the caller, because only the caller has the data: checking
that the `id` is not already registered to another account, and storing the credential.

`registration` takes further parameters, each with a default:

```tabular
column  Parameter
column  Default
column  Meaning
row  `existing`  none  the identifiers of credentials the account already has, so that one authenticator is not registered twice
row  `algorithms`  `WebAuthn.Algorithms`  the COSE algorithms accepted, in order of preference: ES256, EdDSA and RS256
row  `verification`  `false`  whether the user must be verified, by a PIN or a fingerprint, and not merely present
row  `attestation`  `none`  the attestation conveyance asked of the authenticator
```

What is asked for in `verification` and `algorithms` must be given to `register` as well,
which is what enforces it.

### Authentication

`WebAuthn.authentication` makes the options for signing in, for
`navigator.credentials.get`. The identifiers of the account's credentials are passed where
the account is already known, and left out where the authenticator is to offer whichever
credentials it holds for the site:

```scala
val signInChallenge = WebAuthn.Challenge()
val signInOptions: Json = WebAuthn.authentication(globe, signInChallenge)
```

The answer names the credential it is from. `WebAuthn.identifier` reads that name, so the
stored credential can be found, and `WebAuthn.authenticate` checks the answer against it:

```scala fuel
typecheck
##
def signIn(answer: Json, find: Data => WebAuthn.Credential): WebAuthn.Credential =
  val stored = find(WebAuthn.identifier(answer))
  WebAuthn.authenticate(answer, signInChallenge, globe, stored)
```

The credential which is returned has its counter advanced, and is stored in place of the
old one. A counter which has *not* advanced means the private key is in two places, so the
answer is refused. An authenticator which does not count reports zero every time, which is
accepted for as long as the stored counter is zero as well.

Every refusal is a `WebAuthn.Error`, whose reason says which check failed. A challenge is for
one ceremony, and is discarded whether the answer is accepted or not.

### In a web application

A challenge has to be kept from the request which makes a ceremony's options to the request
which brings its answer. `Ceremonies` is a store for that, keyed by the user's `Session`.
`begin` makes a session's challenge and remembers it. `conclude` gives it back, once, and
forgets it:

```scala
val ceremonies = Ceremonies()
```

```scala fuel
typecheck
##
def start(session: Session): Json =
  WebAuthn.authentication(globe, ceremonies.begin(session))

def finish(session: Session, answer: Json, stored: WebAuthn.Credential): WebAuthn.Credential =
  val pending = ceremonies.conclude(session).or(abort(WebAuthn.Error(WebAuthn.Error.Reason.Challenge)))
  WebAuthn.authenticate(answer, pending, globe, stored)
```

Since `conclude` gives a challenge back only once, an answer which is offered a second time
finds no ceremony in progress, and is refused.

### Attestation

An authenticator may *attest* the credential it makes, which is a statement about the
authenticator itself. A credential's `attestation` says what was established:

```tabular
column  Attestation
column  Meaning
row  `Unattested`  the authenticator made no statement
row  `Self`  the statement was signed with the credential's own key, and proves nothing about the authenticator
row  `Certified`  the statement was signed with a key which a certificate chain certifies
```

Most relying parties accept any authenticator, and ask for no attestation. One which
restricts the models it accepts asks for `direct` conveyance, and gives `register` the roots
it trusts as `anchors`. A `Certified` attestation is then accepted only if its chain leads to
one of them, and its `trusted` member is `true`. With no anchors, the chain is reported and
not validated.

The `none` and `packed` formats are understood. Any other format raises a `WebAuthn.Error`.

### Related

- [Cryptography](cryptography.md) provides the keys and certificates.
- [JOSE](jose.md) provides tokens, for the session which follows a sign-in.
- [OAuth 2](oauth.md) delegates sign-in to another party.
- [HTTP server](http-server.md) serves the pages and receives the answers.
