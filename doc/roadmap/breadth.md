# Standards Breadth

Soundness implements standards rather than inventing alternatives to them, and the shelf is
already long: HTTP/2, WebSocket, TLS, CBOR, Protocol Buffers, tar, zip, xz, PDF, TrueType,
X.509, COSE, iCalendar, OAuth 2, OpenAPI, LSP, MCP and dozens more, with RFC citations in the
code. But batteries-included is judged by what a production team reaches for and fails to
find. This track is a fixed target list, drained in order of how often that reach happens.

The list is deliberately closed: five standards, chosen because each completes a story the
platform already tells. JOSE/JWT is the largest hole — token-based authentication is ubiquitous,
and the neighbouring pieces (COSE, OAuth 2, X.509) already exist. TOML is table stakes for
interoperating with the wider tooling world. WebAuthn completes web authentication.
QUIC/HTTP-3 and IMAP round out transport and mail. Only the first three sit inside the
production-readiness gate; the rest lie beyond it. A row is complete
when its module ships with tests, error pages and a topic guide — the same bar as every other
module, because a battery that is undocumented or partial is not included, merely present.

| Item | Standard | Horizon | In the gate |
|------|----------|---------|-------------|
| brd-1 | JOSE: JWS, JWE, JWK, JWT (RFC 7515–7519) | near | yes |
| brd-2 | TOML | near | yes |
| brd-3 | WebAuthn | mid | yes |
| brd-4 | QUIC and HTTP/3 (RFC 9000, RFC 9114) | long | no |
| brd-5 | IMAP (RFC 9051) | long | no |

## brd-1: JOSE

Horizon: near

Signed and encrypted tokens: JWS, JWE, JWK and JWT, building on the primitives enigmatic
already provides, alongside its COSE support.

Done when: the module ships with tests, `SN-` error pages and a `doc/modules/` topic, and
round-trips the RFC 7515/7516 example vectors.

## brd-2: TOML

Horizon: near

The configuration format the wider tooling world speaks. Soundness configures itself with TEL;
it still reads what others write.

Done when: the module ships with tests, `SN-` error pages and a `doc/modules/` topic, and
passes the standard TOML compliance suite.

## brd-3: WebAuthn

Horizon: mid
Needs: brd-1

Passwordless authentication for the web stack, completing the story OAuth 2 and JOSE begin.
The COSE support it depends on already exists.

Done when: the module ships with tests, `SN-` error pages and a `doc/modules/` topic, and a
scripted registration-and-assertion ceremony passes against a reference authenticator.

## brd-4: QUIC and HTTP/3

Horizon: long

The transport successor. HTTP/2 and HPACK are already native; QUIC extends the same treatment
below them.

Done when: the module ships with tests, `SN-` error pages and a `doc/modules/` topic, and
interoperates with a reference HTTP/3 server and client.

## brd-5: IMAP

Horizon: long

Mail retrieval, completing what caduceus's SMTP support begins.

Done when: the module ships with tests, `SN-` error pages and a `doc/modules/` topic, and a
scripted session passes against a reference IMAP server.
