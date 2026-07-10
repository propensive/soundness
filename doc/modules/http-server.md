## HTTP Server

### About

An HTTP server is a handler: a function from a request to a response, served on a port. Soundness
runs one with a single expression — the handler reads the ambient request, matches on its method
and path, and returns a typed response whose `Content-Type` follows from the value it carries. The
raw-socket backend speaks modern HTTP/1.1 — keep-alive, pipelining, chunked bodies, TLS,
`100-continue` and protocol upgrades — on a virtual thread per connection.

Above HTTP/1.1 sit the neighbouring protocols: [WebSockets](https://en.wikipedia.org/wiki/WebSocket)
upgrade a request into a message stream handled as a state machine, and an HTTP/2 client speaks
h2c for the gRPC-style protocols that require it.

### On serving HTTP

Server frameworks tend to arrive as inversion-of-control: routing DSLs, controller classes,
annotation processors, and a runtime that calls the application rather than the reverse. The
protocol underneath is simpler than the frameworks suggest — a request arrives, a response is
computed — and a typed language can express that directly, with routing as ordinary pattern
matching and content negotiation as typeclasses.

A Soundness handler is a block of code; everything else is values. Everything comes from the
`soundness` package, with the concurrency context a server runs under:

```scala
import soundness.*
import strategies.throwUnsafely
import charEncoders.utf8Encoder
import threading.virtualThreading
import probates.awaitProbate
import webserverErrorPages.minimalErrorPage
```

### A server

`SocketServer` listens on a port and runs the handler for each request; the returned service stops
the server when cancelled:

```scala
supervise:
  val server = SocketServer(8080).handle:
    Http.Response(Http.Ok)(t"Hello, world!")
```

Within the handler, `request` is the current request — its method, target, headers, cookies and
body — so routing is a `match`:

```scala
SocketServer(8080).handle:
  request.target match
    case t"/"       => Http.Response(Http.Ok)(homePage)
    case t"/status" => Http.Response(Http.Ok)(t"OK")
    case _          => Http.Response(Http.NotFound)(t"No such page")
```

### Responses

`Http.Response` takes a status, headers as named arguments, and a body of any type with a
`Servable` instance — text, bytes, [HTML](html.md), [JSON](json.md) — which supplies the media
type, so what is served and how it is labelled cannot disagree:

```scala
Http.Response(Http.Ok, cacheControl = t"no-cache")(document)
Http.Response(Http.Found, location = t"/elsewhere")()
```

`Redirect`, `NotFound` and `NoCache` wrap content in the common response shapes. A request body
reads as a typed value with `as` — a [multipart](media-types.md) upload, a form
[submission](forms.md) — and `basicAuth` guards a response behind credentials.

### WebSockets

A request upgrades to a WebSocket by returning a `webSocket` handler, which treats the connection
as a state machine: each incoming message produces a reply, a state transition, or termination.
Messages are typed — raw frames, text, or any type carried over JSON:

```scala
case class Ping(value: Int)

SocketServer(8080).handle:
  webSocket(): (ping: Ping over Json) =>
    Reply(Ping(ping.value + 1).over[Json], ())
```

The implementation handles the protocol's sharp edges — masking, fragmentation, ping–pong,
UTF-8 validation, close codes — and conforms to the
[Autobahn](https://github.com/crossbario/autobahn-testsuite) test suite.

### HTTP/2

An HTTP/2 connection speaks cleartext h2c with prior knowledge — the flavour gRPC and other
multiplexed protocols use between services — with full
[HPACK](https://datatracker.ietf.org/doc/html/rfc7541) header compression. It slots in as an
[HTTP client](http-client.md) transport over an `Http2.Endpoint`, and carries the
[gRPC](rpc.md) support built on top of it.
