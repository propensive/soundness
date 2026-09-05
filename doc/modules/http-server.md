## HTTP Server

### About

An HTTP server is a handler: a function from a request to a response, served on a port. Soundness
runs one with a single expression — the handler reads the ambient request, matches on its method
and path, and returns a typed response whose `Content-Type` follows from the value it carries. The
raw-socket backend speaks modern HTTP/1.1 — keep-alive, pipelining, chunked bodies, TLS,
`100-continue` and protocol upgrades — on a virtual thread per connection, and speaks HTTP/2 on
any connection whose TLS handshake negotiates it.

Above HTTP/1.1 sit the neighboring protocols: [WebSockets](https://en.wikipedia.org/wiki/WebSocket)
upgrade a request into a message stream handled as a state machine, and HTTP/2 multiplexes many
requests over one connection, for browsers and for the gRPC-style protocols that require it.

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

import charEncoders.utf8Encoder
import errorDiagnostics.stackTracesDiagnostics
import formatting.compactJsonFormatting
import logging.silentLogging
import probates.awaitProbate
import strategies.throwUnsafely
import threading.virtualThreading
import webserverErrorPages.minimalErrorPage
```

A handler that states what it needs and what it may fail with is an [honest signature](../philosophy/honest-signatures.md), and the reason a server can be tested without a socket.

### A server

`SocketServer` listens on a port and runs the handler for each request; the returned service stops
the server when canceled:

```scala
def hello(): Unit = supervise:
  val server = SocketServer(8080).handle:
    Http.Response(Http.Ok)(t"Hello, world!")
```

The port is bound when `handle` is called, and the service runs on tasks of the enclosing
`supervise`, so the block lasts as long as the server does.

Within the handler, `request` is the current request — its method, target, headers, cookies and
body — so routing is a `match`:

```scala
val homePage = t"<h1>Welcome</h1>"

def routed(): Unit = supervise:
  SocketServer(8080).handle:
    request.target match
      case t"/"       => Http.Response(Http.Ok)(homePage)
      case t"/status" => Http.Response(Http.Ok)(t"OK")
      case _          => Http.Response(Http.NotFound)(t"No such page")
```

### Responses

`Http.Response` takes a status, headers as named arguments, and a body of any type with a
`Servable` instance — text, bytes, [HTML](html.md), [JSON](json.md) — which supplies the media
type, so what is served and how it is labeled cannot disagree:

```scala
Http.Response(Http.Ok, cacheControl = t"no-cache")(t"""{"status": "ok"}""".read[Json])
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

def pinger(): Unit = supervise:
  SocketServer(8080).handle:
    webSocket(): (ping: Ping over Json) =>
      Control.Reply(Ping(ping.value + 1).over[Json], ())
```

A handler answers with `Control.Reply` to send a message and carry on, `Control.Continue` to
carry on silently, `Control.Conclude` to send a final message and close, or
`Control.Terminate` to close at once. The handler's state, here `()`, is threaded from one
message to the next.

The implementation handles the protocol's sharp edges — masking, fragmentation, ping–pong,
UTF-8 validation, close codes — and conforms to the
[Autobahn](https://github.com/crossbario/autobahn-testsuite) test suite.

### Request and response bodies

A request body is a stream, not a block of bytes that must arrive before the handler runs: a
large upload is consumed as it is received, and a response body is framed off its stream
block by block, so neither side is held in memory. The body belongs to the handler's scope —
it is the connection's, and the connection ends — so a handler that wants to keep a body past
its response memoizes it explicitly.

### HTTP/2

A TLS listening socket offers `h2` then `http/1.1` by ALPN, and a connection that negotiates
`h2` is driven by the HTTP/2 engine instead of the HTTP/1.1 keep-alive loop. The handler
contract is unchanged, so an existing application serves HTTP/2 with no alteration:

```scala
def secure(context: javax.net.ssl.SSLContext): Unit = supervise:
  SocketServer(8443, ssl = context).handle:
    Http.Response(Http.Ok)(t"Hello over h2!")
```

Each client-initiated stream is handled on its own virtual thread, so requests multiplexed on
one connection run concurrently. The engine implements flow control, trailers, and full
[HPACK](https://datatracker.ietf.org/doc/html/rfc7541) header compression, and tolerates the
frame types it does not act on — `PRIORITY`, `PUSH_PROMISE`, extensions — rather than failing
the connection.

Cleartext h2c with prior knowledge — the flavor used between services — is spoken over an
`Http2.Endpoint`, which is also an [HTTP client](http-client.md) transport, and carries the
[gRPC](rpc.md) support built on top of it.
