## HTTP Server

### About

An HTTP server is a handler: a function from a request to a response, served on a port. One
is run with a single expression. The handler reads the request which is in scope, matches on
its method and path, and returns a typed response, whose `Content-Type` follows from the
value it carries.

The server speaks modern HTTP/1.1, with keep-alive, pipelining, chunked bodies, TLS,
`100-continue` and protocol upgrades, on a virtual thread for each connection. It speaks
HTTP/2 on any connection whose TLS handshake negotiates it.

Two neighboring protocols sit above HTTP/1.1.
[WebSockets](https://en.wikipedia.org/wiki/WebSocket) upgrade a request into a stream of
messages, which is handled as a state machine. HTTP/2 carries many requests over one
connection, for browsers and for the protocols in the style of gRPC which require it.

### On serving HTTP

Server frameworks tend to arrive as inversion of control: DSLs for routing, controller
classes, annotation processors, and a runtime which calls the application, instead of the
reverse. The protocol underneath is simpler than the frameworks suggest. A request arrives,
and a response is computed. A typed language can express that directly, with routing as
ordinary pattern matching, and content negotiation as [typeclasses][typeclass].

Here a handler is a block of code, and everything else is values. A handler which states what
it needs, and what it may fail with, has an
[honest signature](../philosophy/honest-signatures.md), and that is why a server can be tested
without a socket.

Everything comes from the `soundness` package. The [concurrency](concurrency.md) context
which a server runs under is in scope:

```scala
import soundness.*

import charsets.utf8Charset
import codepages.utf8Codepage
import errorDiagnostics.stackTracesDiagnostics
import formatting.compactJsonFormatting
import logging.silentLogging
import probates.awaitProbate
import strategies.throwUnsafely
import textSanitizers.skipSanitizer
import threads.virtualThreads
import webserverErrorPages.minimalErrorPage
```

```tabular
column  Import
column  Decides
row  `threads.virtualThreads`, `probates.awaitProbate`  how the server's tasks run, and what happens to them when their scope ends
row  `webserverErrorPages.minimalErrorPage`  what a client is sent when a handler fails
row  `logging.silentLogging`  that requests are not logged; [Logging](logging.md) has the alternatives
row  `formatting.compactJsonFormatting`  how a JSON body is written
row  `charsets`, `codepages`, `textSanitizers`  how bytes become text, and text becomes bytes
row  `strategies.throwUnsafely`  that an error is thrown as an exception
```

The samples on this page bind a port, so they are compiled but not run.

### A server

`SocketServer` listens on a port, and runs the handler for each request. The port is bound
when `handle` is called, and the service which `handle` returns stops the server when it is
canceled. The service runs on tasks of the enclosing `supervise`, so the block lasts as long
as the server does.

Within the handler, `request` is the current request, with its method, target, headers,
cookies and body. Routing is therefore a `match`:

```scala fuel
frame
  label One response to every request
  code
      def hello(): Unit = supervise:
        val server = SocketServer(8080).handle:
          Http.Response(Http.Ok)(t"Hello, world!")
frame
  label Routed on the request's target
  code
      val homePage = t"<h1>Welcome</h1>"

      def routed(): Unit = supervise:
        SocketServer(8080).handle:
          request.target match
            case t"/"       => Http.Response(Http.Ok)(homePage)
            case t"/status" => Http.Response(Http.Ok)(t"OK")
            case _          => Http.Response(Http.NotFound)(t"No such page")
##
```

### Responses

`Http.Response` takes a status, headers as named arguments, and a body. The body may be of any
type with a `Servable` instance, such as text, bytes, [HTML](html.md) or [JSON](json.md). The
instance supplies the media type, so what is served, and how it is labeled, cannot disagree:

```scala
val status = Http.Response(Http.Ok, cacheControl = t"no-cache")(t"""{"status": "ok"}""".read[Json])
val moved = Http.Response(Http.Found, location = t"/elsewhere")()
```

The headers are checked as they are for a [request](http-client.md#headers): a header's value
must have the type which that header takes.

This is the `Content-Type` which each kind of body is served with:

```tabular
column  Body
column  `Content-Type`
row  `Text`  `text/plain`
row  `Data`  `application/octet-stream`
row  `Document[Html]`  `text/html; charset=UTF-8`
row  `Json`  `application/json; charset=UTF-8`
row  `Xml`  `application/xml; charset=UTF-8`
row  `Yaml`  `application/yaml; charset=UTF-8`
row  `Tel`  `application/vnd.tel`
row  `Cbor`  `application/cbor`
row  `Protobuf`  `application/protobuf`
row  `Chain[Sse]`  `text/event-stream`
row  any other type with a `Media` instance  the media type it declares
```

The instances for the format types are in the package `servables`.
[Media types](media-types.md#types-that-know-their-media-type) describes how a type of one's
own declares a media type.

`Redirect`, `NotFound` and `NoCache` wrap content in the common shapes of response. The body
of a request is read as a typed value with `as`, whether it is a
[multipart](media-types.md#multipart) upload or a [form submission](forms.md#submissions).
`basicAuth` guards a response behind credentials, and [OAuth](oauth.md#in-a-web-application)
guards one behind a token.

### Request and response bodies

The body of a request is a [stream](streams.md), not a block of bytes which must arrive before
the handler runs. A large upload is consumed as it is received, and the body of a response is
framed from its stream a block at a time, so neither is held in memory.

The body belongs to the handler's scope. It is the connection's, and the connection ends. A
handler which wants to keep a body past its response therefore reads it into memory
explicitly, with `memoize`.

### WebSockets

A request is upgraded to a WebSocket by returning a `webSocket` handler, which treats the
connection as a state machine. Each incoming message produces a reply, a change of state, or
the end of the conversation. Messages are typed: raw frames, text, or any type carried over
JSON.

```scala
case class Ping(value: Int)

def pinger(): Unit = supervise:
  SocketServer(8080).handle:
    webSocket(): (ping: Ping over Json) =>
      Control.Reply(Ping(ping.value + 1).over[Json], ())
```

A handler answers each message with a `Control`:

```tabular
column  Answer
column  Sends a message
column  Then
row  `Control.Reply(message, state)`  yes  carries on
row  `Control.Continue(state)`  no  carries on
row  `Control.Conclude(message, state)`  yes  closes
row  `Control.Terminate`  no  closes at once
```

The handler's state, which is `()` here, is passed from one message to the next. Where a
state is optional, and is left out, the previous one carries over.

The implementation handles the protocol's sharp edges: masking, fragmentation, ping and pong,
validation of UTF-8, and close codes. It conforms to the
[Autobahn](https://github.com/crossbario/autobahn-testsuite) test suite.

### HTTP/2

A listening socket with TLS offers `h2`, and then `http/1.1`, by ALPN. The protocol which a
connection speaks follows from how it arrives:

```tabular
column  Connection
column  Protocol
row  cleartext, to a `SocketServer`  HTTP/1.1
row  TLS, negotiating `h2`  HTTP/2
row  TLS, negotiating `http/1.1` or nothing  HTTP/1.1
row  cleartext h2c with prior knowledge, over an `Http2.Endpoint`  HTTP/2
```

The handler's contract is unchanged, so an existing application serves HTTP/2 with no
alteration:

```scala
def secure(context: javax.net.ssl.SSLContext): Unit = supervise:
  SocketServer(8443, ssl = context).handle:
    Http.Response(Http.Ok)(t"Hello over h2!")
```

Each stream which a client initiates is handled on a virtual thread of its own, so the
requests which share one connection run concurrently. The engine implements flow control,
trailers, and full [HPACK](https://datatracker.ietf.org/doc/html/rfc7541) compression of
headers. It tolerates the types of frame which it does not act on, such as `PRIORITY`,
`PUSH_PROMISE` and extensions, and does not fail the connection.
[Streams](streams.md#tuning-backpressure) describes how its flow control applies
backpressure.

Cleartext h2c with prior knowledge is the flavor used between services. It is spoken over an
`Http2.Endpoint`, which is also a transport for the [HTTP client](http-client.md), and which
carries the [gRPC](rpc.md) support built on top of it.

### Front-ends

Cleartext HTTP/1.1 connections are served by one of two front-ends. The choice is made by a
`Frontend` given, in scope when `handle` is called:

```tabular
column  Front-end
column  Runs a handler
column  A blocking handler stalls
row  `frontends.threadPerConnectionFrontend`, the default  on its connection's virtual thread  only its own connection
row  `frontends.reactiveFrontend`  inline on one of a fixed fleet of selector threads  every connection on that loop
```

On the default front-end, a handler may block: on a [database](database.md), on a file, or on
another service. The reactive front-end is an event loop, in which each selector thread
serves a batch of ready connections each time it wakes:

```scala
import frontends.reactiveFrontend

def reactive(): Unit = supervise:
  SocketServer(8080).handle:
    Http.Response(Http.Ok)(t"Hello, world!")
```

The reactive front-end serves small requests at about one and a half times the rate of the
default, and allocates less for each request, because nothing is scheduled for each request.
On a loopback benchmark it is level with Netty and Undertow.

The price is its contract. A handler which blocks stalls every connection on that loop. The
reactive front-end therefore suits handlers which compute their response from what they
already have, or which hand off to other tasks and answer from memory. Requests which the loop
cannot serve inline are handed to a virtual thread, and served by the default path: chunked
or oversized bodies, `Expect: 100-continue`, and protocol upgrades. A WebSocket handler
therefore works under either front-end.

### Related

- [HTTP client](http-client.md) makes requests with the same vocabulary of methods, statuses
  and headers.
- [HTML](html.md) and [JSON](json.md#json-over-http) describe the bodies most often served.
- [Forms](forms.md) renders an HTML form and reads its submission.
- [Sockets](sockets.md) is the layer beneath.
- [Daemons](daemons.md) keeps a server running as a service.
