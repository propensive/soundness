## RPC

### About

A remote procedure call pretends that a method call crosses the network. Here the pretense is
typed at both ends. An RPC interface is an ordinary trait whose methods are marked `@rpc`. From
that one definition, a *server* dispatches incoming calls to an implementation, and a *client*
stub turns method calls into wire messages. This works for
[JSON-RPC 2.0](https://www.jsonrpc.org/specification) over HTTP, and for
[gRPC](https://grpc.io/) over HTTP/2 with [Protocol Buffers](protobuf.md).

Beneath the protocols is the shared plumbing of message streams:
[server-sent events](https://en.wikipedia.org/wiki/Server-sent_events), and *framing*, which
carves a stream of bytes or text into its messages by length prefixes, `Content-Length` headers
or line endings.

### On remote calls

The wire format of an RPC protocol is mechanical: method names, parameter encodings, id
correlation, error envelopes. Writing it by hand for each interface invites the usual drift, with
the client and server disagreeing about the name of a parameter. A code generator avoids the
drift, but brings back the build machinery which [derivation] in the language avoids.

Here both sides are derived from the trait. The trait is the single source of truth. A macro
reads its `@rpc` methods, their parameter names and their types, and produces dispatch and stub
code whose encodings agree by construction. Client and server cannot disagree about a method
they share, which is [correctness](../philosophy/correctness.md) by construction.

Everything comes from the `soundness` package, with an encoding for the bytes which cross the
wire, and the last import lets the samples print their results:

```scala
import soundness.*
import strategies.throwUnsafely
import codepages.utf8Codepage
import charsets.utf8Charset
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### An interface

An RPC interface is a trait of annotated methods, with case classes for the payloads:

```scala
case class Ping(message: Text)
case class Pong(message: Text)

trait Echo:
  @rpc def call(request: Ping): Pong
```

A method which returns `Unit` is a *notification*. It is fire-and-forget, with no response
expected, as the JSON-RPC specification distinguishes.

### JSON-RPC

`JsonRpc.serve` turns an implementation into a dispatcher, ready to sit behind any transport,
typically an [HTTP](http-server.md) handler:

```scala
object EchoService extends Echo:
  def call(request: Ping): Pong = Pong(request.message)

val dispatch = JsonRpc.serve[Echo](EchoService)
```

The dispatcher is a function from a request `Json` document to an [optional] response. A
notification produces none. A route handler calls it with the body of the request:

```scala fuel
run
##
import formatting.compactJsonFormatting

val request = t"""{"jsonrpc": "2.0", "id": 1, "method": "call", "params": {"request": {"message": "hello"}}}""".read[Json]
Out.println(dispatch(request).let(_.show).inspect)
```

On the calling side, `remote` manufactures a client from the same trait. It sends each call as a
request to the URL:

```scala fuel
typecheck
##
val echo = remote[Echo](url"https://api.example.com/rpc")
val pong: Pong = echo.call(Ping(t"hello"))
```

### gRPC

The same shape of trait serves gRPC. A `Grpc.Channel` opens over an [HTTP/2](http-server.md)
connection, and `Grpc.remote` derives the stub. A method which returns a value makes a unary
call, and one which returns a `Stream` makes a server-streaming call. The payloads are Protocol
Buffers messages:

```scala fuel
typecheck
##
import threads.platformThreads
import internetAccess.online
import socketBackends.javaBaseSockets
import probates.awaitProbate

supervise:
  val endpoint = Endpoint(t"localhost", Port[Tcp](50051))
  val channel = Grpc.Channel(Http2.Endpoint(endpoint, t"localhost"))
  val echo = Grpc.remote[Echo](channel, t"echo.Echo")

  val pong: Pong = echo.call(Ping(t"ping"))
```

A response which is not OK raises a `Grpc.Error`, which carries the canonical status code, such
as `NotFound` or `Unauthenticated`, as a typed value.

### Server-sent events

An `Sse` value is one event of an SSE stream, with its event name, data lines, id and retry
interval. A stream of them serves as `text/event-stream`. An `Sse.Source` buffers events, and
replays them from the last id a client saw, which is the reconnection behavior the protocol
calls for. A client which last saw the event with id `0` receives the next:

```scala fuel
run
##
val source = Sse.Source(capacity = 128)
source.put(t"first update")
source.put(t"second update")

Out.println(source.stream(start = 0).head.let(_.data).inspect)
```

### Framing

The boundaries of messages in a raw stream are recovered with `frames`, which names the
convention:

```tabular
column  Convention
column  Frames
column  Each message is
row  `LengthPrefix`  `Data`  a four-byte big-endian length, then that many bytes
row  `ContentLength`  `Data`  a `Content-Length` header block, as LSP uses, then that many bytes
row  `CrLf`  `Text`  a line ending in CRLF
row  `Linefeed`  `Text` or `Data`  a line ending in a bare linefeed
row  `CarriageReturn`  `Text`  a line ending in CR alone
row  `Grpc.Framing`  `Data`  a flag byte and a four-byte length, then that many bytes
```

```scala
val prefixed = Chain(Data(0, 0, 0, 2, 104, 105))
val headed = Chain(t"Content-Length: 2\r\n\r\nhi".in[Data])
val lines = Chain(t"one\r\ntwo\r\n")
```

```scala fuel
run
##
prefixed.iterator.frames[LengthPrefix].each(frame => Out.println(frame.utf8))
headed.iterator.frames[ContentLength].each(frame => Out.println(frame.utf8))
lines.iterator.frames[CrLf].each(Out.println(_))
```

gRPC's own framing, `Grpc.Framing`, is used the same way.

A `Duplex` which carries BinTEL messages, such as the protocol of a daemon over TLS, or over a
UNIX domain socket to a local tool, is `framed` by naming what it reads and what it sends. Each
`send` writes one length-prefixed record whose body is the BinTEL encoding of the value.
`messages` reads the records of the peer back as values. The schemas are derived from the two
types once, when the connection is framed, and not for each message. One end's
`Framed from Reply to Request` meets the other's `Framed from Request to Reply`:

```scala fuel
run
##
case class Request(id: Int, text: Text)
case class Reply(id: Int, length: Int)

val (near, far) = Duplex.pair()
val client: Framed from Reply to Request = near.framed[Reply, Request]
val server: Framed from Request to Reply = far.framed[Request, Reply]

client.send(Request(1, t"hello"))

server.messages.next() match
  case request: Request   => server.send(Reply(request.id, request.text.length))
  case _                  => ()

Out.println(client.messages.next().inspect)
```

A truncated record raises `Framing.Error`, and a body which is not the expected message raises
`Bintel.Error`, each as the record is read.

Framing is independent of how the bytes arrive. A message split across three chunks, two
messages in one chunk, and a final message with no terminator are all handled, because the
framer keeps its own position, and assumes nothing about the boundaries of chunks:

```scala fuel
run
##
Chain(t"one\ntwo\nth", t"ree").iterator.frames[Linefeed].each(Out.println(_))
```

A truncated or malformed frame raises a `Framing.Error` which names what was wrong, so a
protocol failure is a diagnosis and not a hang.

### Verifying a wire format

A protocol implementation which only round-trips against itself proves nothing. Two matching
bugs look exactly like correctness. The HTTP/2 and gRPC codecs are therefore checked against
*golden bytes*, the exact octets which each frame must serialize to, written out in the tests,
as well as round-tripped. The framing of a gRPC message is one flag byte and a four-byte length,
and the encoder is checked to produce exactly that:

```scala fuel
run
##
import alphabets.hexLowerCase

Out.println(Grpc.Framing.encode(t"hello".in[Data]).serialize[Hex])
```

HPACK's header compression is verified against every example in the appendices of
[RFC 7541](https://datatracker.ietf.org/doc/html/rfc7541), with and without Huffman coding, and
the `Huffman` codec against the strings which the specification tabulates. Where a specification
publishes vectors, they are what the implementation is measured against.

### Related

- [HTTP server](http-server.md) hosts a JSON-RPC dispatcher, and the HTTP/2 which gRPC runs on.
- [HTTP client](http-client.md) carries the requests of a JSON-RPC stub.
- [JSON](json.md) and [Protobuf](protobuf.md) encode the payloads.
- [TEL](tel.md) describes BinTEL, which a framed `Duplex` carries.
- [Sockets](sockets.md) describes `Duplex` and `Endpoint`.
- [MCP](mcp.md), [ACP](acp.md) and [LSP](lsp.md) are protocols built on JSON-RPC.
- [Containers](docker.md) speaks gRPC to a containerd daemon.
