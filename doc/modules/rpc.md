## RPC

### About

A remote procedure call pretends a method call crosses the network, and Soundness makes the
pretence typed at both ends. An RPC interface is an ordinary trait whose methods are marked
`@rpc`; from that one definition, a *server* dispatches incoming calls to an implementation and a
*client* stub turns method calls into wire messages — for
[JSON-RPC 2.0](https://www.jsonrpc.org/specification) over HTTP, and for
[gRPC](https://grpc.io/) over HTTP/2 with [Protocol Buffers](protobuf.md).

Beneath the protocols sits the shared plumbing of message streams:
[server-sent events](https://en.wikipedia.org/wiki/Server-sent_events), and *framing* — carving a
byte or text stream into its messages by length prefixes, `Content-Length` headers, or line
endings.

### On remote calls

The wire format of an RPC protocol is mechanical: method names, parameter encodings, id
correlation, error envelopes. Writing it by hand for each interface invites the usual drift —
client and server disagreeing about a parameter name — and code generators reintroduce the build
machinery that in-language derivation avoids.

Soundness derives both sides from the trait. The trait is the single source of truth; the macro
reads its `@rpc` methods, their parameter names and types, and produces dispatch and stub code
whose encodings agree by construction. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### An interface

An RPC interface is a trait of annotated methods, with case classes for the payloads:

```scala
case class Ping(message: Text)
case class Pong(message: Text)

trait Echo:
  @rpc def call(request: Ping): Pong
```

A method returning `Unit` is a *notification* — fire-and-forget, with no response expected — as
the JSON-RPC specification distinguishes.

### JSON-RPC

`JsonRpc.serve` turns an implementation into a dispatcher — a function from a request document to
an optional response — ready to sit behind any transport, typically an [HTTP](http-server.md)
handler. On the calling side, `remote` manufactures a client from the same trait:

```scala
val dispatch = JsonRpc.serve[Echo](implementation)

val echo = remote[Echo](url"https://api.example.com/rpc")
echo.call(Ping(t"hello"))   // a JSON-RPC request over HTTP
```

### gRPC

The same trait shape serves gRPC. A `GrpcChannel` opens over an [HTTP/2](http-server.md)
connection, and `Grpc.remote` derives the stub — a method returning a value makes a unary call,
one returning a `Stream` a server-streaming call — with payloads as Protocol Buffers messages:

```scala
supervise:
  val channel = GrpcChannel(endpoint)
  val echo = Grpc.remote[Echo](channel, t"echo.Echo")

  echo.call(Ping(t"ping"))   // a unary gRPC call
```

A non-OK response raises a `GrpcError` carrying the canonical status code — `NotFound`,
`Unauthenticated` and the rest — as a typed value.

### Server-sent events

An `Sse` value is one event of an SSE stream, with its event name, data lines, id and retry
interval; a stream of them serves as `text/event-stream`, and an `SseSource` buffers events with
replay from a client's last-seen id — the reconnection behaviour the protocol calls for:

```scala
val source = SseSource(capacity = 128)
source.put(update)
source.stream(start = lastEventId)
```

### Framing

Message boundaries in a raw stream are recovered with `frames`, naming the convention: a 4-byte
length prefix, a `Content-Length` header as LSP uses, or line endings:

```scala
stream.iterator.frames[LengthPrefix]     // length-prefixed messages
input.iterator.frames[ContentLength]     // header-framed messages, byte-counted
lines.iterator.frames[CrLf]              // CRLF-separated lines
lines.iterator.frames[Linefeed]          // or bare linefeeds, or CR alone
framed.iterator.frames[GrpcFraming]      // gRPC's flag byte and 4-byte length
```

Framing is independent of how the bytes arrive. A message split across three chunks, two messages
in one chunk, and a final message with no terminator are all handled, because the framer keeps its
own position rather than assuming chunk boundaries mean anything:

```scala
LazyList(t"one\ntwo\nth", t"ree").iterator.frames[Linefeed].to(List)
// List("one", "two", "three")
```

A truncated or malformed frame raises a `FrameError` naming what was wrong, so a protocol failure
is a diagnosis rather than a hang.

### Verifying a wire format

A protocol implementation that only round-trips against itself proves nothing: two matching bugs
look exactly like correctness. The HTTP/2 and gRPC codecs are therefore checked against *golden
bytes* — the exact octets each frame must serialize to, written out in the tests — as well as
round-tripped. A gRPC message's framing is one flag byte and a four-byte length, and the encoder
is checked to produce exactly that:

```scala
GrpcFraming.encode(payload)   // 00 00 00 00 05, then the payload
```

HPACK's header compression is verified against every example in
[RFC 7541](https://datatracker.ietf.org/doc/html/rfc7541)'s appendices, with and without Huffman
coding, and the `Huffman` codec against the strings the specification tabulates. Where a
specification publishes vectors, they are what the implementation is measured against.
