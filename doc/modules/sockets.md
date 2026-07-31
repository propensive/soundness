## Sockets

### About

Network communication below HTTP — raw
[TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol),
[UDP](https://en.wikipedia.org/wiki/User_Datagram_Protocol) and
[Unix domain sockets](https://en.wikipedia.org/wiki/Unix_domain_socket) — is handled through one
set of operations, generic over the transport. A server *listens* on a socket with a handler per
connection; a client *transmits* a message and reads the reply, holds a stateful *exchange*, or
opens a *duplex* connection for full bidirectional traffic. Which transport carries the bytes is
decided by the type of the socket value, not by a different API.

### On sockets

Socket programming has a shape that transcends the transport — bind and accept on one side, connect
and converse on the other — but each transport's API differs in incidentals: TCP has connections
where UDP has packets, domain sockets have paths where ports have numbers. Code written against one
rarely moves to another, and all of them expose blocking streams whose lifecycle the caller must
manage precisely.

Soundness abstracts the shape into typeclasses: a value that is *bindable* can serve, one that is
*connectable* can hold a conversation, and the transports differ only in which instances exist —
so a protocol written over one socket type runs over another where that makes sense. Everything
comes from the `soundness` package, with concurrency in scope for the connection handlers:

```scala
import soundness.*
import strategies.throwUnsafely
import threading.platformThreading
import probates.awaitProbate
```

### Serving

`listen` binds a socket and runs a handler for each arrival — a connection for TCP and domain
sockets, a packet for UDP — each on its own daemon. The returned service stops the listener:

```scala
supervise:
  val port = Port[Tcp]()   // an unused ephemeral port

  val server = port.listen[Data]: connection =>
    response(connection.stream())

  server.stop()
```

A UDP handler receives a `Packet` — the datagram with its sender — and answers with a reply or
silence:

```scala
udpPort.listen[Data]: packet =>
  UdpResponse.Reply(acknowledge(packet.data))
```

### Clients

A request–response client sends a message and reads the reply stream with `transmit`; a message is
anything *transmissible* — bytes, text, or any value that encodes to text:

```scala
val reply = DomainSocket(t"/run/service.sock").transmit(t"request")
```

`exchange` holds a conversation: each received message produces a `Control` — continue with new
state, reply, or conclude — so a client-side protocol is a state machine rather than interleaved
reads and writes. `duplex` opens a persistent bidirectional connection, sending and receiving
independently, which is the transport beneath [HTTP/2](http-server.md):

```scala
socket.duplex: duplex =>
  duplex.send(Stream(request))
  duplex.stream.head
```

### Sessions

A *session* is the same idea stated as a scope: `session` opens a connection to the target, lends
it to the block, and closes it when the block ends. The result type is quantified outside the
block, so a value still borrowing the live connection cannot escape it, while a memoized value
can:

```scala
endpoint.session: connection ?=>
  converse(connection)
```

This is what an [HTTP session](http-client.md) is built on, and how concurrent requests multiplex
on one HTTP/2 connection without a parked daemon: the loan and the scope coincide.

### TLS

A TLS connection is made through a secure endpoint, with the trust policy a contextual value. The
handshake may offer [ALPN](https://en.wikipedia.org/wiki/Application-Layer_Protocol_Negotiation)
protocols, in preference order, and the protocol the peer selected is reported back on the
connection — the seam an HTTP client uses to choose between an HTTP/2 and an HTTP/1.1 driver over
one socket. Offering nothing preserves the plain-TLS handshake that `wss` peers expect.

### Choosing a backend

Nothing above names a platform API. The primitive operations each role needs — bind and accept,
connect and converse, receive and reply, dispatch a datagram — are gathered into a
`SocketBackend`, and the loops that compose them stay platform-neutral. The
`java.nio.channels` implementation is `socketBackends.virtualMachine`, and backends over
`wasi:sockets` and over Scala Native's sockets supply the same operations, so the same protocol
code runs on the JVM, inside a WebAssembly component, and in a native binary. An operation a
backend cannot support — Unix-domain sockets or TLS on WASI — raises the appropriate error rather
than approximating it. Narrowing the platform's surface to a seam this small is
[decoupling](../philosophy/decoupling.md) applied within a module.

### Socket options

The socket options of the underlying platform — `SO_REUSEADDR`, keep-alive, buffer sizes, timeouts —
are typed values collected from scope, each valid only for the transports that support it, so a
TCP-only option cannot be applied to a UDP socket:

```scala
import socketOptions.reuseAddressSocketOption
import socketOptions.keepAliveSocketOption
import socketOptions.broadcastSocketOption
```

Options are collected from scope as a set rather than passed at each call, so a program's socket
policy is stated once. `SO_REUSEADDR` for a server that must restart without waiting out
`TIME_WAIT`, `SO_BROADCAST` for a UDP socket that sends to a broadcast address, `TCP_NODELAY` for
a protocol that must not wait for Nagle's algorithm, buffer sizes and timeouts — each is typed by
the transports that accept it, and a backend silently skips an option its platform does not
support rather than failing.

### Messages and their conversions

What a socket sends and receives is not bytes but values. `Transmissible` says how a value becomes
bytes on the way out, and `Ingressive` how bytes become a value on the way in, so a protocol is
written in its own vocabulary:

```scala
Ingressive.bytes         // received as raw Data
Ingressive.text          // decoded through the character encoding in scope
Ingressive.decoder[Port] // decoded to any type with a Decodable instance
```

A `Port` received over the wire is therefore a `Port`, validated on arrival, rather than text to
be checked later.

### Conversations as state machines

`exchange` drives a client-side protocol as a state machine: each received message yields a
`Control` saying what happens next. `Continue` carries the new state — or none, leaving it
unchanged — and `Terminate` ends the conversation:

```scala
socket.exchange(initial):
  case (state, message) =>
    if done(message) then Terminate else Continue(next(state, message))
```

Writing a protocol this way keeps its states explicit, rather than implied by where control
happens to be in a sequence of interleaved reads and writes.

### Errors

Binding can fail — the port in use, permission denied — as a `BindError`, and an established
connection can fail during accept, transmission or close as a `ConnectionError`, each naming its
reason.
