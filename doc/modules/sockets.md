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

### Socket options

The socket options of the underlying platform — `SO_REUSEADDR`, keep-alive, buffer sizes, timeouts —
are typed values collected from scope, each valid only for the transports that support it, so a
TCP-only option cannot be applied to a UDP socket:

```scala
import socketOptions.reuseAddressSocketOption
import socketOptions.keepAliveSocketOption
```

### Errors

Binding can fail — the port in use, permission denied — as a `BindError`, and an established
connection can fail during accept, transmission or close as a `ConnectionError`, each naming its
reason.
