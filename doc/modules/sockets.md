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
import threads.platformThreads
import probates.awaitProbate
import logging.silentLogging
import charsets.utf8Charset
import codepages.utf8Codepage
import textSanitizers.skipSanitizer
import socketBackends.javaBaseSockets
```

The operations, each covered below:

```tabular
column  Operation
column  Side
column  Does
row  `listen`  server  runs a handler for each connection or packet, for the duration of a block
row  `subscribe`  server  joins a multicast group and runs a handler for each datagram
row  `transmit`  client  sends a message and returns the reply stream
row  `react`  client  holds a conversation as a state machine
row  `duplex`  client  opens a persistent bidirectional connection
row  `session`  client  lends a connection to a block and closes it when the block ends
```

### Serving

`listen` binds a socket and runs a handler for each arrival — a connection for TCP and domain
sockets, a packet for UDP — each on its own task. The listener runs for the duration of the block
it is given, and stops when the block ends:

```scala fuel
typecheck
##
def response(request: Data): Data = request

supervise:
  val port = Port[Tcp]()   // an unused ephemeral port

  port.listen[Data](connection => response(connection.source.memoize)):
    // the server runs for the duration of this block
    ()
```

The endpoint decides the shape of the handler, on both sides. A stateful endpoint hands the
handler something it can hold a conversation through; a stateless one hands it a payload and
takes an answer back. So the same `listen` reads correctly for a TCP connection and for a UDP
datagram without the handler having to state which it is dealing with.

A UDP handler receives a `Packet` — the datagram with its sender — and answers with a reply or
silence:

```scala fuel
typecheck
##
supervise:
  Port[Udp]().listen[Data](packet => UdpResponse.Reply(packet.data)):
    ()
```

So each transport is chosen by the socket value:

```tabular
column  Transport
column  Socket value
column  A handler receives
row  TCP  a `Port[Tcp]`, or an `Endpoint` of one to connect to  a connection
row  UDP  a `Port[Udp]`, or an `Endpoint` of one to send to  a `Packet`
row  Unix domain  a `DomainSocket` path  a connection
```

### Multicast

A multicast group is subscribed to rather than listened on: `subscribe` joins the group on every
interface that is up and multicast-capable, runs the handler for each datagram the group
delivers, and lends a `Multicast.Subscription` that can also send to the group unprompted — the
shape of a service announcing itself, as mDNS does. A handler answers to the group, to the one
sender, or not at all:

```scala fuel
parse
##
import socketOptions.multicastLoopSocketOption   // see our own announcements too

supervise:
  val group: Multicast = Multicast(ip"224.0.0.251", udp"mdns")

  group.subscribe(packet => Multicast.Reply.Group(packet.data)):
    summon[Multicast.Subscription].send(t"hello, group".in[Data])
```

The group's port is bound with address and port reuse, so several programs on one host — ours
and the system's own mDNS responder, say — subscribe side by side. A group is one address, so
a protocol spoken over both IPv4 and IPv6 multicast subscribes to two.

### Datagram exchanges

Some protocols over UDP are one datagram out and one back — a DNS query, an NTP request. The
backend's `exchangeUdp` does exactly that from an ephemeral port, bounded by a
`Socket.Option.Timeout` in scope; `coaxial.dns` builds a typed DNS client on it. A nameserver is
an `Endpoint` of a UDP port, and `query` sends a `Dns.Message` and returns the reply that
answers it, or, given a name and a record type (`A` by default), asks the common question
directly and returns the answering records:

```scala fuel
typecheck
##
import coaxial.query

val resolver = ip"9.9.9.9" on udp"domain"
val addresses = resolver.query(dns"example.com").map(_.rdata)
```

A reply whose ID or question differ from the query's — a stray datagram — is a
`Dns.Error(Mismatch)`, and a reply that never comes a `Dns.Error(Timeout)`.

### Clients

A request–response client sends a message and reads the reply stream with `transmit`; a message is
anything *transmissible* — bytes, text, or any value that encodes to text:

```scala fuel
typecheck
##
val reply: Stream[Data] over Credit = DomainSocket(t"/run/service.sock").transmit(t"request")
```

`react` holds a conversation: each received message produces a `Control` — continue with new
state, reply, or conclude — so a client-side protocol is a state machine rather than interleaved
reads and writes. `duplex` opens a persistent bidirectional connection, sending and receiving
independently, which is the transport beneath [HTTP/2](http-server.md):

```scala fuel
typecheck
##
DomainSocket(t"/run/service.sock").duplex: duplex =>
  duplex.send(Stream(t"request".in[Data]))
  duplex.source   // the read side, a Stream[Data]
```

### Sessions

A *session* is the same idea stated as a scope: `session` opens a connection to the target, lends
it to the block, and closes it when the block ends. The result type is quantified outside the
block, so a value still borrowing the live connection cannot escape it, while a memoized value
can:

```scala fuel
typecheck
##
import internetAccess.online

val endpoint = ip"127.0.0.1" on Port[Tcp](8080)

val greeting: Data = endpoint.session: connection ?=>
  connection.send(Stream(t"hello".in[Data]))
  connection.source.memoize
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
`Socket.Backend`, and the loops that compose them stay platform-neutral. Three backends supply
the same operations, so the same protocol code runs on the JVM, inside a WebAssembly component,
and in a native binary:

```tabular
column  Backend
column  Platform
column  Over
column  Unsupported
row  `socketBackends.javaBaseSockets`  JVM  `java.nio.channels`  nothing
row  `socketBackends.wasiSockets`  WebAssembly  `wasi:sockets`  Unix-domain sockets, UDP, multicast, TLS
row  `socketBackends.scalaNativeSockets`  Scala Native  Scala Native's sockets  Unix-domain sockets
```

An operation a backend cannot support raises the appropriate error rather than approximating it.
Narrowing the platform's surface to a seam this small is
[decoupling](../philosophy/decoupling.md) applied within a module.

### Socket options

The socket options of the underlying platform — `SO_REUSEADDR`, keep-alive, buffer sizes, timeouts —
are typed values collected from scope, each valid only for the transports that support it, so a
TCP-only option cannot be applied to a UDP socket. A flag is a given imported by name:

```scala
import socketOptions.reuseAddressSocketOption
import socketOptions.keepAliveSocketOption
import socketOptions.broadcastSocketOption
```

An option carrying a value is made by a method of the same package, and given at its own type,
as in `given Socket.Option.ReceiveBuffer = socketOptions.receiveBuffer(65536)`:

```tabular
column  Option
column  In scope with
column  Transports
column  For
row  `SO_REUSEADDR`  `reuseAddressSocketOption`  TCP, UDP, domain  a server that must restart without waiting out `TIME_WAIT`
row  `SO_REUSEPORT`  `reusePortSocketOption`  TCP, UDP  several sockets on one port (OS-dependent)
row  `TCP_NODELAY`  `noDelaySocketOption`  TCP  a protocol that must not wait for Nagle's algorithm
row  `SO_KEEPALIVE`  `keepAliveSocketOption`  TCP  keep-alive probes on an idle connection
row  `SO_BROADCAST`  `broadcastSocketOption`  UDP  sending to a broadcast address
row  `IP_MULTICAST_LOOP`  `multicastLoopSocketOption`  UDP  receiving our own multicast datagrams
row  `SO_RCVBUF`  `receiveBuffer(bytes)`  TCP, UDP, domain  the receive buffer's size
row  `SO_SNDBUF`  `sendBuffer(bytes)`  TCP, UDP, domain  the send buffer's size
row  `SO_TIMEOUT`  `timeout(milliseconds)`  TCP, UDP, domain  bounding a blocking wait
row  `SO_LINGER`  `linger(seconds)`  TCP, domain  how long a close waits for unsent data
row  `IP_TOS`  `trafficClass(value)`  TCP, UDP  the traffic class
row  `IP_MULTICAST_TTL`  `multicastHops(count)`  UDP  how many hops a multicast datagram travels
row
  cell
  cell  `datagramSize(bytes)`
  cell  UDP
  cell  the largest datagram received whole: 1472 bytes unless set
```

Options are collected from scope as a set rather than passed at each call, so a program's socket
policy is stated once, and a backend silently skips an option its platform does not support
rather than failing.

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

`react` drives a client-side protocol as a state machine: each received message yields a
`Control` saying what happens next. `Continue` carries the new state — or none, leaving it
unchanged — and `Terminate` ends the conversation:

```scala fuel
typecheck
##
val received: Int = endpoint.react(0)[Text]: state ?=> message =>
  if message == t"bye" then Control.Terminate else Control.Continue(state + 1)
```

Writing a protocol this way keeps its states explicit, rather than implied by where control
happens to be in a sequence of interleaved reads and writes.

### Errors

Binding can fail — the port in use, permission denied — as a `Bind.Error`, and an established
connection can fail during accept, transmission or close as a `Socket.Error`, each naming its
reason.
