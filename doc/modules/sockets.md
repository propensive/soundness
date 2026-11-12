## Sockets

### About

Network communication below HTTP is handled through one set of operations, which is generic
over the transport: raw [TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol),
[UDP](https://en.wikipedia.org/wiki/User_Datagram_Protocol) and
[Unix domain sockets](https://en.wikipedia.org/wiki/Unix_domain_socket).

A server *listens* on a socket, with a handler for each connection. A client *transmits* a
message and reads the reply, holds a stateful *exchange*, or opens a *duplex* connection for
traffic in both directions. Which transport carries the bytes is decided by the type of the
socket value, not by a different API.

### On sockets

Programming with sockets has a shape which is the same for every transport: bind and accept
on one side, connect and converse on the other. But each transport's API differs in its
incidentals. TCP has connections where UDP has packets, and domain sockets have paths where
ports have numbers. Code written against one rarely moves to another, and all of them expose
blocking streams whose lifetime the caller must manage precisely.

Here the shape is abstracted into [typeclasses][typeclass]. A value which is *bindable* can
serve, and one which is *connectable* can hold a conversation. The transports differ only in
which instances exist, so a protocol written over one type of socket runs over another, where
that makes sense.

Everything comes from the `soundness` package. [Concurrency](concurrency.md) is in scope, for
the handlers of connections:

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

The samples on this page open connections, so they are compiled but not run.

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

`listen` binds a socket, and runs a handler for each arrival: a connection for TCP and domain
sockets, and a packet for UDP. Each handler runs on a task of its own. The listener runs for
the duration of the block which it is given, and stops when the block ends:

```scala fuel
typecheck
##
def response(request: Data): Data = request

supervise:
  val port = Port[Tcp]()

  port.listen[Data](connection => response(connection.source.memoize)):
    ()
```

`Port[Tcp]()` asks the operating system for a
[port which is not in use](network-addresses.md#ports).

The endpoint decides the shape of the handler, on both sides. A stateful endpoint hands the
handler something which it can hold a conversation through. A stateless one hands it a
payload, and takes an answer back. So the same `listen` reads correctly for a TCP connection
and for a UDP datagram, without the handler having to state which it is dealing with.

A UDP handler receives a `Packet`, which is the datagram with its sender. It answers with a
reply, or with silence:

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
row  Unix domain  a `Uds` path  a connection
```

### Clients

A client which makes a request, and expects a response, sends a message and reads the stream
of the reply with `transmit`. A message is anything *transmissible*: bytes, text, or any value
which encodes to text.

```scala fuel
typecheck
##
val reply: Stream[Data] over Credit = Uds(t"/run/service.sock").transmit(t"request")
```

`duplex` opens a persistent connection in both directions, sending and receiving
independently. It is the transport beneath [HTTP/2](http-server.md#http2):

```scala fuel
typecheck
##
Uds(t"/run/service.sock").duplex: duplex =>
  duplex.send(Stream(t"request".in[Data]))
  duplex.source
```

`duplex.source` is the read side, a [`Stream[Data]`](streams.md).

### Sessions

A [session] is the same idea, stated as a scope. `session` opens a connection to the target,
lends it to the block, and closes it when the block ends. A value which still borrows the live
connection cannot escape the block, while a value which has been read into memory can:

```scala fuel
typecheck
##
import internetAccess.online

val endpoint = ip"127.0.0.1" on Port[Tcp](8080)

val greeting: Data = endpoint.session: connection ?=>
  connection.send(Stream(t"hello".in[Data]))
  connection.source.memoize
```

An [HTTP session](http-client.md#sessions) is built on this. It is how concurrent requests
share one HTTP/2 connection, without a thread parked to manage it: the loan and the scope
coincide.

### Conversations as state machines

`react` holds a conversation on the client's side, as a state machine. Each message which is
received gives a `Control`, which says what happens next. `Continue` carries the new state, or
none, to leave it unchanged. `Terminate` ends the conversation:

```scala fuel
typecheck
##
val received: Int = endpoint.react(0)[Text]: state ?=> message =>
  if message == t"bye" then Control.Terminate else Control.Continue(state + 1)
```

Writing a protocol in this way keeps its states explicit. They are not implied by where
control happens to be, in a sequence of interleaved reads and writes.

### Messages and their conversions

What a socket sends and receives is not bytes, but values. `Transmissible` says how a value
becomes bytes on the way out. `Ingressive` says how bytes become a value on the way in. A
protocol is therefore written in its own vocabulary:

```tabular
column  Instance
column  A message is received as
row  `Ingressive.bytes`  raw `Data`
row  `Ingressive.text`  text, decoded through the character encoding in scope
row  `Ingressive.decoder[Port]`  any type with a [`Decodable`](decoding.md) instance, here a `Port`
```

A `Port` which is received over the wire is therefore a `Port`, validated on arrival, and not
text to be checked later.

### Multicast

A multicast group is subscribed to, not listened on. `subscribe` joins the group on every
interface which is up and capable of multicast, and runs the handler for each datagram which
the group delivers. It lends a `Multicast.Subscription`, which can also send to the group
unprompted. That is the shape of a service announcing itself, as mDNS does. A handler answers
to the group, to the one sender, or not at all:

```scala fuel
parse
##
import socketOptions.multicastLoopSocketOption

supervise:
  val group: Multicast = Multicast(ip"224.0.0.251", udp"mdns")

  group.subscribe(packet => Multicast.Reply.Group(packet.data)):
    summon[Multicast.Subscription].send(t"hello, group".in[Data])
```

The option `multicastLoopSocketOption` lets the program see its own announcements too.

The group's port is bound with reuse of the address and the port, so several programs on one
host can subscribe side by side, such as this one and the system's own mDNS responder. A
group is one address, so a protocol which is spoken over both IPv4 and IPv6 multicast
subscribes to two. [Service discovery](service-discovery.md) is built on this.

### Datagram exchanges

Some protocols over UDP send one datagram out, and get one back: a DNS query, or an NTP
request. The backend's `exchangeUdp` does exactly that, from an ephemeral port, bounded by a
`Socket.Option.Timeout` in scope. `coaxial.dns` builds a typed DNS client on it.

A nameserver is an `Endpoint` of a UDP port. `query` sends a
[`Dns.Message`](network-addresses.md#dns-names-and-messages), and returns the reply which
answers it. Given a name and a type of record, which is `A` by default, it asks the common
question directly, and returns the records which answer it:

```scala fuel
typecheck
##
import coaxial.query

val resolver = ip"9.9.9.9" on udp"domain"
val addresses = resolver.query(dns"example.com").map(_.rdata)
```

A reply whose ID or question differs from the query's is a stray datagram, and is a
`Dns.Error(Mismatch)`. A reply which never comes is a `Dns.Error(Timeout)`.

### TLS

A TLS connection is made through a secure endpoint, and the trust policy is a
[contextual value]. The handshake may offer
[ALPN](https://en.wikipedia.org/wiki/Application-Layer_Protocol_Negotiation) protocols, in
order of preference, and the protocol which the peer selected is reported back on the
connection. That is what an HTTP client uses to choose between an HTTP/2 driver and an
HTTP/1.1 driver over one socket. Offering nothing preserves the plain TLS handshake which
`wss` peers expect. [Cryptography](cryptography.md#certificates) generates the certificates
which a server presents.

### Socket options

The socket options of the underlying platform, such as `SO_REUSEADDR`, keep-alive, the sizes
of buffers and timeouts, are typed values, collected from scope. Each is valid only for the
transports which support it, so an option for TCP alone cannot be applied to a UDP socket. A
flag is a given, imported by name:

```scala
import socketOptions.reuseAddressSocketOption
import socketOptions.keepAliveSocketOption
import socketOptions.broadcastSocketOption
```

An option which carries a value is made by a method of the same package, and given at its own
type, as in `given Socket.Option.ReceiveBuffer = socketOptions.receiveBuffer(65536)`:

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

Options are collected from scope as a set, and are not passed at each call, so a program's
policy for its sockets is stated once. A backend silently skips an option which its platform
does not support, and does not fail.

### Errors

Binding can fail, because the port is in use, or permission is denied. That is a
`Bind.Error`. An established connection can fail while it is accepted, during transmission,
or as it is closed. That is a `Socket.Error`. Each names its reason, and is handled like any
other [error](errors.md).

### Choosing a backend

Nothing above names a platform's API. The primitive operations which each role needs are
gathered into a `Socket.Backend`: bind and accept, connect and converse, receive and reply,
and dispatch a datagram. The loops which compose them stay neutral between platforms. Three
backends supply the same operations, so the same protocol code runs on the JVM, inside a
WebAssembly component, and in a native binary:

```tabular
column  Backend
column  Platform
column  Over
column  Unsupported
row  `socketBackends.javaBaseSockets`  JVM  `java.nio.channels`  nothing
row  `socketBackends.wasiSockets`  WebAssembly  `wasi:sockets`  Unix-domain sockets, UDP, multicast, TLS
row  `socketBackends.scalaNativeSockets`  Scala Native  Scala Native's sockets  Unix-domain sockets
```

An operation which a backend cannot support raises the appropriate error. It is never
approximated. Narrowing the platform's surface to a seam this small is
[decoupling](../philosophy/decoupling.md) applied within one library.

### Related

- [HTTP client](http-client.md) and [HTTP server](http-server.md) are built on these sockets.
- [Network addresses](network-addresses.md) describes the ports, IP addresses and DNS names
  used here.
- [Streams](streams.md) describes the streams which a connection reads and writes.
- [RPC](rpc.md) builds typed remote calls over a connection.
