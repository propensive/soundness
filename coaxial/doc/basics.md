All terms and types are defined in the `coaxial` package, which can be imported
with:
```scala
import coaxial.*
```

### Transmitting data

In general, a client connects to a listing server by calling either `transmit`
(for stateless transmission) or `connect` (to start a stateful connection) on
an endpoint. The endpoint type determines the transmission protocol.

Endpoint types include,
 - local UDP ports, e.g. `udp"3480"`
 - local TCP ports, e.g. `tcp"2205"`
 - remote ports, e.g. `host"example.com" on tcp"8844"` or
   `ip"192.168.1.24" on udp"1280"`
 - UNIX domain sockets
 - any type for which an `Addressable` or `Connectable` typeclass instance
   exists

For example, a simple message could be sent to a local UDP port with,
```scala
udp"7327".transmit(t"Hello world!")
```
This will send a single UDP packet containing the bytes of the string `Hello
world!`, and nothing further. This is useful for "fire-and-forget" messaging.

The payload of the UDP packet is a `Text` string, but can be any type for which
a `Transmissible` typeclass instance exists, which includes `Text`, `Bytes` and
anything that has a [Spectacular](https://github.com/propensive/spectacular/)
`Encoder`.

All `Addressable` endpoints can have data transmitted to them, but some, such
as TCP ports, can initiate a bidirectional connection. These are those
endpoints for which a `Connectable` typeclass instance exists. `Connectable` is
a subtype of `Addressable`.

Starting a stateful connection, with the `connect` method, requires a bit more
work. Its state is user-defined, and can be represented by any type. So a
connection requires the following:
 - the initial state
 - the type of the state, if it can't be inferred from the initial state
 - the initial message, in a type wich is `Transmissible`
 - a lambda to interact with the server, interpreting responses, modifying
   state, and sending further messages

Of these, the lambda is where most of the work is done. It will be executed
sequentially for every chunk of data, as `Bytes`, received from the server, and
must choose how to process that data.

The return type of the lambda must be one of the following `Control` values:
 - `Reply(message)`, to respond to the server with a new message on the current
   connection
 - `Continue(state)`, to update the state to a new value, and wait for more data
   from the server
 - `Terminate`, to abort the connection
 - `Conclude(message, state)`, to respond to the server with a final message,
   and update the state to a new value

Within the body of the lambda, the named contextual value, `state`, provides
the current state.

The return type of `connect` will be the final state value.

Here is an trivial example:
```scala
(host"remote.com" on tcp"1920").connect(t"")(t"DATA\n"): bytes =>
  val text = bytes.as[Text]
  val state2 = state+text
  if state2.length < 100 then Control.Continue(state2)
  else Control.Conclude(t"END\n", state2)
```

This will connect to TCP port 1920 on host `remote.com`, sending the initial
request message, `DATA`. When the server (hopefully) responds with some textual
data, we convert the bytes to text and append them to the existing state,
accessed with the named contextual value, `state`. If we have less than 100
characters of text, then we continue waiting, updating the state to the new
value. But if we have already received more than 100 characters, we conclude
the connection by sending a final message, `END`, to the server, updating the
state to its final value. The return value will be at least 100 characters of
text.

It should be clear from this example that more complex connection handlers are
possible, and can be written in a style that's similar to folding over a
stream.

The functional interface of `connect`, whose lambda _requires_ a `Control`
value to be specified means that it's impossible to forget how to handle the
input; the programmer must decide to reply, continue, conclude or terminate the
connection, otherwise the code will not compile.

### Implementing a Server

Writing a server can be simpler than sending a client request. The `listen`
method is available on any type with a `Bindable` typeclass instance, but this
notably includes:
 - UNIX domain sockets
 - TCP ports
 - UDP ports

We can write a server by calling the `listen` method on a `Bindable` type, and
specifying, by means of a lambda, how a request should be handled.

Calling `listen` will start a new server in a separate thread. Any incoming
request will dispatch it to the handler lambda. The type of the input will
depend on the type of the local endpoint. So a stateful connection will provide
an input type which can support an interactive session, whereas a stateless
connection will provide just the payload.

The return value of the lambda will also be determined from the endpoint type,
as appropriate for stateful and stateless connections.

As an example, a simple UDP server takes input as a `UdpPacket` and returns a
`UdpResponse`. `UdpPacket` is a case class with fields: `data`, the payload
bytes; `sender`, an `Ipv4` or `Ipv6` source address; and `port`, the remote UDP
port.

`UdpResponse` is an enumeration, which is either `Ignore` (if no response is
expected to the UDP packet), or `Reply(payload)` with the bytes to respond
with.

So a UDP server which responds to `PING` messages with `PONG`, but ignores
other messages could be implemented with just:
```scala
val server = udp"1722".listen: packet =>
  if packet.as[Text] == t"PING" then Reply(t"PONG") else Ignore
```

The return value, `server`, is a `SocketService` representing the server
running in the background, and will be returned as soon as the port has been
bound. It may be stopped at any time by calling its `stop` method, like so:
```scala
server.stop()
```



