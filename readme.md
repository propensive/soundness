[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/coaxial/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/coaxial/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Coaxial

__Socket handling for Scala__

_Coaxial_ provides a generic framework for client/server communications over a
network, abstracting over protocol differences, including statefulness.
Implementations are provided for sending and receiving UDP packets, and
connecting and serving TCP connections.

## Features

- provides an abstract framework for data transmission through sockets
- supports both stateful (e.g. TCP) and stateless (e.g. UDP) connections
- server and client implementations for UDP, TCP and UNIX domain sockets
- allows servers and clients to process connections as state machines
- uses a safe, functional API


## Availability Plan

Coaxial has not yet been published. The medium-term plan is to build Coaxial
with [Fury](https://github.com/propensive/fury) and to publish it as a source build on
[Vent](https://github.com/propensive/vent). This will enable ordinary users to write and build
software which depends on Coaxial.

Subsequently, Coaxial will also be made available as a binary in the Maven
Central repository. This will enable users of other build tools to use it.

For the overeager, curious and impatient, see [building](#building).

## Getting Started

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





## Status

Coaxial is classified as __embryotic__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, can still be used,
as long as caution is taken to avoid a mismatch between the project's stability
level and the required stability and maintainability of your own project.

Coaxial is designed to be _small_. Its entire source code currently consists
of 295 lines of code.

## Building

Coaxial will ultimately be built by Fury, when it is published. In the
meantime, two possibilities are offered, however they are acknowledged to be
fragile, inadequately tested, and unsuitable for anything more than
experimentation. They are provided only for the necessity of providing _some_
answer to the question, "how can I try Coaxial?".

1. *Copy the sources into your own project*
   
   Read the `fury` file in the repository root to understand Coaxial's build
   structure, dependencies and source location; the file format should be short
   and quite intuitive. Copy the sources into a source directory in your own
   project, then repeat (recursively) for each of the dependencies.

   The sources are compiled against the latest nightly release of Scala 3.
   There should be no problem to compile the project together with all of its
   dependencies in a single compilation.

2. *Build with [Wrath](https://github.com/propensive/wrath/)*

   Wrath is a bootstrapping script for building Coaxial and other projects in
   the absence of a fully-featured build tool. It is designed to read the `fury`
   file in the project directory, and produce a collection of JAR files which can
   be added to a classpath, by compiling the project and all of its dependencies,
   including the Scala compiler itself.
   
   Download the latest version of
   [`wrath`](https://github.com/propensive/wrath/releases/latest), make it
   executable, and add it to your path, for example by copying it to
   `/usr/local/bin/`.

   Clone this repository inside an empty directory, so that the build can
   safely make clones of repositories it depends on as _peers_ of `coaxial`.
   Run `wrath -F` in the repository root. This will download and compile the
   latest version of Scala, as well as all of Coaxial's dependencies.

   If the build was successful, the compiled JAR files can be found in the
   `.wrath/dist` directory.

## Contributing

Contributors to Coaxial are welcome and encouraged. New contributors may like
to look for issues marked
[beginner](https://github.com/propensive/coaxial/labels/beginner).

We suggest that all contributors read the [Contributing
Guide](/contributing.md) to make the process of contributing to Coaxial
easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Coaxial was designed and developed by Jon Pretty, and commercial support and
training on all aspects of Scala 3 is available from [Propensive
O&Uuml;](https://propensive.com/).



## Name

_Coaxial_ cable is designed for the transmission of high-frequency data with
minimal loss, alluding to the function of transmission of data that this
library provides.

In general, Scala One project names are always chosen with some rationale,
however it is usually frivolous. Each name is chosen for more for its
_uniqueness_ and _intrigue_ than its concision or catchiness, and there is no
bias towards names with positive or "nice" meanings—since many of the libraries
perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it
should be noted how willingly English adopts foreign words. Names are generally
of Greek or Latin origin, and have often arrived in English via a romance
language.

## Logo

The logo shows a cross-section of a coaxial connector.

## License

Coaxial is copyright &copy; 2024 Jon Pretty & Propensive O&Uuml;, and
is made available under the [Apache 2.0 License](/license.md).

