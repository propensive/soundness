[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/spectral/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/spectral/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Spectral

__Launch Scala applications lightning-fast by running them as daemons.__

_Spectral_ provides the capability to turn an existing Scala command-line application into a daemon for fast startup
(like [Nailgun](https://github.com/facebookarchive/nailgun)). This is particularly useful when combined with
[Exoskeleton](https://github.com/propensive/exoskeleton) for tab completions.

Spectral's launcher script is written in around 100 lines of zero-dependency Bash, and can be used to transform a
Scala application into a self-contained, portable executable.

## Features

- fast daemon process for near-instantaneous invocations
- safe concurrent invocations so that at most one daemon is running at any time
- portable launcher written in Bash
- facilitates single-file distributions of Scala executables
- allows easy bundling with a JDK or JRE for distribution as a single file
- support for messaging between clients



## Availability

Spectral has not yet been published as a binary.

## Getting Started

## Running as a Daemon

Java is known for its slow startup time compared to other languages. While that has improved significantly in
more recent versions, waiting about a second is typical, even for a trivial "Hello world" application. This is
unacceptable for many purposes, in particular, providing dynamic tab-completions.

Furthermore, many programs run faster, the longer they have been running, thanks to the HotSpot just-in-time
compiler. This advantage returns to zero every time the application is started.

Spectral makes it easy to write a program which runs as a daemon. The first time it is called, it starts the
JVM as a background process which listens for socket connections on a free local port, and all subsequent
invocations connect to the running JVM through sockets.

A "launcher script" written in Bash handles this and a few other tasks:
1. connecting to a port if the daemon is already running
2. launching the daemon if it is not running
3. forwarding environment information (environment variables, working directory, etc.) to the daemon
4. switching to an appropriate TTY mode
5. handling standard input and output
6. forwarding interrupts received by the script to the daemon
7. returning the exit status when the request has been handled

Nailgun provides two launcher clients written in C and Python. Unfortunately, the former must be installed
separately, distributed as multiple binaries for different CPU architectures, or compiled with a C compiler
before use. And the latter has noticeably poor performance for each invocation.

Spectral's launcher, which is written in Bash, is both portable and fast.

The daemon implementation is an ordinary method call which primarily specifies the code to be invoked for each
invocation, and would typically be the implementation of a main method,
```scala
@main
def myapp(): Unit = daemon:
  Out.println(t"Hello world")
  ExitStatus.Ok
```
or:
```scala
object MyApp:
  def main(args: IArray[String]): Unit = daemon:
    Out.println(t"Hello world")
    ExitStatus.Ok
```

When invoking this `main` method, the code inside the `daemon` block is not run immediately, but a server is
started which listens on a free port, and each time it receives a request on that port, _then_ will the code in
the block be executed. The context of a `daemon` block provides all the same context as an `application` block,
so it's very easy to convert a non-daemon application into a daemon.

Note that unlike an `application` block, no arguments need to be passed to the the `daemon` block, but the
`arguments` and `parameters` methods can be used to get the arguments or parameters for the current invocation.

The `Out` object from [Turbulence](https://github.com/propensive/turbulence/) (or `Err`) must be used for
producing output, since the default `println` method defined in Scala's `Predef` prints to the JVM's global
`System.out`, which corresponds to standard output for the _daemon_ process; not any of the clients. Unless
`System.out` is configured to do something else, its output will be lost.

### Packaging

The launcher script is designed to be invoke a self-contained "fat JAR" file with a `Main-Class` declaration in
its manifest. But furthermore, it can be bundled with the JAR as a single executable file.

This is possible thanks to a feature of JAR files (which are just ZIP files with a different extension): the
index of a ZIP file is at the end, and refers to data within the file as a (negative) offset from the end. This
means that a ZIP file can be prefixed with unrelated data, without affecting its contents or ability to be read.

So a new file consisting of the launcher script followed by the JAR file can be distributed as a single,
standalone, portable, executable file. The launcher script simply calls `java -jar` passing _its own path_ as
the parameter, since it is simultaneously both a script and a JAR file.

The executable can be packaged using,
```bash
cat launcher application.jar > application
chmod +x application
```

### Messaging between daemon clients

An application running as a daemon can, of course, have multiple clients connected simultaneously. Normally,
these would be independent, with no interaction between them.

However, there are times when it's useful for one client to communicate with the others, and a _bus_ is provided
to facilitate this. A bus allows one client to pass messages of some type to _all_ the other clients. It's not
possible to send messages just to one particular client, although any client could decide whether a particular
message is relevant to it.

To use the bus facility, the daemon should have a type parameter specified when it is invoked, for example:
```scala
@main
def myapp(): Unit = daemon[Text]:
  // body
```

This may be any `Matchable` type, and determines what values may be broadcast to other clients, and conversely,
what values may be received (and therefore should be handled).

A message may be sent to all other clients by calling `broadcast` from within a `daemon` block, for example,
```scala
enum BusMessage:
  case Hello, Goodbye

def myapp(): Unit = daemon[BusMessage]:
  broadcast(BusMessage.Hello)
  // do something
  broadcast(BusMessage.Goodbye)
```

If such messages are sent, then they ought to be received too! The contextual method, `bus`, provides a stream
of messages which can be processed while the client is running. However, while the clients may be doing
different things at different times, they are homogeneous, so in order to be useful, the same client
implementation should be both sending and receiving messages on the bus.

A typical implementation might multiplex the event stream from a
[Profanity](https://github.com/propensive/profanity/) terminal with the bus, and handle keypresses and messages
from other clients in an event loop.




## Status

Spectral is classified as ____. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Spectral is designed to be _small_. Its entire source code currently consists
of 242 lines of code.

## Building

Spectral can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Spectral are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/spectral/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Spectral easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Spectral was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

_Spectral_ alludes to the paranormal concept of spectres, a type of daemon.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows a spectrum of light, and is thus _spectral_.

## License

Spectral is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
