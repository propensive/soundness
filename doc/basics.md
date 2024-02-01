## Running as a Daemon

Java is known for its slow startup time compared to other languages. While that has improved significantly in
more recent versions, waiting about a second is typical, even for a trivial "Hello world" application. This is
unacceptable for many purposes, in particular, providing dynamic tab-completions.

Furthermore, many programs run faster, the longer they have been running, thanks to the HotSpot just-in-time
compiler. This advantage returns to zero every time the application is started.

Ethereal makes it easy to write a program which runs as a daemon. The first time it is called, it starts the
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

Ethereal's launcher, which is written in Bash, is both portable and fast.

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




