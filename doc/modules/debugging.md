## Debugging

### About

A JVM can be debugged from another JVM over the
[Java Debug Wire Protocol](https://docs.oracle.com/en/java/javase/21/docs/specs/jdwp/jdwp-spec.html),
and Soundness speaks it directly over a socket. A debuggee is launched under the agent or attached
to where it already runs, and the session that results sets breakpoints, steps threads, reads
frames and receives the VM's events as typed values — with no dependency on `com.sun.jdi`.

### On debugging from the outside

The JVM's own debugging interface is a Java API wrapping a wire protocol, and it shows: the types
are mutable, the errors are unchecked, and the shape of a session — connect, suspend, request,
resume, disconnect — is left to the caller to get right. Anything built on it inherits that,
which is why debuggers are large programs rather than small ones.

The protocol underneath is straightforward: length-prefixed packets, big-endian, with identifier
widths negotiated once at the start of the connection. Modelling it directly gives typed
identifiers that cannot be confused for each other, a session confined to a scope by
[capture checking](../philosophy/capture-checking.md), and events as a stream to consume rather
than callbacks to register. Everything comes from the `soundness` package:

```scala
import soundness.*
import strategies.throwUnsafely
```

### Opening a session

A `Debuggee` describes a program to run under the debug agent — a command, the port to listen on,
and whether the VM should suspend before running `main`, which is what a breakpoint in
initialization needs:

```scala
val target = Debuggee(sh"java -cp $classpath com.example.Main", port = 5005)

target.session:
  session.threads()
```

The command is launched with the agent option inserted, the connection waits for the VM to begin
listening, and everything is torn down when the block ends — the socket closed, the process
stopped — whether the block returns or fails. A `Debugger` opens the same kind of session against
an endpoint where a suspended VM is already listening, for attaching to something already running.

The session handle is a capability confined to its block, so it cannot escape and be used after
the debuggee has gone.

### Threads and frames

The VM's threads are listed, named, suspended and resumed individually or together, and a
suspended thread's call stack reads as frames paired with their locations:

```scala
session.suspend()

session.threads().each: thread =>
  Out.println(session.name(thread))
  session.frames(thread).each: (frame, location) =>
    report(location)

session.resume()
```

### Breakpoints and stepping

A breakpoint is an event request at a location, and the request identifier it returns is what
later clears it. The *suspend policy* decides how much of the VM stops when it is hit — the whole
VM, only the thread that hit it, or nothing at all:

```scala
val request = session.breakpoint(location, Jdwp.SuspendPolicy.EventThread)

session.clear(Jdwp.EventKind.Breakpoint, request)
```

Stepping is a request too, made against a suspended thread. The *depth* says whether to step into
a call, over it, or out of the current frame, and the *size* whether a step is one bytecode
instruction or one source line:

```scala
session.step(thread, Jdwp.StepDepth.Into, Jdwp.StepSize.Line)
```

### Events

The VM reports what happened as composite events, and `events` is the stream of them, so a
debugger's main loop is an ordinary traversal rather than a set of registered callbacks:

```scala
session.events.each: composite =>
  composite.events.each:
    case Jdwp.Event.Breakpoint(request, thread, location) => report(thread, location)
    case Jdwp.Event.SingleStep(request, thread, location) => report(thread, location)
    case other                                            => ()
```

Requests and replies are correlated asynchronously, so many commands may be in flight at once and
an event arriving during one does not disturb it.

### The protocol vocabulary

`Jdwp` holds the model of the protocol itself, for work below the level the session exposes.
The identifier types — `ObjectId`, `ThreadId`, `ThreadGroupId`, `StringId`, `ClassLoaderId`,
`ReferenceTypeId`, `MethodId`, `FieldId`, `FrameId` — are tagged views of one opaque reference
type, so they are free at runtime and not interchangeable at compiletime. Alongside them sit
`Jdwp.Location`, the tagged `Jdwp.Value`s, the `Jdwp.Modifier`s that constrain an event request,
and the `Jdwp.Event`s a suspended VM sends back.

`Jdwp.Reader` and `Jdwp.Writer` are the hand-written big-endian codecs, aware of the identifier
sizes negotiated with the VM when the session opened — which is why they are constructed with
those sizes rather than assuming them.
