## Debugging

<!-- doccheck: language captureChecking -->

### About

A JVM can be debugged from another JVM over the
[Java Debug Wire Protocol](https://docs.oracle.com/en/java/javase/21/docs/specs/jdwp/jdwp-spec.html),
and Soundness speaks it directly over a socket. A debuggee is launched under the agent or attached
to where it already runs, and the session that results sets breakpoints, steps threads, reads
frames and variables, evaluates expressions in the stopped program, and receives the VM's events
as typed values — with no dependency on `com.sun.jdi`. The same session serves an editor over the
[Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/).

### On debugging from the outside

The JVM's own debugging interface is a Java API wrapping a wire protocol, and it shows: the types
are mutable, the errors are unchecked, and the shape of a session — connect, suspend, request,
resume, disconnect — is left to the caller to get right. Anything built on it inherits that,
which is why debuggers are large programs rather than small ones.

The protocol underneath is straightforward: length-prefixed packets, big-endian, with identifier
widths negotiated once at the start of the connection. Modeling it directly gives typed
identifiers that cannot be confused for each other, a session confined to a scope by
[capture checking](../philosophy/capture-checking.md), and events as a stream to consume rather
than callbacks to register. Everything comes from the `soundness` package; a session runs its
reader as a task, so it needs a [thread model](concurrency.md) in scope and lives inside
`supervise`:

```scala
import soundness.*

import probates.awaitProbate
import strategies.throwUnsafely
import stdios.javaLangSystemStdio
import threading.virtualThreading
```

### Opening a session

A `Debuggee` describes a program to run under the debug agent — a command, the port to listen on,
and whether the VM should suspend before running `main`, which is what a breakpoint in
initialization needs. Its `session` lends a `Debug` capability to a block, reached through
`debug`:

```scala
def inspect(classpath: Text): List[Text] = supervise:
  val target = Debuggee(sh"java -cp $classpath com.example.Main", port = 5005)

  target.session:
    debug.threads().map(debug.name(_))
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
def stacks()(using Debug^): Unit = debug.suspend() yet
  debug.threads().each: thread =>
    debug.frames(thread).each: (frame, location) =>
      Out.println(t"${debug.name(thread)}: ${location.index}")
  debug.resume()
```

### Breakpoints

A breakpoint is set on a source file and line, or on a class and method, and takes a *handler*
that runs whenever a thread stops there. The handler is lent a `Halt`: a view over the stopped
thread — its `frames()`, its `variables()`, the exception in flight if that is why it stopped —
which cannot outlive the stop, since once the thread resumes every identifier it handed out may
be stale. A handler that wants the thread left suspended says so with `remain()`; otherwise the
thread resumes when the handler returns:

```scala
def watch()(using Debug^): Unit =
  debug.breakpoint(t"Main.scala", Ordinal.uniary(42)): stop ?=>
    stop.variables().each: variable =>
      Out.println(t"${variable.name}: ${variable.erased}")

  debug.breakpoint(t"com.example.Ledger", t"deposit"): stop ?=>
    stop.remain()
```

Breakpoints on inlined code work by position: the file and line where the code was written
rather than where the compiler placed it, recovered from the source maps the compiler records.
A `logpoint` prints a message at a location without stopping; `watch` stops when a field is read
or written; `exceptions` stops when one is thrown. The request identifier a breakpoint returns is
what later clears it, and the *suspend policy* decides how much of the VM stops when it is hit —
the whole VM, only the thread that hit it, or nothing at all:

```scala
def once(location: Jdwp.Location)(using Debug^): Unit =
  val request = debug.breakpoint(location, Jdwp.SuspendPolicy.EventThread)
  debug.clear(Jdwp.EventKind.Breakpoint, request)
```

### Stepping

Stepping is a request against a suspended thread, with a handler for where it lands. The *depth*
says whether to step into a call, over it, or out of the current frame. Inlined calls step as the
source reads — into the inlined body, line by line — rather than as the bytecode was laid out:

```scala
def stepOver(thread: ThreadId)(using Debug^): Unit =
  debug.step(thread, Jdwp.StepDepth.Over): stop ?=>
    stop.frames().prim.let: (frame, location) =>
      Out.println(stop.describe(location)(0))
```

### Evaluating expressions

With the `Evaluator` in scope, an expression is compiled against the stopped frame's locals and
injected into the running debuggee, so `total + 1` in a handler means what it would mean at that
line of the source. This is what a debugger's console and hover use, and it is also how an editor
shows the compiler-inferred type of a name.

### Events

Underneath the handlers, the VM reports what happened as composite events, and `events` is the
stream of them, so a debugger's main loop can equally be an ordinary traversal:

```scala
def trace()(using Debug^): Unit =
  debug.events.each: composite =>
    composite.events.each:
      case Jdwp.Event.Breakpoint(request, thread, location) => Out.println(t"stopped at ${location.index}")
      case Jdwp.Event.SingleStep(request, thread, location) => Out.println(t"stepped to ${location.index}")
      case other                                            => ()
```

Requests and replies are correlated asynchronously, so many commands may be in flight at once and
an event arriving during one does not disturb it.

### Serving an editor

`Dap.listen` serves the session over the Debug Adapter Protocol on a TCP port, so any editor that
speaks it — VS Code, and most others — launches, stops, steps, inspects variables and evaluates
through this implementation. Completions, hover types and inline-aware stepping all pass through
to the editor.

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
