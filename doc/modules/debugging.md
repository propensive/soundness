## Debugging

### About

A JVM can be debugged from another JVM over the
[Java Debug Wire Protocol](https://docs.oracle.com/en/java/javase/21/docs/specs/jdwp/jdwp-spec.html),
and these types speak it directly over a socket. A debuggee is launched under the debug agent,
or attached to where it already runs. The [session] which results sets breakpoints, steps
threads, reads frames and variables, evaluates expressions in the stopped program, and receives
the events of the VM as typed values. Nothing depends on `com.sun.jdi`.

The same session serves an editor over the
[Debug Adapter Protocol](https://microsoft.github.io/debug-adapter-protocol/).

### On debugging from the outside

The JVM's own debugging interface is a Java API which wraps a wire protocol, and it shows. The
types are mutable, the errors are unchecked, and the shape of a session, which is to connect,
suspend, request, resume and disconnect, is left to the caller to get right. Anything built on
it inherits all this, which is why debuggers are large programs and not small ones.

The protocol underneath is simple: length-prefixed packets, big-endian, with the widths of
identifiers negotiated once at the start of the connection. Modeling it directly gives typed
identifiers which cannot be confused with each other, a session confined to a scope by
[capture checking](../philosophy/capture-checking.md), and events as a stream to consume and
not callbacks to register.

Everything comes from the `soundness` package. A session runs its reader as a task, so it needs
a [thread model](concurrency.md) in scope and lives inside `supervise`:

```scala fuel
feature experimental.captureChecking
##
import soundness.*

import environments.javaBaseEnvironment
import errorDiagnostics.stackTracesDiagnostics
import internetAccess.online
import logging.silentLogging
import probates.awaitProbate
import socketBackends.javaBaseSockets
import strategies.throwUnsafely
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap
import threads.virtualThreads
import workingDirectories.javaBaseWorkingDirectory
```

The samples on this page launch and connect to other processes, so they are compiled but not
run.

### Opening a session

A `Debuggee` describes a program to run under the debug agent: a command, the port to listen on,
and whether the VM should suspend before running `main`, which a breakpoint in initialization
needs. Its `session` method lends a `Debug` [capability] to a block, which is reached through
`debug`:

```scala
def inspect(classpath: Text): List[Text] = supervise:
  val target: Debuggee = Debuggee(sh"java -cp $classpath com.example.Main", port = 5005)

  target.session:
    debug.threads().map(debug.name(_))
```

The command is launched with the agent option inserted, and the connection waits for the VM to
begin listening. When the block ends, whether it returns or fails, everything is torn down: the
socket is closed and the process is stopped.

A `Debugger` opens the same kind of session against an endpoint where a suspended VM is already
listening, for attaching to something which is already running.

The session handle is a capability confined to its block. It cannot escape, and so cannot be used
after the debuggee has gone.

### Threads and frames

The threads of the VM are listed, named, suspended and resumed, individually or together. The
call stack of a suspended thread reads as frames paired with their locations:

```scala
def stacks()(using Debug): Unit =
  debug.suspend()
  debug.threads().each: thread =>
    debug.frames(thread).each: (frame, location) =>
      Out.println(t"${debug.name(thread)}: ${location.index}")
  debug.resume()
```

### Breakpoints

A breakpoint is set on a source file and line, or on a class and method. It takes a *handler*,
which runs whenever a thread stops there.

The handler is lent a `Halt`: a view of the stopped thread, with its `frames()`, its
`variables()`, and the exception in flight if that is why it stopped. A `Halt` cannot outlive
the stop, since once the thread resumes, every identifier it handed out may be stale. A handler
which wants the thread left suspended says so with `remain()`. Otherwise the thread resumes when
the handler returns:

```scala
def watch()(using Debug): Unit =
  debug.breakpoint(t"Main.scala", Ordinal.uniary(42)): stop ?=>
    stop.variables().each: variable =>
      Out.println(t"${variable.name}: ${variable.erased}")

  debug.breakpoint(t"com.example.Ledger", t"deposit"): stop ?=>
    stop.remain()
```

Breakpoints on inlined code work by position. The file and line are where the code was written,
not where the compiler placed it, recovered from the source maps which the compiler records.

Each kind of request runs its handler on a different event:

```tabular
column  Request
column  Runs when
column  Event kind
row  `breakpoint`  a thread reaches a source line, a method's entry or a location  `Breakpoint`
row  `logpoint`  a thread reaches a location, printing a message and resuming at once  `Breakpoint`
row  `watch`  a field is written, or with `access = true` read  `FieldModified`, `FieldAccess`
row  `exceptions`  an exception is thrown  `Exception`
row  `step`  a stepping thread lands  `SingleStep`
```

A breakpoint returns the identifier of its request, which is what later clears it. The *suspend
policy* decides how much of the VM stops when the breakpoint is hit:

```tabular
column  Suspend policy
column  Stops
row  `Jdwp.SuspendPolicy.All`  the whole VM
row  `Jdwp.SuspendPolicy.EventThread`  only the thread that hit it
row  `Jdwp.SuspendPolicy.None`  nothing at all
```

```scala
def once(location: Jdwp.Location)(using Debug): Unit =
  val request = debug.breakpoint(location, Jdwp.SuspendPolicy.EventThread)
  debug.clear(Jdwp.EventKind.Breakpoint, request)
```

### Stepping

Stepping is a request against a suspended thread, with a handler for where it lands. The *depth*
says whether to step into a call (`Jdwp.StepDepth.Into`), over it (`Over`), or out of the
current frame (`Out`). An inlined call steps as the source reads, into the inlined body line by
line, and not as the bytecode was laid out:

```scala
def stepOver(thread: ThreadId)(using Debug): Unit =
  debug.step(thread, Jdwp.StepDepth.Over): stop ?=>
    stop.frames().head.let: (frame, location) =>
      Out.println(stop.describe(location)(0))
```

### Evaluating expressions

With the `Evaluator` in scope, an expression is compiled against the locals of the stopped frame
and injected into the running debuggee. So `total + 1` in a handler means what it would mean at
that line of the source. A debugger's console and hover use this, and it is also how an editor
shows the type which the compiler inferred for a name.

### Events

Underneath the handlers, the VM reports what happened as composite events. `events` is the
[stream](streams.md) of them, so the main loop of a debugger can be an ordinary traversal:

```scala
def trace()(using Debug): Unit =
  debug.events.each: composite =>
    composite.events.each:
      case Jdwp.Event.Breakpoint(request, thread, location) => Out.println(t"stopped at ${location.index}")
      case Jdwp.Event.SingleStep(request, thread, location) => Out.println(t"stepped to ${location.index}")
      case other                                            => ()
```

Requests and replies are correlated asynchronously. Many commands may be in flight at once, and
an event which arrives during one does not disturb it.

### Serving an editor

`Dap.listen` serves the session over the Debug Adapter Protocol on a TCP port. Any editor which
speaks it, which is VS Code and most others, can launch, stop, step, inspect variables and
evaluate through this implementation. Completions, hover types and inline-aware stepping all
pass through to the editor.

### The protocol vocabulary

`Jdwp` holds the model of the protocol itself, for work below the level which the session
exposes. The identifier types are tagged views of one [opaque type]. They cost nothing
at [runtime], and cannot be interchanged at [compiletime]:

```tabular
column  Type
column  Identifies
row  `ObjectId`  an object
row  `ThreadId`  a thread
row  `ThreadGroupId`  a thread group
row  `StringId`  a string object
row  `ClassLoaderId`  a class loader
row  `ReferenceTypeId`  a class, interface or array type
row  `MethodId`  a method
row  `FieldId`  a field
row  `FrameId`  a stack frame
```

Beside them sit `Jdwp.Location`, the tagged `Jdwp.Value`s, the `Jdwp.Modifier`s which constrain
an event request, and the `Jdwp.Event`s which a suspended VM sends back.

`Jdwp.Reader` and `Jdwp.Writer` are the hand-written big-endian codecs. They know the sizes of
identifiers negotiated with the VM when the session opened, which is why they are constructed
with those sizes rather than assuming them.

### Related

- [Processes](processes.md) describes the `sh"…"` commands which a debuggee is launched with.
- [Concurrency](concurrency.md) describes `supervise` and the thread models a session runs
  under.
- [Sockets](sockets.md) describes the TCP connections which the protocols travel over.
- [LSP](lsp.md) serves an editor over the sibling Language Server Protocol.
- [Stack traces](stack-traces.md) describes the frames of a stopped program as values.
- [Capture checking](../philosophy/capture-checking.md) explains how a session handle is
  confined to its block.
