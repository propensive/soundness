## Logging

### About

Soundness provides typesafe logging. A method that logs declares as much in its return
type, with the `logs` infix type, and emits a value of a typed event rather than a
string: instead of returning `Int` we would return `Int logs SomeEvent`. A logger placed in the caller's scope receives those events and writes them
somewhere — a terminal, a file, the system log. With no logger in scope, logging is
silent and costs nothing, so declaring that a method logs imposes no obligation on the
code that calls it.

Because a logger is an ordinary contextual value, several can be in scope at once, and
every event fans out to each of them. Each logger determines its own severity threshold, its
own rendering and its own destination. The decision of where logs go, and at what detail, belongs to the
application, not to the libraries it uses.

### On logging

Most logging is typed as strings, and is always on. A log statement formats a message
eagerly, whether or not anything will read it, and the cost — building the string,
evaluating its interpolations — is paid even when the level is disabled, so careful
code guards every statement with a level check. The message itself is an opaque
string, so a sink cannot reformat it, filter on its contents, or redact a field it did
not know was there.

Soundness separates the three concerns. A method states _that_ it logs, and _what_ it
logs, in its type; the event it emits carries typed fields, not formatted text;
and _where_ the event goes, and _how_ it renders, is decided by the loggers in scope.
The argument to a log call is passed by name and forced only if some sink will record
it, so a disabled statement evaluates nothing — log calls can be written freely,
without guards. When no logger is in scope at all, the machinery compiles away to
nothing.

The names come from the `soundness` package. A logging program also needs a rendering
format and the concurrency context a logger writes under:

```scala
import soundness.*
import logFormats.standardLogFormat
import probates.cancelProbate
import threading.virtualThreading
```

### Setting up a logger

A `Logger` writes events to a destination. Placing one in scope as a given makes it
the sink for everything logged within that scope. Its two type parameters say which
events it accepts — `Any` for all of them — and the form they are rendered through. A
logger writing to standard output is enough to see log output:

```scala
supervise:
  given Logger[Any, Message] = Logger(Out)
  // logging within here is written to standard output
```

Narrowing the first parameter filters by kind: a `Logger[HttpEvent, Message]` receives
HTTP events and nothing else, so the events of one library can be routed or silenced
independently of the rest.

`Logger` runs its writes on a background thread, so it needs the ambient concurrency
context that `supervise` provides. The format in scope — here `standardLogFormat`,
which prefixes each line with a timestamp and level — decides how an event renders to
text; `untimestampedLogFormat` omits the timestamp.

A threshold drops everything below a chosen level:

```scala
given Logger[Any, Message] = Logger(Out, level = Level.Warn)
```

### Logging a message

The simplest thing to log is a message, written with the `m"…"` interpolator, and this
suits application code, where the author controls both the logging and the reading. A
library should instead define a dedicated event type — described below — so its events
stay structured and filterable. Four methods log a message at a given level:

```scala
Log.fine(m"starting...")
Log.info(m"connected to $remote")
Log.warn(m"retrying after timeout")
Log.fail(m"could not open $path")
```

### Levels

A level reflects frequency and severity together:

- **`Fine`** — high-frequency, per-unit detail useful only while debugging: each frame,
  each packet, each file opened. Normally switched off, so it can be verbose.
- **`Info`** — one line per significant operation or lifecycle milestone: a connection
  established, a server now listening, a download finished.
- **`Warn`** — a recoverable or unexpected-but-handled condition: a forced kill, a
  connection reset, a fallback taken.
- **`Fail`** — an operation that failed unrecoverably, emitted alongside the error
  being raised. Used sparingly.

### Declaring that a method logs

A method opts into logging by naming its event type in its return type with `logs`:

```scala
def fetch(url: HttpUrl): HttpResponse logs HttpEvent = ...
```

The requirement propagates: a method that calls `fetch` must itself admit
`HttpEvent`, either by declaring it or by having a logger for it in scope. A method
that logs more than one kind of event chains the declarations:

```scala
def stop(server: Server): Unit logs HttpEvent logs ExecEvent = ...
```

### Event types

An event is an enumeration whose cases carry the relevant values as typed fields,
paired with a `Communicable` instance in its companion that maps each case to a
message. Keeping the values typed — rather than pre-formatting them — lets each sink
render them its own way:

```scala
object ExecEvent:
  given (ExecEvent is Communicable) =
    case ProcessStart(command)   => m"starting process $command"
    case AbortProcess(pid)       => m"the process with PID $pid was aborted"
    case KillProcess(pid)        => m"killed process with PID $pid"

enum ExecEvent:
  case ProcessStart(command: Command)
  case AbortProcess(pid: Pid)
  case KillProcess(pid: Pid)
```

The event is then logged by value, at the point the side effect happens:

```scala
Log.info(ExecEvent.ProcessStart(command))
```

A module that builds on another's work reuses its events rather than duplicating
them. Transcribing one event into a case of another threads the wrapped event through
unchanged — so a module that shells out reuses the process events of the shell:

```scala
given (GitEvent transcribes ExecEvent) = GitEvent.Exec(_)
```

### Cost when disabled

The argument to every `Log` method is taken by name, and the level is checked before
the argument is forced. When no logger accepts an event at its level — including when
no logger is in scope at all — the event is never constructed and its fields are never
evaluated. A disabled log statement therefore costs nothing, and log calls need no
guard.

This is why an event should carry its original typed values and leave formatting to
its `Communicable`: the rendering happens only if the event is actually recorded.
The exception is a value whose rendering depends on context available only at the call
site, which should be rendered there, into the event:

```scala
Log.info(IoEvent.Move(source.show, destination.show))
```

The `show` still runs only when the event is logged, because the argument is by name.

### Routing to several places

Because a logger is just a contextual value, more than one can be in scope, and an
event reaches every one. Two loggers — one to a file, one to the terminal — both
receive each event, and each applies its own threshold, so the file can keep `Fine`
detail while the terminal shows only `Warn` and `Fail`:

```scala
given fileLog: Logger[Any, Message] = Logger(logFile, level = Level.Fine)
given terminalLog: Logger[Any, Message] = Logger(Out, level = Level.Warn)
```

### Silencing

A single block can be logged with everything suppressed, regardless of the loggers in
scope, with `mute`:

```scala
mute[ExecEvent]:
  // ExecEvent logging is suppressed here
```

Importing `logging.silentLogging` suppresses all logging for a whole file, which is
useful where a dependency logs but the output is unwanted.

### Writing good logs

Log only observable side effects that have latency or a failure mode — network,
filesystem, process, and external-service operations. Pure, in-memory work such as
hashing, parsing, or arithmetic produces no log output, however expensive.

A message should be specific. Name the resource it concerns — an address and port, a
path, a process id, a URL — and quantify when it is cheap to do so, with byte counts,
elapsed milliseconds, or status codes. Never log secrets: credentials, tokens,
cookies, key material, or whole request and response bodies. Log a redaction, a size,
or a short preview instead.

Messages are lowercase, terse, present-tense, and carry no trailing period,
interpolating the relevant context:

```scala
m"connected to $remote"
m"opened $path for reading"
m"downloaded $url ($size bytes in ${elapsed}ms)"
```

A message that says only that something happened — `m"a failure occurred"` — is not
enough; name what failed and include the identifier.
