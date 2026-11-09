## Logging

### About

Logging is typed. A method which logs says so in its return type, with the infix type
`logs`, and emits a value of a typed event, not a string. Instead of returning `Long`, it
returns `Long logs Backup.Event`.

A logger which is placed in the caller's scope receives those events, and writes them
somewhere: a terminal, a file, or the system log. With no logger in scope, logging is
silent, and costs nothing, so declaring that a method logs imposes no obligation on the code
which calls it. A `logs` clause is an
[honest signature](../philosophy/honest-signatures.md): it states what the method actually
emits, so a caller knows what there is to route.

A logger is an ordinary [contextual value]. Several can be in scope at once, and every event
goes to each of them. Each logger has its own threshold of severity, its own format and its
own destination. The decision of where logs go, and in what detail, belongs to the
application, not to the libraries which it uses. Choosing that by what is in scope is
[declarative context](../philosophy/declarative-context.md).

### On logging

Most logging is typed as strings, and is always on. A log statement formats its message
eagerly, whether or not anything will read it. The cost of building the string is paid even
when the level is disabled, so careful code guards every statement with a check of the
level. The message itself is an opaque string, so whatever receives it cannot reformat it,
filter on its contents, or redact a field which it did not know was there.

Here the three concerns are separate:

- A method states *that* it logs, and *what* it logs, in its type.
- The event which it emits has typed fields, not formatted text.
- *Where* the event goes, and *how* it is written, is decided by the loggers in scope.

The argument to a log call is passed by name, and is evaluated only if some logger will
record it. A disabled statement evaluates nothing, so log calls can be written freely,
without guards. When no logger is in scope at all, the machinery compiles away to nothing.

Everything comes from the `soundness` package. A program which logs also needs a format,
somewhere to write, and the [concurrency](concurrency.md) context which a logger writes
under:

```scala
import soundness.*

import logFormats.ansiTimestampedLogFormat
import probates.awaitProbate
import stdios.javaLangSystemStdio
import strategies.throwUnsafely
import termcapDefinitions.xterm256Termcap
import threads.virtualThreads
import writables.outTeletypeWritable

given palette: LogPalette = new Palette:
  type Form = Srgb
  def background: Color in Srgb  = WebColors.Black
  def foreground: Color in Srgb  = WebColors.White
  def error: Color in Srgb       = WebColors.Red
  def warning: Color in Srgb     = WebColors.Orange
  def informative: Color in Srgb = WebColors.SteelBlue
  def subdued: Color in Srgb     = WebColors.DimGray
```

```tabular
column  Import or given
column  Decides
row  `logFormats.ansiTimestampedLogFormat`  how an event is written: here, with color, for a terminal
row  the `LogPalette`  the colors which that format uses
row  `writables.outTeletypeWritable`  that standard output can be written to as styled text
row  `stdios`, `termcapDefinitions`  where standard output goes, and what the terminal can display
row  `threads`, `probates`  how the logger's background task runs
row  `strategies.throwUnsafely`  that an error is thrown as an exception
```

### Setting up a logger

A `Logger` writes events to a destination. Placing one in scope, as a [given], makes it the
destination for everything which is logged within that scope. Its two type parameters say
which events it accepts, with `Any` for all of them, and the form in which they are written.
A logger which writes to standard output is enough to see log output:

```scala fuel
typecheck
##
supervise:
  given logger: Logger[Any, Message] = Logger(Out)
  Log.info(m"the logger is in scope")
```

The message is written to standard output, with a timestamp.

Narrowing the first parameter filters by kind. A `Logger[Http.Event, Message]` receives HTTP
events, and nothing else, so the events of one library can be routed, or silenced,
independently of the rest.

`Logger` runs its writes on a background thread, so it needs the concurrency context which
`supervise` provides. The format in scope decides how an event is written. Each format is
imported from the package `logFormats`:

```tabular
column  Format
column  Renders
column  For
row  `ansiTimestampedLogFormat`  a dimmed timestamp and the level on a colored ground  a terminal
row  `timestampedLogFormat`  the timestamp and level as text  a plain-text destination, such as a file
row  `untimestampedLogFormat`  the level as text, with no timestamp  a plain-text destination
```

A threshold drops everything below a chosen level:

```scala fuel
typecheck
##
supervise:
  given logger: Logger[Any, Message] = Logger(Out, level = Level.Warn)
  Log.info(m"not written")
  Log.warn(m"written")
```

### Logging a message

The simplest thing to log is a message, written with the
[`m"…"` interpolator](errors.md#defining-an-error). That suits the code of an application,
where the author controls both the logging and the reading. A library should instead define
a dedicated [event type](#event-types), so that its events stay structured, and can be
filtered. Four methods log a message, each at a level:

```scala
def report(remote: Text, path: Text): Unit =
  Log.fine(m"starting...")
  Log.info(m"connected to $remote")
  Log.warn(m"retrying after timeout")
  Log.fail(m"could not open $path")
```

Without a logger in scope, `report` compiles, and logs nothing. A message is `Loggable` by
default, so a method which logs only messages needs no declaration.

### Levels

A level reflects frequency and severity together. `Fine` is normally switched off, so it can
be verbose. `Fail` is used sparingly.

```tabular
column  Level
column  Method
column  For
column  Such as
row  `Fine`  `Log.fine`  high-frequency, per-unit detail useful only while debugging  each frame, each packet, each file opened
row  `Info`  `Log.info`  one line per significant operation or lifecycle milestone  a connection established, a server now listening, a download finished
row  `Warn`  `Log.warn`  a recoverable or unexpected-but-handled condition  a forced kill, a connection reset, a fallback taken
row  `Fail`  `Log.fail`  an operation that failed unrecoverably, emitted alongside the error being raised
```

### Event types

An event is an [enumeration] whose cases hold the relevant values as typed fields. It is
paired with a `Communicable` instance in its companion, which maps each case to a message.
Keeping the values typed, instead of formatting them in advance, lets each logger write them
in its own way:

```scala
object Backup:
  enum Event:
    case Started(source: Text)
    case Copied(source: Text, bytes: Long)
    case Failed(source: Text)

  object Event:
    given communicable: (Event is Communicable) =
      case Started(source)       => m"backing up $source"
      case Copied(source, bytes) => m"copied $source ($bytes bytes)"
      case Failed(source)        => m"could not back up $source"
```

### Declaring that a method logs

A method opts into logging by naming its event type in its return type, with `logs`. It then
logs the event by value, at the point where the side effect happens:

```scala
def backup(source: Text): Long logs Backup.Event =
  Log.info(Backup.Event.Started(source))
  val bytes = source.length.toLong
  Log.info(Backup.Event.Copied(source, bytes))
  bytes
```

The requirement propagates. A method which calls `backup` must itself admit `Backup.Event`,
either by declaring it, or by having a logger for it in scope. A method which logs more than
one kind of event chains the declarations, as in `Unit logs Http.Event logs Exec.Event`.

A library which builds on another's work reuses its events, and does not duplicate them.
*Transcribing* one event into a case of another passes the wrapped event through unchanged.
So a library which schedules backups reuses the backup events:

```scala
enum Schedule:
  case Ran(event: Backup.Event)
  case Skipped(reason: Text)

object Schedule:
  given communicable: (Schedule is Communicable) =
    case Ran(event)       => m"scheduled: ${event.communicate}"
    case Skipped(reason)  => m"skipped: $reason"

given transcription: (Schedule transcribes Backup.Event) = Schedule.Ran(_)

def nightly(): Unit logs Schedule =
  backup(t"/home")
  Log.info(Schedule.Skipped(t"nothing else to do"))
```

With the transcription in scope, the events of `backup` are accepted by the declaration of
`nightly`, wrapped as `Schedule.Ran`, and a logger for `Schedule` sees both.

### Cost when disabled

The argument to every `Log` method is taken by name, and the level is checked before the
argument is evaluated. When no logger accepts an event at its level, which includes the case
where no logger is in scope at all, the event is never constructed, and its fields are never
evaluated. A disabled log statement therefore costs nothing, and log calls need no guard.

This is why an event should hold its original typed values, and leave formatting to its
`Communicable`: the formatting happens only if the event is actually recorded. The exception
is a value whose formatting depends on context which is available only where it is logged.
That should be formatted there, into the event. The formatting still runs only when the
event is logged, because the argument is passed by name.

### Routing to several places

A logger is just a contextual value, so more than one can be in scope, and an event reaches
every one. Two loggers, one to a file and one to the terminal, both receive each event, and
each applies its own threshold. The file can keep the detail of `Fine`, while the terminal
shows only `Warn` and `Fail`.

The file is written as text, so its logger imports the text format. That import is nearer,
so it takes precedence over the terminal format which was imported above:

```scala fuel
typecheck
##
import codepages.utf8Codepage
import filesystemBackends.javaBaseFilesystem
import pathInterfaces.pathOnLinux
import temporaryDirectories.javaBaseTemporaryDirectory

val logFile = temporaryDirectory[Path on Linux] / "logging" / "app.log"
logFile.create[File](CreateFlag.Parents)

supervise:
  given fileLog: Logger[Any, Message] =
    import logFormats.timestampedLogFormat
    Logger(logFile, level = Level.Fine)

  given terminalLog: Logger[Any, Message] = Logger(Out, level = Level.Warn)
  Log.fine(m"only the file sees this")
  Log.warn(m"both see this")
```

The first message reaches only the file, and the second reaches both.
[Filesystem](filesystem.md) describes the path which the file logger writes to.

### Silencing

A single block can be run with one kind of event suppressed, whatever loggers are in scope,
with `mute`:

```scala fuel
typecheck
##
supervise:
  given logger: Logger[Any, Message] = Logger(Out)

  mute[Backup.Event]:
    backup(t"/var")
```

Importing `logging.silentLogging` suppresses all logging for a whole file, which is useful
where a dependency logs, and its output is not wanted. The tutorials for the
[HTTP client](http-client.md) and [processes](processes.md) import it for that reason.

### Writing good logs

Log only side effects which can be observed, and which take time or can fail: operations on
the network, the filesystem, processes and external services. Pure work in memory, such as
hashing, parsing or arithmetic, produces no log output, however expensive it is.

A message should be specific:

- Name the resource which it concerns: an address and port, a path, a process id, a URL.
- Give quantities when that is cheap: counts of bytes, elapsed milliseconds, status codes.
- Never log secrets: credentials, tokens, cookies, key material, or the whole body of a
  request or a response. Log a redaction, a size or a short preview instead.

Messages are in lower case, terse, in the present tense, and have no trailing period. They
substitute the relevant context:

```scala
def describe(remote: Text, path: Text, url: Text, size: Long, elapsed: Long): List[Message] = List
  ( m"connected to $remote",
    m"opened $path for reading",
    m"downloaded $url ($size bytes in ${elapsed}ms)" )
```

A message which says only that something happened, such as `m"a failure occurred"`, is not
enough. Name what failed, and include its identifier.

### Related

- [Errors](errors.md#defining-an-error) describes the messages which events are written as.
- [Terminal](terminal.md#styled-text) describes the styled text which a terminal logger
  writes.
- [Concurrency](concurrency.md) describes `supervise`, which a logger runs under.
- [Stack traces](stack-traces.md) presents the trace of an error which is logged.
- [HTTP server](http-server.md), [service discovery](service-discovery.md) and [metaprogramming](metaprogramming.md) log their events through the sinks in scope.
