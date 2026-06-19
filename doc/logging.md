# Soundness Logging Standards

This document defines when, what, and how to log in the Soundness libraries. It
complements `syntax.md` (whitespace and formatting) and `naming.md` (factory
names). Examples are drawn from `lib/*/src/core/*.scala`.

Logging is type-safe: a method that logs declares the requirement in its return
type with the `logs` infix type, and emits values of a module-specific `Event`
type — never raw strings. A `Logger` (a `Sink`) defined in the caller's scope
receives the events; with no logger in scope, logging is silent and free, so a
`logs` declaration imposes nothing on callers.

## 1. When to log

A method should log if, and only if, it performs an **observable side effect with
latency or a failure mode**:

- **Network** — connect, bind, listen, accept, send, receive, close, upgrade.
- **Filesystem** — open, read, write, create, delete, move, symlink, watch.
- **Process** — spawn, kill, wait, signal.
- **External services** — HTTP requests, daemon protocols, Docker, package
  registries, downloads and uploads.

Pure, CPU-bound, or in-memory work **must not** log: hashing, encryption, parsing,
codecs, serialization, arithmetic, and data-structure manipulation produce no
log output, however expensive.

A method opts in by naming its event type in the return type:

```scala
def fetch(url: HttpUrl): HttpResponse logs HttpEvent = ...
```

A method may log more than one event type by chaining:

```scala
def stop(server: Server): Unit logs HttpEvent logs ExecEvent = ...
```

Do **not** add a `logs` requirement (or a `Log` call) inside a method that
implements a typeclass's abstract method — the extra context parameter changes
the signature and breaks the override. Log in the public extension method that
*calls* the typeclass instead; that is also the right granularity (one entry per
user-level operation, not per internal dispatch).

## 2. Levels

`Level` has four values, chosen by **frequency and severity** together:

- **`Fine`** — high-frequency, per-unit detail useful only while debugging: each
  frame, packet, file open, request received, cache hit. Assume it is normally
  switched off; be verbose. A per-packet send is `Fine` even though it is
  "normal".
- **`Info`** — one line per significant logical operation or lifecycle milestone:
  a connection established, a server now listening, a request sent, a process
  started, an archive opened, a download finished. A one-per-connection bind is
  `Info`.
- **`Warn`** — a recoverable or unexpected-but-handled condition: a forced kill, a
  connection reset or retry, a broken stream, an idle timeout, a gracefully
  handled protocol violation, a fallback taken.
- **`Fail`** — an operation that failed unrecoverably; emitted alongside the
  `Error` being raised. Used sparingly.

## 3. What to log

- **Identify the resource**: address and port, path, PID, URL, stream id, archive
  or entry name, container id.
- **Quantify when cheap**: byte counts, entry counts, elapsed milliseconds,
  status codes, exit codes.
- **Never log secrets**: credentials, tokens, cookies, key material, authorization
  headers, or whole request and response bodies. Log a redaction, a size, or a
  short preview instead.
- **Bound the size**: log lengths and previews, not entire payloads.
- **Carry typed fields**, not pre-formatted strings. The event case holds the
  values; the `Communicable` instance does the formatting (§4). This keeps the
  event type-safe and lets the rendering vary by sink.

### Cost when disabled

`Log.fine`/`info`/`warn`/`fail` take their argument **by name**, and
`Loggable.fanOut` consults each sink's `accepts(level)` *before* forcing it. So
when no sink is in scope, or none accepts the level, the event value is never
constructed and its fields are never evaluated — a disabled log statement costs
nothing. Write log calls freely; do not guard them.

Because of this, prefer passing the **original typed values** into the event and
letting the `Communicable` render them, rather than converting at the call site:

```scala
Log.info(SendEvent(method, url, headers))   // typed fields; rendering deferred
```

Both forms are free when disabled (the argument is by-name), but typed fields
keep the event type-safe and let the rendering vary by sink.

**Exception — call-site context.** When rendering a value needs information only
available at the call site (for example, a `serpentine.Path on plane` whose
textual form depends on its `plane`), capture the rendered `Text` *there*:

```scala
Log.info(IoEvent.Move(source.show, destination.show))   // path rendering needs `plane`
```

This is correct, not a compromise: the `.show` runs at the (by-name) call site
where the context exists, and still only when the event is actually logged.

## 4. Event types

Each module that logs defines a single `enum` named `<Domain>Event` in its own
file `<module>.<Event>.scala`, with the companion `object` before the `enum`
(SN-398). Cases are CamelCase verb or noun phrases carrying the relevant values
as typed fields:

```scala
object ExecEvent:
  given communicable: ExecEvent is Communicable =
    case AbortProcess(pid)       => m"the process with PID $pid was aborted"
    case PipelineStart(commands) => m"started pipeline ${commands.map(_.show).join(t" ")}"
    case KillProcess(pid)        => m"killed process with PID $pid"
    case ProcessStart(command)   => m"starting process $command"

enum ExecEvent:
  case ProcessStart(command: Command)
  case AbortProcess(pid: Pid)
  case PipelineStart(commands: Seq[Command])
  case KillProcess(pid: Pid)
```

The companion holds a `given communicable: <Event> is Communicable` that maps each
case to an `m"..."` message, with the `=>` arrows aligned (syntax.md §5.1).

The event type must be exported into the `soundness` package (SN-742).

### Composing with `ExecEvent`

A module that shells out to a subprocess reuses `guillotine.ExecEvent` by
transcribing it into its own event, rather than duplicating process cases:

```scala
given execEvent: GitEvent transcribes ExecEvent = GitEvent.Exec(_)
```

(existing pattern: `GitEvent`, `CliEvent`, `IoEvent`).

## 5. Message style

Log messages are lowercase, terse, present-tense, with no trailing period, and
interpolate the relevant context — matching the house style for `fulminate`
error messages:

```scala
m"connected to $remote"
m"opened $path for reading"
m"killed process $pid"
m"downloaded $url ($size bytes in ${elapsed}ms)"
```

A message must say something specific: `m"a failure occurred"` is not acceptable —
name what failed and include the identifier.

## 6. Emitting

Call `Log.fine`, `Log.info`, `Log.warn`, or `Log.fail` with an event value at the
point the side effect happens:

```scala
Log.info(ExecEvent.ProcessStart(this))
val process = processBuilder.start()
```

```scala
Log.warn(ExecEvent.KillProcess(pid))
process.destroyForcibly()
```

A lifecycle event is logged once, at `Info`; a per-unit event (per frame, per
file) is logged at `Fine`; a handled anomaly is logged at `Warn`.
