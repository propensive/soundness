## Daemons

### About

A JVM command-line tool pays the JVM's startup cost — and loses the just-in-time compiler's
accumulated optimisation — on every invocation, which makes even a fast program feel slow at the
shell. Soundness removes that cost by making the application resident: the first invocation starts
a daemon, and every later one is dispatched to the running process by a small native launcher,
with its arguments, environment, working directory, standard streams and signals all forwarded
faithfully.

The transformation costs one word: the body of a [command-line application](cli.md) wrapped in
`cli` rather than `application` becomes a daemon. Packaging produces a single self-contained
executable — the native launcher with the application inside — and the launcher can verify and
apply signed upgrades of itself.

### On daemonized applications

The idea is old — Nailgun kept a JVM warm for exactly this reason — but the details decide whether
it is trustworthy. Each invocation must behave *exactly* as a fresh process would: its own
environment and working directory, its own stdin and exit code, Ctrl-C reaching the right
invocation and not the daemon. And the daemon must manage itself: starting on demand, shutting
down when idle, surviving upgrades.

Soundness handles those details in a per-platform native launcher and a protocol over a Unix
domain socket, so the Scala application simply runs — many invocations concurrently, each with its
own faithful context. Everything comes from the `soundness` package, alongside the CLI machinery:

```scala
import soundness.*
import executives.completions
import interpreters.posixInterpreter
import backstops.stackTraceBackstop
import threading.virtualThreading
```

### A daemon application

`cli` is the daemonized counterpart of `application` — the same body, the same
[completions](cli.md) structure, resident execution:

```scala
@main
def mytool(): Unit = cli:
  execute:
    Out.println(t"Hello world")
    Exit.Ok
```

The first run starts the daemon; later runs connect to it and return at native-tool speed. Tab
completions gain the most: each completion request is an invocation, and a resident process
answers in milliseconds.

### Signals and shutdown

An invocation traps the signals it cares about, and the response reaches the code of that
invocation, not the shared process:

```scala
execute:
  trap:
    case signal: UnixSignal => SignalResponse.Accept
  longRunningWork()
```

The daemon retires itself after six idle hours, when its state files are removed, or on demand —
the built-in `'{admin}'` subcommand reports the daemon's pid and kills it.

### The service bus

Concurrent invocations of one daemon share a typed *bus*: an invocation broadcasts a message and
others observe the stream, which is how "the running watch command notices that another invocation
just changed the configuration" is expressed:

```scala
service.broadcast(ConfigChanged)
service.bus   // a Stream of messages from other invocations
```

### Packaging

Running the application's JAR with `-Dbuild.executable` assembles the distributable: the platform's
native launcher stub with the application embedded, as one executable file:

```sh
java -Dbuild.executable=mytool -jar mytool.jar
```

The launcher finds or fetches a suitable JVM, starts the daemon when none is running, and — where a
public key was built in — accepts only signed binaries when the application
[upgrades itself](https://en.wikipedia.org/wiki/Digital_signature) in place.
