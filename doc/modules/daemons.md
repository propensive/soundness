## Daemons

### About

A JVM command-line tool pays the JVM's startup cost — and loses the just-in-time compiler's
accumulated optimization — on every invocation, which makes even a fast program feel slow at the
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

A daemon that serves each invocation against that invocation's own environment is [declarative context](../philosophy/declarative-context.md) taken seriously: nothing about the caller is global.

Soundness handles those details in a per-platform native launcher and a protocol over a Unix
domain socket, so the Scala application simply runs — many invocations concurrently, each with its
own faithful context. The protocol is a TEL schema, `ethereal-launcher` (its text is
`Launcher.schemaText`): every connection the launcher opens begins with one BinTEL document of
that schema — an invocation with its arguments and environment, a signal, an exit-status
request — and the daemon answers in kind, so both sides check the schema's signature before
reading a field, and a launcher and daemon built against different contracts refuse each other
rather than misread each other. Everything comes from the `soundness` package, alongside the CLI
machinery:

```scala
import soundness.*

import backstops.stackTraceBackstop
import executives.completionsExecutive
import interpreters.posixInterpreter
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

`cli` takes no arguments: the launcher forwards them, with the environment, working directory
and standard streams of the invoking shell, and the body sees them through the same `arguments`,
`Out` and `Exit` as an ordinary application.

The first run starts the daemon; later runs connect to it and return at native-tool speed. Tab
completions gain the most: each completion request is an invocation, and a resident process
answers in milliseconds.

Output must go through `Out` and `Err`. Scala's own `println` writes to the JVM's global
`System.out`, which belongs to the *daemon* process rather than to any client — so whatever it
prints reaches no user and, in the ordinary case, is simply lost. `Out` and `Err` resolve the
`Stdio` of the current invocation, which is the client's. When the reader of that stream goes
away — `mytool | head -1`, once `head` has exited — the launcher says so, and the invocation's
next write to it raises `Outlet.Error`, ending the invocation with status 141 as `SIGPIPE`
would end a process writing a pipe.

Arguments and environment values arrive as text. A byte sequence that is not valid UTF-8 in
either is delivered with U+FFFD in its place, which is the substitution the JVM itself makes
for its own argument vector, so an application sees what it would have seen if run directly.

### What the invocation knows about its client

The daemon holds a socket, not the client's process, so everything it knows about the
invocation is what the launcher told it, and the invocation's `resident` (a `Resident`, its
handle on the daemon) is where that knowledge is read.
`resident.cliInput`, `cliOutput` and `cliError` say whether each standard stream is a terminal.
`resident.invokedAs` is the name the executable was invoked by — `argv[0]` as the caller
supplied it — so one executable installed under several names by symbolic links can dispatch
on the name, while every alias shares one warm daemon:

```scala
def multicall(): Unit = cli:
  execute:
    resident.invokedAs.let(_.cut(t"/").last) match
      case t"gunzip" => Out.println(t"decompressing")
      case _         => Out.println(t"compressing")
    Exit.Ok
```

`resident.umask` is the invocation's file-creation mask, which the [filesystem](filesystem.md)
library applies to whatever the invocation creates; and `resident.windowSize` is the client
terminal's size, measured by the launcher when the invocation began and again on every resize.
The invocation's `Termcap` reads the same measurement, so tables and wrapped text fit the
terminal as it is now.

### Signals and shutdown

An invocation traps the signals it cares about, and the response reaches the code of that
invocation, not the shared process. A trap sees a `Signal`: the interrupt itself, and whatever
the launcher attached to it — the terminal's new size with `WINCH` and `CONT`, and on Windows
the time the system allows after a close, logoff or shutdown event before it ends the client
regardless:

```scala
def longRunningWork(): Exit = Exit.Ok

def watch(): Unit = cli:
  execute:
    trap:
      case Signal(Interrupt.Winch, columns, rows, _) =>
        Out.println(t"now ${columns.or(0)}×${rows.or(0)}")
        SignalResponse.Accept

      case Signal(WindowsSignal.Close, _, _, deadline) =>
        SignalResponse.Accept

      case Signal(_: UnixSignal, _, _, _) =>
        SignalResponse.Accept

    longRunningWork()
```

The daemon retires itself after six idle hours, when its state files are removed, or on demand.
The built-in `'{admin}'` subcommand reports the daemon's pid (`pid`), ends it at once (`kill`),
or asks it to go gracefully (`shutdown`): no further invocation is served, those in flight
finish, and then the process exits, so a warm JVM held for a command run rarely can be
reclaimed without finding its pid. A rebuilt executable displaces its daemon the same way.

### Asking for a cooked terminal

The launcher puts the terminal into raw mode before it connects, so that keypresses can be
forwarded to an interactive session. That is wrong for a command that just wants a line of input:
without the terminal driver's help there is no echo, and Backspace arrives as a literal byte
inside the line. `cooked` asks the launcher for canonical mode for the duration of a block, and
raw mode is restored afterwards:

```scala
def ask(): Unit = cli:
  execute:
    val name = resident.cooked:
      Out.println(t"Name?")
      In.read[Text]
    Out.println(t"Hello, $name")
    Exit.Ok
```

Echo and line editing then come from the terminal driver itself. A launcher with no such channel
— a pipe, or an older stub — leaves the request to expire harmlessly.

### The message bus

Concurrent invocations of one daemon share a typed *bus*: an invocation broadcasts a message and
others observe the stream, which is how "the running watch command notices that another invocation
just changed the configuration" is expressed. The message type is the type argument to `cli`, so
every invocation of the daemon agrees on what can be sent:

```scala
enum Message:
  case ConfigChanged

def configure(): Unit = cli[Message]:
  execute:
    resident.broadcast(Message.ConfigChanged)
    resident.bus.each:
      case Message.ConfigChanged => Out.println(t"another invocation changed the configuration")
    Exit.Ok
```

### Packaging

The distributable is assembled by the `xek` builder, published with the runner stubs from
[propensive/xek](https://github.com/propensive/xek), and installed with
`curl -fsSL https://propensive.dev/xek | sh`. It joins the platform's native launcher stub, a
small configuration record and the application JAR into one executable file:

```sh
xek build mytool.jar
```

That writes `mytool`, for the platform `xek` runs on. `xek build -p linux-x64 -p
windows-x64 mytool.jar` builds for other platforms, and `xek build --polyglot mytool.jar` builds one file for all
of them, which runs in `sh` (and in PowerShell and `cmd.exe` once renamed) and unpacks the right
launcher where it runs; `xek --help` lists the rest.

The launcher finds or fetches a suitable JVM, starts the daemon when none is running, and — where a
public key was built in — accepts only signed binaries when the application
[upgrades itself](https://en.wikipedia.org/wiki/Digital_signature) in place.

### Signing a release

An application can ship with a public key and an application identifier built in, which the
launcher uses to verify any candidate upgrade before swapping it into place. Verification
happens in the launcher, before the JVM starts, so no Scala code in the running application sits
on the trust boundary.

`xek` also signs releases (its signing subcommands need Java 24 or later). It generates a key
pair:

```sh
xek keygen --out release-keys/myapp
xek keygen --out release-keys/myapp-recovery
```

Each writes a 32-byte FIPS-204 signing-key seed, `<prefix>.seed`, and a 1312-byte ML-DSA-44
public key, `<prefix>.pub`. **The seeds belong offline**, the recovery seed most of all. Anyone
holding the release seed can ship a binary that users' launchers will accept; the recovery key
exists so that losing or leaking it does not strand every install.

Each release is then built and signed in two steps. The build bakes in a build identifier, both
public keys and the application's identifier:

```sh
xek build --build-id 42 --public-key release-keys/myapp.pub \
  --recovery-key release-keys/myapp-recovery.pub --app-id example/myapp \
  dist/myapp.jar dist/myapp
```

`--build-id` is a 64-bit number which must increase from release to release; the launcher
rejects a candidate that is not newer than itself. A candidate built for another application
identifier is rejected even when it is signed with the same key. Omitting `--public-key` and
`--app-id` produces a binary whose launcher rejects *every* upgrade — the right default for a
local build where the upgrade path is never exercised. Signing then produces the file to
distribute:

```sh
xek sign --key release-keys/myapp.seed --in dist/myapp --out dist/myapp.signed
```

That output is simultaneously a valid executable and a valid upgrade candidate. The application
stages one by passing `Upgrade.stage` any source of bytes — a URL, a file, a response body. It
writes the candidate as `.pending` in the application's data directory and returns; the launcher
verifies it at the start of the next invocation, swaps it into place, and starts a new daemon in
place of the old one, so nothing is interrupted:

```scala
import environments.javaBaseEnvironment
import systems.javaBaseSystem
import errorDiagnostics.stackTracesDiagnostics
import internetAccess.online

def upgrade(): Text raises Upgrade.Error =
  if !Upgrade.enabled then t"this build cannot be upgraded"
  else if Upgrade.pending then t"the upgrade will be applied the next time the command is run"
  else
    Upgrade.stage(url"https://releases.example.com/myapp.signed")
    t"the upgrade will be applied the next time the command is run"
```

`Upgrade.enabled` is false in a build with no key or no application identifier, whose launcher
would refuse any candidate, and `resident.buildId` is the running build's identifier, to compare
with that of a release on offer.

The launcher records what it did with a candidate, and `Upgrade.outcome` reads it back: the
`result` (`Applied`, or why the candidate was refused — `Disabled`, `NoRecord`,
`WrongApplication`, `BadSignature`, `NotNewer` or `SwapFailed`), the `candidate` and `previous`
build identifiers, and `when` it happened. `Upgrade.acknowledge()` clears it, so that each
outcome is reported once:

```scala
import environments.javaBaseEnvironment
import systems.javaBaseSystem

def report(): Optional[Message] =
  Upgrade.outcome.let: outcome =>
    Upgrade.acknowledge()
    m"${outcome.result} (build ${outcome.candidate.show})"
```

What the signature covers is chosen so that each part of it defeats a specific attack: the
launcher's own code, the bundled JAR, the build identifier (so an older legitimately-signed
release cannot be replayed), the application identifier (so another application's release cannot
be installed in its place), the flag byte permitting a downgrade (so it cannot be turned on after
the fact), and the keys themselves (so a different release key cannot be substituted into an
otherwise-legitimate binary). A deliberate rollback — shipping 42 over a broken 43 — is signed
with `--allow-downgrade`, which sets that flag inside the signed payload.

The keys a candidate is checked against are always those of the *running* binary, so rotation
works as a chain. A release signed with the old key may carry a new key as its `--public-key`,
after which releases are signed with the new one (`xek sign` asks for `--foreign-key` to sign
with a key the binary does not carry, since that is otherwise almost always a mistake). A release
signed with the recovery key is accepted too, and may replace either key.
