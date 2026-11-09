## Daemons

### About

A command-line tool on the JVM pays the JVM's startup cost on every invocation, and loses
the optimization which the just-in-time compiler had built up. That makes even a fast
program feel slow at the shell.

Making the application resident removes the cost. The first invocation starts a daemon.
Every later one is dispatched to the running process by a small native launcher, with its
arguments, environment, working directory, standard streams and signals all forwarded
faithfully.

The change costs one word. The body of a
[command-line application](cli.md) which is wrapped in `cli`, instead of in `application`,
becomes a daemon. Packaging produces a single self-contained executable, which is the native
launcher with the application inside, and the launcher can verify and apply signed upgrades
of itself.

### On daemonized applications

The idea is old. Nailgun kept a JVM warm for exactly this reason. But the details decide
whether it can be trusted. Each invocation must behave *exactly* as a fresh process would:
with its own environment and working directory, its own standard input and exit code, and
with Ctrl-C reaching the right invocation, and not the daemon. And the daemon must manage
itself: starting on demand, shutting down when it is idle, and surviving upgrades.

Here those details are handled in a native launcher for each platform, and in a protocol
over a Unix domain [socket](sockets.md). The Scala application simply runs, with many
invocations at once, each with its own faithful context. A daemon which serves each
invocation against that invocation's own environment is
[declarative context](../philosophy/declarative-context.md) taken seriously: nothing about
the caller is global.

The protocol is a [TEL](tel.md#bintel) schema, `ethereal-launcher`, whose text is
`Launcher.schemaText`. Every connection which the launcher opens begins with one BinTEL
document of that schema: an invocation with its arguments and environment, a signal, or a
request for an exit status. The daemon answers in kind. Both sides check the schema's
signature before reading a field, so a launcher and a daemon which were built against
different contracts refuse each other, instead of misreading each other.

Everything comes from the `soundness` package, beside the imports which a command-line
application needs:

```scala
import soundness.*

import backstops.stackTraceBackstop
import executives.completionsExecutive
import interpreters.posixInterpreter
import threads.virtualThreads
```

The samples on this page define whole programs, so they are compiled but not run.

### A daemon application

`cli` is the daemonized counterpart of `application`. It has the same body, and the same
structure for [completions](cli.md#tab-completions), and runs resident:

```scala
@main
def mytool(): Unit = cli:
  execute:
    Out.println(t"Hello world")
    Exit.Ok
```

`cli` takes no arguments. The launcher forwards them, with the environment, working
directory and standard streams of the shell which invoked it, and the body sees them through
the same `arguments`, `Out` and `Exit` as an ordinary application.

The first run starts the daemon. Later runs connect to it, and return at the speed of a
native tool. Tab completions gain the most: each request for completions is an invocation,
and a resident process answers in milliseconds.

#### Output

Output must go through `Out` and `Err`. Scala's own `println` writes to the JVM's global
`System.out`, which belongs to the *daemon* process, not to any client, so whatever it
prints reaches no user, and is ordinarily lost. `Out` and `Err` resolve the
[`Stdio`](streams.md#standard-streams) of the current invocation, which is the client's.

When the reader of that stream goes away, as in `mytool | head -1` once `head` has exited,
the launcher says so. The invocation's next write to the stream raises an `Egress.Error`,
which ends the invocation with status 141, as `SIGPIPE` would end a process writing to a
pipe.

Code which cannot be changed, such as a library which prints to `System.out`, is redirected
with `Stdio.divert`. The diversion belongs to a *thread*, not to the JVM. It follows the
thread on which it is made, the threads which are started under it, and every task which is
spawned within it, so a daemon may divert around several clients at once, without their
output crossing.

Arguments and the values of environment variables arrive as text. A sequence of bytes in
either which is not valid UTF-8 is delivered with U+FFFD in its place. That is the
substitution which the JVM itself makes for its own arguments, so an application sees what it
would have seen if it were run directly.

### What the invocation knows about its client

The daemon holds a socket, not the client's process, so everything which it knows about the
invocation is what the launcher told it. The invocation's `resident`, a `Resident`, is its
handle on the daemon, and is where that knowledge is read:

```tabular
column  Member
column  Is
row  `resident.cliInput`, `cliOutput`, `cliError`  whether each standard stream is a `Terminus.Terminal` or a `Terminus.Pipe`
row  `resident.invokedAs`  the name the executable was invoked by: `argv[0]` as the caller supplied it
row  `resident.umask`  the invocation's file-creation mask
row  `resident.windowSize`  the client terminal's size, measured when the invocation began and on every resize
row  `resident.buildId`  the running build's identifier
```

One executable which is installed under several names, by symbolic links, can therefore
dispatch on `invokedAs`, while every alias shares one warm daemon:

```scala
import dysasymptotics.linearSize

def multicall(): Unit = cli:
  execute:
    resident.invokedAs.let(_.cut(t"/").last) match
      case t"gunzip" => Out.println(t"decompressing")
      case _         => Out.println(t"compressing")
    Exit.Ok
```

The [filesystem](filesystem.md#creation-masks) applies `umask` to whatever the invocation
creates. The invocation's `Termcap` reads the same measurement as `windowSize`, so
[tables](tables.md) and wrapped text fit the terminal as it is now.

### Signals and shutdown

An invocation traps the signals which it cares about, and the response reaches the code of
that invocation, not the shared process. A trap sees a `Signal`: the interrupt itself, and
whatever the launcher attached to it. That is the terminal's new size, with `WINCH` and
`CONT`. On Windows, it is the time which the system allows after an event of closing, logging
off or shutting down, before it ends the client regardless:

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

The daemon retires itself after six idle hours, or when its state files are removed, or on
demand, through the built-in `'{admin}'` subcommand:

```tabular
column  Subcommand
column  Effect
row  `pid`  reports the daemon's pid
row  `kill`  ends the daemon at once
row  `shutdown`  serves no further invocation, lets those in flight finish, then exits
```

A warm JVM which is held for a command that is rarely run can therefore be reclaimed,
without finding its pid. An executable which has been rebuilt displaces its daemon in the
same way as `shutdown`.

### Asking for a cooked terminal

The launcher puts the terminal into raw mode before it connects, so that keypresses can be
forwarded to an interactive [terminal session](terminal.md#keyboard-input). That is wrong
for a command which just wants a line of input. Without the terminal driver's help there is
no echo, and Backspace arrives as a literal byte inside the line. `cooked` asks the launcher
for canonical mode for the duration of a block, and raw mode is restored afterwards:

```scala
def ask(): Unit = cli:
  execute:
    val name = resident.cooked:
      Out.println(t"Name?")
      java.io.BufferedReader(java.io.InputStreamReader(summon[Stdio].in)).readLine().nn.tt
    Out.println(t"Hello, $name")
    Exit.Ok
```

Echo and line editing then come from the terminal driver itself. A launcher with no such
channel, such as a pipe or an older launcher, lets the request expire harmlessly.

### The message bus

Concurrent invocations of one daemon share a typed *bus*. An invocation broadcasts a
message, and others observe the stream. That is how "the running watch command notices that
another invocation just changed the configuration" is expressed. The type of message is the
type argument to `cli`, so every invocation of the daemon agrees on what can be sent:

```scala
enum Notice:
  case ConfigChanged

def configure(): Unit = cli[Notice]:
  execute:
    resident.broadcast(Notice.ConfigChanged)
    resident.bus.each:
      case Notice.ConfigChanged => Out.println(t"another invocation changed the configuration")
    Exit.Ok
```

### Packaging

The distributable is assembled by the builder `xek`. It is published, with the launchers,
from [propensive/xek](https://github.com/propensive/xek), and is installed with
`curl -fsSL https://propensive.dev/xek | sh`. It joins the platform's native launcher, a
small configuration record and the application's JAR into one executable file:

```sh
xek build mytool.jar
```

That writes `mytool`, for the platform on which `xek` runs.
`xek build -p linux-x64 -p windows-x64 mytool.jar` builds for other platforms.
`xek build --polyglot mytool.jar` builds one file for all of them, which runs in `sh`, and
in PowerShell and `cmd.exe` once it is renamed, and unpacks the right launcher where it
runs. The flags which this page uses are:

```tabular
column  Command
column  Flag
column  Effect
row  `xek build`  `-p` *platform*  builds for that platform; repeatable
row  `xek build`  `--polyglot`  builds one file for every platform
row  `xek build`  `--build-id` *n*  bakes in the build identifier, which must increase from release to release
row  `xek build`  `--public-key` *file*  bakes in the release key
row  `xek build`  `--recovery-key` *file*  bakes in the recovery key
row  `xek build`  `--app-id` *id*  bakes in the application identifier
row  `xek keygen`  `--out` *prefix*  writes `<prefix>.seed` and `<prefix>.pub`
row  `xek sign`  `--key` *seed*  signs with that key
row  `xek sign`  `--in` *file*, `--out` *file*  names the binary to sign and the signed result
row  `xek sign`  `--allow-downgrade`  permits the candidate to replace a newer build
row  `xek sign`  `--foreign-key`  permits signing with a key the binary does not carry
```

`xek --help` lists the rest.

The launcher finds or fetches a suitable JVM, and starts the daemon when none is running.
Where a public key was built in, it accepts only signed binaries when the application
[upgrades itself](https://en.wikipedia.org/wiki/Digital_signature) in place.
[Packaging](packaging.md) covers the other forms of distributable.

### Signing a release

An application can ship with a public key and an application identifier built in. The
launcher uses them to verify any candidate upgrade, before swapping it into place.
Verification happens in the launcher, before the JVM starts, so no Scala code in the running
application sits on the boundary of trust.

`xek` also signs releases. Its signing subcommands need Java 24 or later. It generates a
pair of keys:

```sh
xek keygen --out release-keys/myapp
xek keygen --out release-keys/myapp-recovery
```

Each writes a seed for a signing key, of 32 bytes, as `<prefix>.seed`, and an ML-DSA-44
public key, of 1312 bytes, as `<prefix>.pub`. **The seeds belong offline**, and the recovery
seed most of all. Anyone who holds the release seed can ship a binary which users' launchers
will accept. The recovery key exists so that losing or leaking the release seed does not
strand every installation. [Cryptography](cryptography.md#signing) describes ML-DSA
signatures.

Each release is then built and signed, in two steps. The build bakes in a build identifier,
both public keys, and the application's identifier:

```sh
xek build --build-id 42 --public-key release-keys/myapp.pub \
  --recovery-key release-keys/myapp-recovery.pub --app-id example/myapp \
  dist/myapp.jar dist/myapp
```

`--build-id` is a 64-bit number which must increase from release to release. The launcher
rejects a candidate which is not newer than itself. A candidate which was built for another
application identifier is rejected, even when it is signed with the same key. Omitting
`--public-key` and `--app-id` produces a binary whose launcher rejects *every* upgrade,
which is the right default for a local build, where the upgrade path is never exercised.

Signing then produces the file to distribute:

```sh
xek sign --key release-keys/myapp.seed --in dist/myapp --out dist/myapp.signed
```

That output is at once a valid executable and a valid candidate for upgrade.

#### Staging an upgrade

The application stages a candidate by passing `Upgrade.stage` any source of bytes: a file,
an [HTTP response](http-client.md), a [stream](streams.md). It writes the candidate as
`<name>/.pending`, in the application's data directory, and returns. The launcher verifies
it at the start of the next invocation, swaps it into place, and starts a new daemon in
place of the old one, so nothing is interrupted:

```scala
import environments.javaBaseEnvironment
import systems.javaBaseSystem
import errorDiagnostics.stackTracesDiagnostics
import internetAccess.online
import httpBackends.javaNetHttp
import strategies.throwUnsafely

def upgrade(): Text raises Upgrade.Error =
  if !Upgrade.enabled then t"this build cannot be upgraded"
  else if Upgrade.pending then t"the upgrade will be applied the next time the command is run"
  else
    Upgrade.stage(url"https://releases.example.com/myapp.signed".fetch())
    t"the upgrade will be applied the next time the command is run"
```

`Upgrade.enabled` is false in a build with no key, or no application identifier, whose
launcher would refuse any candidate. `resident.buildId` is the identifier of the running
build, to compare with that of a release on offer.

The launcher records what it did with a candidate, and `Upgrade.outcome` reads the record
back: the `result`, the build identifiers `candidate` and `previous`, and `when` it
happened. The result is one of:

```tabular
column  Result
column  Meaning
row  `Applied`  the upgrade was applied
row  `Disabled`  the running executable does not accept upgrades
row  `NoRecord`  the candidate is not an XEK executable
row  `WrongApplication`  the candidate was built for another application
row  `BadSignature`  the candidate's signature does not verify
row  `NotNewer`  the candidate is not newer than the running executable
row  `SwapFailed`  the candidate could not be swapped into place
```

`Upgrade.acknowledge()` clears the record, so that each outcome is reported once:

```scala
import environments.javaBaseEnvironment
import systems.javaBaseSystem

def report(): Optional[Message] =
  Upgrade.outcome.let: outcome =>
    Upgrade.acknowledge()
    m"${outcome.result} (build ${outcome.candidate.show})"
```

#### What the signature covers

What the signature covers is chosen so that each part of it defeats a specific attack:

```tabular
column  Signed part
column  Prevents
row  the launcher's own code  tampering with the launcher
row  the bundled JAR  tampering with the application
row  the build identifier  replaying an older, legitimately-signed release
row  the application identifier  installing another application's release in its place
row  the flag byte permitting a downgrade  turning it on after the fact
row  the keys themselves  substituting a different release key into an otherwise-legitimate binary
```

A deliberate rollback, such as shipping 42 over a broken 43, is signed with
`--allow-downgrade`, which sets that flag inside the signed payload.

The keys which a candidate is checked against are always those of the *running* binary, so
rotation of keys works as a chain. A release which is signed with the old key may carry a
new key as its `--public-key`, after which releases are signed with the new one. `xek sign`
asks for `--foreign-key` to sign with a key which the binary does not carry, since that is
otherwise almost always a mistake. A release which is signed with the recovery key is
accepted too, and may replace either key.

### Related

- [Command-line applications](cli.md) describes the body which `cli` runs.
- [Environment](environment.md#overriding-for-a-block) explains how each invocation sees its
  own environment.
- [Concurrency](concurrency.md) runs the invocations side by side.
- [Packaging](packaging.md) builds other kinds of distributable.
