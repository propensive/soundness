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
`Stdio` of the current invocation, which is the client's.

### Signals and shutdown

An invocation traps the signals it cares about, and the response reaches the code of that
invocation, not the shared process:

```scala
def longRunningWork(): Exit = Exit.Ok

def watch(): Unit = cli:
  execute:
    trap:
      case signal: UnixSignal => SignalResponse.Accept
    longRunningWork()
```

The daemon retires itself after six idle hours, when its state files are removed, or on demand —
the built-in `'{admin}'` subcommand reports the daemon's pid and kills it.

### Asking for a cooked terminal

The launcher puts the terminal into raw mode before it connects, so that keypresses can be
forwarded to an interactive session. That is wrong for a command that just wants a line of input:
without the terminal driver's help there is no echo, and Backspace arrives as a literal byte
inside the line. `cooked` asks the launcher for canonical mode for the duration of a block, and
raw mode is restored afterwards:

```scala
def ask(): Unit = cli:
  execute:
    val name = service.cooked:
      Out.println(t"Name?")
      In.read[Text]
    Out.println(t"Hello, $name")
    Exit.Ok
```

Echo and line editing then come from the terminal driver itself. A launcher with no such channel
— a pipe, or an older stub — leaves the request to expire harmlessly.

### The service bus

Concurrent invocations of one daemon share a typed *bus*: an invocation broadcasts a message and
others observe the stream, which is how "the running watch command notices that another invocation
just changed the configuration" is expressed. The message type is the type argument to `cli`, so
every invocation of the daemon agrees on what can be sent:

```scala
enum Message:
  case ConfigChanged

def configure(): Unit = cli[Message]:
  execute:
    service.broadcast(Message.ConfigChanged)
    service.bus.each:
      case Message.ConfigChanged => Out.println(t"another invocation changed the configuration")
    Exit.Ok
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

### Signing a release

An application can ship with a public key baked in, which the launcher uses to verify any
candidate upgrade before swapping it into place. Verification happens in the launcher, before the
JVM starts, so no Scala code in the running application sits on the trust boundary.

The `ethereal-sign` tool generates the keypair, once:

```sh
ethereal-sign keygen --out release-keys/myapp
```

This writes a 32-byte FIPS-204 signing-key seed and a 1312-byte ML-DSA-44 public key. **The seed
belongs offline.** Anyone holding it can ship a binary that users' launchers will accept, and
there is no revocation at the launcher level: the public key is baked into every shipped binary,
and only a further release replaces it.

Each release is then built and signed in two steps. The build bakes in the public key and a build
identifier:

```sh
java -Dbuild.executable=dist/myapp \
     -Dbuild.id=42 \
     -Dethereal.publicKey=release-keys/myapp.pub \
     -jar dist/myapp.jar
```

`build.id` must increase monotonically; the verifier compares it against the running launcher's
own and rejects downgrades. Omitting `ethereal.publicKey` leaves the key slot zeroed, producing a
binary whose launcher rejects *every* upgrade — the right default for a local build where the
upgrade path is never exercised. Signing then produces the file to distribute:

```sh
ethereal-sign sign --key release-keys/myapp.seed --in dist/myapp --out dist/myapp.signed
```

That output is simultaneously a valid executable and a valid upgrade candidate. The application
applies one by pointing `Upgrade` at any source of bytes — a URL, a file, a response body — and
`Upgrade` does not return, because on success the running process is replaced:

```scala
import environments.javaBaseEnvironment
import systems.javaBaseSystem
import errorDiagnostics.stackTracesDiagnostics
import internetAccess.online

def upgrade(): Nothing = Upgrade(url"https://releases.example.com/myapp.signed")
```

The bytes are written aside, a fresh launcher starts and the old process exits; the new launcher
verifies the signature against its baked-in key, checks the build identifier, and either swaps the
binary into place or discards the candidate and carries on with the existing one.

What the signature covers is chosen so that each part of it defeats a specific attack: the
launcher's own code, the bundled JAR, the build identifier (so an older legitimately-signed
release cannot be replayed), the flag byte permitting a downgrade (so it cannot be turned on
after the fact), and the baked-in public key itself (so a different release key cannot be
substituted into an otherwise-legitimate binary). A deliberate rollback — shipping 42 over a
broken 43 — is signed with `--allow-downgrade`, which sets that flag inside the signed payload.

Rotating keys needs one bridging release: sign it with the **old** seed but bake in the **new**
public key, so existing installs accept it through the normal upgrade path and trust the new key
from then on. Skip that step and existing installs are stranded, holding a key that will reject
everything signed thereafter.
