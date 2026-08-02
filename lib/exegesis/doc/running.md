### Running the server

A server is one call to `Lsp.listen`, which registers the server's feature handlers and then serves Language
Server Protocol messages from standard input to standard output, exactly as an editor expects. The call runs
until its input is exhausted, so a server object wraps it in a small `main` using the resident-daemon idiom:

```scala
import soundness.*

import backstops.stackTraceBackstop
import executives.completions
import interpreters.posixInterpreter
import probates.awaitProbate
import strategies.throwUnsafely
import threading.virtualThreading

object MyServer:
  import Lsp.*

  def main(args: Array[Text]): Unit = cli:
    execute:
      supervise:
        Lsp.listen(t"my-server"):
          hover:
            Hover(MarkupContent(value = t"Hello from my server."))

      Exit.Ok
```

There is no capabilities record to write: registering `hover` is what advertises `hoverProvider` to the
editor, and so on for every feature.

#### Observing the traffic

`Lsp.listen` takes an optional `Lsp.Observer`, which sees every message crossing the transport — inbound before
it is parsed, outbound before it is framed — as the JSON body, without the `Content-Length` header. This is how
a server exposes a log of its own traffic, which it cannot simply print: standard output is reserved for the
wire protocol.

```scala
  val observer = new Lsp.Observer:
    def received(message: Text): Unit = log.put(t"recv $message")
    def sent(message: Text): Unit = log.put(t"send $message")

  Lsp.listen(t"my-server", t"1.0", observer):
    hover:
      Hover(MarkupContent(value = t"Hello from my server."))
```

A message that fails to parse is observed too, since the observer runs before decoding. Omitting the parameter
observes nothing, at no cost.

#### Fast startup as a daemon

The `cli` entry point runs the server through [Ethereal](https://github.com/propensive/ethereal), so it starts
as a resident daemon: the first invocation launches a background JVM, and subsequent invocations connect to the
already-running process. This avoids paying the JVM's startup cost—and losing the just-in-time compiler's
accumulated optimizations—on every launch, which matters for a language server that an editor may start and
restart frequently.

To build a native launcher for the server, run its JAR with the `build.executable` property set to the desired
filename:
```sh
java -Dbuild.executable=my-server -jar my-server.jar
```
Ethereal assembles a small native launcher that starts (or connects to) the daemon and forwards standard input,
standard output and signals to it.

#### Externalizing the classpath with Burdock

By default the launcher bundles every dependency into a single fat JAR. With
[Burdock](https://github.com/propensive/burdock), the server can instead be distributed as a thin launcher whose
dependencies are fetched and cached on demand. To opt in, add a `burdock` dependency to the server's module and
wrap the entry point's body in `externalize`.

`externalize` runs at compile time in the server's own module: it hashes each dependency JAR on the classpath,
caches it locally by hash, and records the hash list in the compiled artifact. At runtime it simply runs the
server. When the artifact is later repackaged, each dependency that resolves to a published location is
externalized (referenced by URL and hash) rather than inlined, yielding a thin launcher instead of a fat JAR.

#### Demo

The `exegesis.demo` module contains `DemoLspServer`, a small example server demonstrating a hover showing the
word under the cursor, a fixed completion list, a command, and a diagnostic published when a document is opened.
Its registrations are the complete definition of the server's behaviour — and of its advertised capabilities.
