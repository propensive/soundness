### Running the server

An `LspServer` is runnable on its own: defining the server is enough to run it. Every `LspServer` carries its
own `main` method, so a server object is a valid application entry point with no additional ceremony—there is no
need to write a `@main` method, and no need to wire up `cli`, `execute` and `supervise` by hand.

A minimal server needs only a `name` and its `capabilities`, plus whichever handler hooks it chooses to
override:
```scala
object MyServer extends LspServer():
  def name: Text = t"my-server"

  def capabilities: Lsp.ServerCapabilities =
    Lsp.ServerCapabilities(hoverProvider = true)

  override def hover(uri: Text, position: Lsp.Position): Optional[Lsp.Hover] =
    Lsp.Hover(Lsp.MarkupContent(value = t"Hello from my server."))
```

That object can be launched directly: its inherited `main` reads Language Server Protocol messages from standard
input and writes responses to standard output, exactly as an editor expects.

#### Fast startup as a daemon

The inherited `main` runs the server through [Ethereal](https://github.com/propensive/ethereal), so it starts
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
dependencies are fetched and cached on demand.

To opt in, add a `burdock` dependency to the server's module and override `main` to wrap the inherited entry
point in `externalize`:
```scala
object MyServer extends LspServer():
  def name: Text = t"my-server"
  def capabilities: Lsp.ServerCapabilities = Lsp.ServerCapabilities(hoverProvider = true)

  override def main(args: IArray[Text]): Unit = externalize(super.main(args))
```

`externalize` runs at compile time in the server's own module: it hashes each dependency JAR on the classpath,
caches it locally by hash, and records the hash list in the compiled artifact. At runtime it simply runs the
server. When the artifact is later repackaged, each dependency that resolves to a published location is
externalized (referenced by URL and hash) rather than inlined, yielding a thin launcher instead of a fat JAR.

#### Demo

The `exegesis.demo` module contains `DemoLspServer`, a small example server demonstrating a fixed hover message,
a fixed completion list and a diagnostic published when a document is opened. It is an ordinary
`object DemoLspServer extends LspServer()`, so it too carries its own `main` and needs no separate entry point.
