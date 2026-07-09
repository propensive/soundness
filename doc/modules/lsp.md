## Language Server Protocol

### About

A [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) server
is a class extending `LspServer`: it declares which features it provides, overrides a
handler for each one, and serves over standard input and output. The protocol underneath —
JSON-RPC messages, their length-prefixed framing, and the bookkeeping of open documents —
is handled, so the code that remains is the language logic.

The protocol's vocabulary is modelled as ordinary types. A `Position` is a line and a
character, a `Range` is two positions, a `Diagnostic` is a range with a severity and a
message; a hover response, a completion list, a set of document symbols each have their
type. A handler receives these as typed values and returns them as typed values, never as
hand-assembled JSON.

### On the protocol

An editor that understands LSP can use any language's server, and a language that provides
a server works in any such editor. That leverage is why the protocol exists, and it is
also why the protocol is large: dozens of request and notification types, each with its
own JSON shape, exchanged over a framed stream. Implementing it by hand means marshalling
JSON and tracking document versions, work that has nothing to do with the language being
served.

Soundness does that work once. Each message type is a Scala type with a derived JSON codec,
each request is dispatched to a method whose parameters and result are those types, and the
open documents are tracked for the server. What is left to write is the part that is
specific to a language — what a hover shows, what completions to offer, which diagnostics to
report. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Defining a server

A server extends `LspServer` and gives its name, an optional version, and the capabilities
it advertises to the editor. The capabilities declare which features the server actually
implements, so an editor asks only for what the server can answer:

```scala
object DemoServer extends LspServer():
  def name: Text = t"Demo"
  override def version: Optional[Text] = t"0.1.0"

  def capabilities: Lsp.ServerCapabilities =
    Lsp.ServerCapabilities
      ( textDocumentSync   = Lsp.TextDocumentSyncKind.Full,
        hoverProvider      = true,
        completionProvider = Lsp.CompletionOptions() )
```

### Responding to requests

Each language feature is a method on `LspServer` with a default that does nothing;
overriding one provides that feature. A hover handler receives the document's URI and a
position, and returns the content to show — or `Unset` for nowhere worth hovering:

```scala
  override def hover(uri: Text, position: Lsp.Position): Optional[Lsp.Hover] =
    Lsp.Hover(Lsp.MarkupContent(value = t"the symbol under the cursor"))

  override def complete(uri: Text, position: Lsp.Position): Lsp.CompletionList =
    Lsp.CompletionList
      ( items = List
          ( Lsp.CompletionItem(label = t"alpha", kind = Lsp.CompletionItemKind.Keyword),
            Lsp.CompletionItem(label = t"beta",  kind = Lsp.CompletionItemKind.Keyword) ) )
```

Handlers for going to a definition, finding references, listing a document's symbols,
formatting, renaming, code actions and signature help follow the same shape: typed
parameters in, a typed result out.

### Documents and diagnostics

The server keeps each open document as the editor reports it, through notifications the
server can observe by overriding `onOpen`, `onChange`, `onSave` and `onClose`. These
carry an `LspClient`, the channel back to the editor, which is how the server pushes
diagnostics — errors and warnings — for a document rather than waiting to be asked:

```scala
  override def onOpen(document: Lsp.TextDocumentItem)(using LspClient): Unit =
    summon[LspClient].publishDiagnostics
      ( document.uri,
        List
          ( Lsp.Diagnostic
              ( range    = Lsp.Range(Lsp.Position(0, 0), Lsp.Position(0, 1)),
                severity = Lsp.DiagnosticSeverity.Warning,
                message  = t"a diagnostic for the first character" ) ) )
```

`LspClient` also carries `showMessage` and `logMessage`, for notices shown to the user and
lines written to the editor's log.

### Running the server

The server object is a complete application: defining it is enough to run it. Every
`LspServer` carries its own `main`, so there is no entry point to write — no `@main`
method, and no command-line plumbing to wire up by hand. The `DemoServer` defined above is
already runnable; launching it runs the inherited `main`, which reads Language Server
Protocol messages from standard input and writes responses to standard output, exactly as
an editor expects. An editor configured to launch the object as its language server
exchanges JSON-RPC with it over the pipe, and each request arrives at the matching handler.

### Fast startup

The inherited `main` runs the server as a resident [daemon](daemons.md): the first
invocation launches a background JVM, and every later one connects to the process already
running. An editor starts and restarts a language server often, so avoiding the JVM's
startup cost — and keeping the just-in-time compiler's accumulated optimizations — on each
launch matters. Running the server's JAR with the `build.executable` property set assembles
a small native launcher that starts, or connects to, the daemon and forwards standard
input, output and signals to it:

```sh
java -Dbuild.executable=demo-server -jar demo-server.jar
```

### A thin launcher

By default the launcher bundles every dependency into one fat JAR. Adding a dependency on
Soundness's on-demand dependency loader and overriding `main` to wrap the inherited entry
point in `externalize` distributes the server as a thin launcher instead, whose
dependencies are fetched and cached the first time they are needed:

```scala
override def main(args: IArray[Text]): Unit = externalize(super.main(args))
```

`externalize` hashes each dependency as the server's own module compiles and records the
list in the compiled artifact. When that artifact is later repackaged, each dependency
published somewhere is referenced by URL and hash rather than inlined, so the launcher stays
small.
