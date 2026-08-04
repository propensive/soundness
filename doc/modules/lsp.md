## Language Server Protocol

### About

A [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) server
is one call to `Lsp.listen`: its block registers a handler for each feature the server
provides, and the server then serves over standard input and output. The protocol
underneath — JSON-RPC messages, their length-prefixed framing, the bookkeeping of open
documents, and the capabilities announced to the editor — is handled, so the code that
remains is the language logic.

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
each request is dispatched to the matching registered handler, and the open documents are
tracked — with incremental edits applied — for the server. What is left to write is the
part that is specific to a language: what a hover shows, what completions to offer, which
diagnostics to report. Everything comes from the `soundness` package, and the LSP
vocabulary itself — handler registration, the ambient document, the protocol types — from
`Lsp`, imported inside the server's own object:

```scala
import soundness.*

object DemoServer:
  import Lsp.*
```

### Defining a server

A server registers its features in the block given to `Lsp.listen`. There is no
capabilities record to maintain: each capability is advertised to the editor exactly when
its handler was registered, so the declaration can never disagree with the implementation.
Options a capability needs beyond mere presence — completion trigger characters, a
semantic-tokens legend — are parameters of the registration.

Inside a handler, the current document is ambient: `document` is a live view of the
document the request concerns, with its text, line access, position/offset conversion and
the word under a position; `workspace` reaches the other open documents and what the
editor reported at initialization; and request payloads such as `position` are ambient
too, so no URIs or parameters are threaded by hand.

```scala
Lsp.listen(t"Demo", t"0.1.0"):
  hover:
    document.word(position).let: word =>
      Hover(MarkupContent(value = t"**$word**"))

  complete():
    CompletionList
      ( items = List
          ( CompletionItem(label = t"alpha", kind = CompletionItemKind.Keyword),
            CompletionItem(label = t"beta",  kind = CompletionItemKind.Keyword) ) )
```

Registrations exist for the full protocol surface: definitions, references, document
symbols, formatting, renaming, code actions, signature help, folding, semantic tokens,
call and type hierarchies, workspace symbols, file operations and the `*/resolve` family
all follow the same shape — payloads ambient, a typed result out.

### Documents and diagnostics

The server tracks each open document as the editor reports it, applying incremental
changes at the protocol's UTF-16 offsets. The lifecycle can be observed by registering
`opened`, `changed`, `saved` and `closed` handlers, each of which sees the document's
current state. From any handler, `client` is the channel back to the editor, which is how
a server pushes diagnostics — errors and warnings — rather than waiting to be asked:

```scala
  opened:
    client.publishDiagnostics
      ( document.uri,
        List
          ( Diagnostic
              ( range    = document.fullRange,
                severity = DiagnosticSeverity.Warning,
                message  = t"a diagnostic for the whole document" ) ) )
```

`client` also carries `showMessage` and `logMessage`, for notices shown to the user and
lines written to the editor's log.

### Observing the traffic

A server's standard output is reserved for the wire protocol, so it cannot print a record
of what it is exchanging. `Lsp.listen` takes an optional observer instead, which sees each
message as the JSON body it was carried as — inbound before it is parsed, so a message
that fails to decode is observed too, and outbound before it is framed:

```scala
  val observer = new Lsp.Observer:
    def received(message: Text): Unit = log.put(t"recv $message")
    def sent(message: Text): Unit = log.put(t"send $message")

  Lsp.listen(t"Demo", t"0.1.0", observer):
    ...
```

What the observer does with a message is the server's own concern: a debugging aid that
streams it to a second process, a trace file, or nothing at all. Omitting the parameter
observes nothing.

### Reporting errors

A handler that cannot answer raises a typed fault, and continues:

```scala
  command(t"demo.run"):
    raise(LspError(LspError.Reason.RequestFailed, t"nothing to run"))
    Unset
```

A raised fault pre-empts the handler's result: for a request it becomes a JSON-RPC error
response with the reason's wire code and the request's own id; for a notification, which
may not be answered, it is reported through `window/logMessage`. Requests for documents
that are not open, and commands that were never registered, are answered the same way.

### Talking to a server

The same vocabulary reads in the other direction. `Lsp.Server` describes a language server
this process can speak to — a command to launch, or a pair of streams already connected to
one — and `session`, the extension method from Soundness's `Sessional` typeclass, opens the
channel, lends a connection for the duration of a block, and disposes of both afterwards. A
connection cannot escape the block, so it can never outlive the server that answers it.

```scala
Lsp.Server(sh"rust-analyzer").session: server ?=>
  server.initialize(root = t"file:///project")
  server.initialized()
  server.open(t"file:///project/src/main.rs", t"rust", source)
  server.hover(t"file:///project/src/main.rs", Position(10, 4))
```

Requests return the same types a server's handlers return — `Optional[Hover]`,
`CompletionList`, `List[Location]` — and notifications (`open`, `edit`, `save`, `close`)
return nothing. An error response from the server is raised as an `LspError` carrying the
reason its wire code names, rather than being awaited forever. Requests are answered on a
task of their own, so several may be in flight at once, and may come back in any order.

What a server says unbidden — diagnostics, window messages, progress — reaches an
`Lsp.Listener`, whose methods all default to doing nothing, so a client implements only what
it acts on. A listener is supplied contextually:

```scala
given diagnostics: Lsp.Listener = new Lsp.Listener:
  override def diagnostics(uri: Text, version: Optional[Int], reports: List[Diagnostic])
  :   Unit =
    reports.each { report => Out.println(t"$uri: ${report.message}") }
```

### Proxying a server

A proxy is both halves at once: it serves an editor over its own standard input and output
while holding a session with a real language server, forwarding everything between them and
amending what it chooses to. `Lsp.proxy` runs that exchange, and the block registers the
amendments:

```scala
Lsp.proxy(Lsp.Server(sh"rust-analyzer")):
  rewrite.capabilities(_.copy(hoverProvider = true))

  rewrite.hover: hover =>
    hover.copy(contents = MarkupContent(value = t"_(proxied)_ ${hover.contents.value}"))

  rewrite.diagnostics(_.filter(_.severity != DiagnosticSeverity.Hint))
```

Everything not registered is forwarded byte for byte: methods this library does not model,
capabilities it does not know about, and the editor's own request ids, which are never
rewritten. The relay is asynchronous in both directions, so a proxy changes what an exchange
says, not how it behaves.

Each `rewrite` combinator decodes the payload into the same type the server side uses,
applies the function and re-encodes it; `rewrite.result` and `rewrite.notification` take a
method name and a `Json => Json` for anything without a combinator of its own. For whole
messages there are two more hooks — `rewrite.outbound` and `rewrite.inbound` — each
returning what should become of the message:

```scala
rewrite.outbound: (method, message) =>
  if method == t"textDocument/inlineValue" then Transit.Drop else Transit.Forward
```

`Transit.Forward` passes the message on, `Transit.Rewrite` passes on a different one,
`Transit.Drop` swallows it, and `Transit.Answer` answers a request here, so that the server
upstream never sees it.

### Running the server

`Lsp.listen` serves standard input until it is exhausted, so a server object supplies a
small `main` in the resident-daemon idiom:

```scala
  def main(args: Array[Text]): Unit = cli:
    execute:
      supervise:
        Lsp.listen(t"Demo", t"0.1.0"):
          ...
      Exit.Ok
```

An editor configured to launch the object as its language server exchanges JSON-RPC with
it over the pipe, and each request arrives at the matching handler.

### Fast startup

The `cli` entry point runs the server as a resident [daemon](daemons.md): the first
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
Soundness's on-demand dependency loader and wrapping the entry point's body in
`externalize` distributes the server as a thin launcher instead, whose dependencies are
fetched and cached the first time they are needed. `externalize` hashes each dependency as
the server's own module compiles and records the list in the compiled artifact; when that
artifact is later repackaged, each dependency published somewhere is referenced by URL and
hash rather than inlined, so the launcher stays small.
