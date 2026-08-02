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
