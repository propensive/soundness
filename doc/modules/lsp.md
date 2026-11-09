## Language Server Protocol

### About

A [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) server is
one call to `Lsp.listen`. Its block registers a handler for each feature which the server
provides, and the server then serves over standard input and output. The protocol underneath,
with its JSON-RPC messages, their length-prefixed framing, the bookkeeping of open documents and
the capabilities announced to the editor, is handled. The code which remains is the logic of
the language.

The vocabulary of the protocol is modeled as ordinary types. A `Position` is a line and a
character. A `Range` is two positions. A `Diagnostic` is a range with a severity and a message.
A hover response, a completion list and a set of document symbols each have their type. A
handler receives these as typed values and returns them as typed values, never as JSON
assembled by hand.

### On the protocol

An editor which understands LSP can use the server of any language, and a language which
provides a server works in any such editor. That leverage is why the protocol exists. It is also
why the protocol is large: dozens of request and notification types, each with its own JSON
shape, exchanged over a framed stream. Implementing it by hand means marshaling JSON and
tracking the versions of documents, which is work with nothing to do with the language being
served.

That work is done once here. Each message type is a Scala type with a derived JSON codec. Each
request is dispatched to the matching registered handler. The open documents are tracked, with
incremental edits applied. What is left to write is the part specific to a language: what a
hover shows, which completions to offer, which diagnostics to report. Declaring the methods of
a server as typed handlers keeps the implementation
[honest](../philosophy/honest-signatures.md) about what it supports.

Everything comes from the `soundness` package. The LSP vocabulary itself, which is the handler
registration, the ambient document and the protocol types, comes from `Lsp`, imported inside
the server's own object, where it takes precedence over any name the umbrella exports:

```scala
import soundness.*

import backstops.stackTraceBackstop
import executives.completionsExecutive
import interpreters.posixInterpreter
import probates.awaitProbate
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import threads.virtualThreads
import workingDirectories.javaBaseWorkingDirectory
import environments.javaBaseEnvironment
```

The samples on this page serve or launch language servers, so they are compiled but not run.

### Defining a server

A server registers its features in the block given to `Lsp.listen`. There is no record of
capabilities to maintain. Each capability is advertised to the editor exactly when its handler
is registered, so the declaration can never disagree with the implementation. An option which a
capability needs beyond its presence, such as completion trigger characters or a legend for
semantic tokens, is a parameter of the registration.

Inside a handler, the context of the request is ambient, so no URIs or parameters are threaded
by hand:

```tabular
column  Name
column  Is
row  `document`  a live view of the document the request concerns: its text, line access, position/offset conversion and the word under a position
row  `workspace`  the other open documents, and what the editor reported at initialization
row  `client`  the channel back to the editor
row  `position`, `range`, `newName`, …  the request's payload
```

Here is a whole server, with its `main` in the resident-daemon idiom described below:

```scala
object DemoServer:
  import Lsp.*

  def main(args: Array[Text]): Unit = cli:
    execute:
      supervise:
        Lsp.listen(t"Demo", t"0.1.0"):
          hover:
            document.word(position).let: word =>
              Hover(MarkupContent(value = t"**$word**"))

          complete():
            CompletionList
              ( items = List
                  ( CompletionItem(label = t"alpha", kind = CompletionItemKind.Keyword),
                    CompletionItem(label = t"beta",  kind = CompletionItemKind.Keyword) ) )

          opened:
            client.publishDiagnostics
              ( document.uri,
                List
                  ( Diagnostic
                      ( range    = document.fullRange,
                        severity = DiagnosticSeverity.Warning,
                        message  = t"a diagnostic for the whole document" ) ) )

          command(t"demo.run"):
            raise(Lsp.Error(Lsp.Error.Reason.RequestFailed, t"nothing to run"))
            Unset
      Exit.Ok
```

The fragments which follow each belong inside such a `listen` block.

Registrations exist for the full surface of the protocol. Definitions, references, document
symbols, formatting, renaming, code actions, signature help, folding, semantic tokens, call and
type hierarchies, workspace symbols, file operations and the `*/resolve` family all follow the
same shape: the payload is ambient, and a typed result comes out.

### Documents and diagnostics

The server tracks each open document as the editor reports it, and applies incremental changes
at the protocol's UTF-16 offsets. The lifecycle is observed by registering `opened`, `changed`,
`saved` and `closed` handlers, each of which sees the current state of the document. From any
handler, `client` is the channel back to the editor. That is how a server pushes diagnostics,
which are errors and warnings, rather than waiting to be asked, as the `opened` registration
above does:

```scala fuel
typecheck
##
object DiagnosingServer:
  import Lsp.*

  def serve(): Unit = supervise:
    Lsp.listen(t"Diagnosing", t"0.1.0"):
      opened:
        client.publishDiagnostics
          ( document.uri,
            List
              ( Diagnostic
                  ( range    = document.fullRange,
                    severity = DiagnosticSeverity.Warning,
                    message  = t"a diagnostic for the whole document" ) ) )
```

`client` also carries `showMessage` and `logMessage`, for notices shown to the user and lines
written to the editor's log.

### Observing the traffic

The standard output of a server is reserved for the wire protocol, so the server cannot print a
record of what it is exchanging. `Lsp.listen` takes an optional observer instead. It sees each
message as the JSON body it was carried as: inbound before it is parsed, so a message which
fails to decode is observed too, and outbound before it is framed:

```scala
val observer = new Lsp.Observer:
  def received(message: Text): Unit = Err.println(t"recv $message")
  def sent(message: Text): Unit = Err.println(t"send $message")
```

Passing it as `Lsp.listen(t"Demo", t"0.1.0", observer)` traces the exchange on standard error.

What the observer does with a message is the server's own concern. It might stream it to a
second process, write it to a trace file, or do nothing at all. Omitting the parameter observes
nothing.

### Reporting errors

A handler which cannot answer raises a typed fault, and continues, as the `demo.run` command
above does with `Lsp.Error(Lsp.Error.Reason.RequestFailed, t"nothing to run")`.

A raised fault pre-empts the result of the handler. For a request, it becomes a JSON-RPC error
response, with the wire code of the reason and the id of the request. For a notification,
which may not be answered, it is reported through `window/logMessage`. A request for a document
which is not open, and a command which was never registered, are answered the same way.

### Talking to a server

The same vocabulary reads in the other direction. `Lsp.Server` describes a language server which
this process can speak to: a command to launch, or a pair of streams already connected to one.
`session` opens the channel, lends a connection for the duration of a block, and disposes of
both afterwards. A connection cannot escape the block, so it can never outlive the server which
answers it:

```scala
def query(source: Text): Optional[Lsp.Hover] = supervise:
  Lsp.Server(sh"rust-analyzer").session: server ?=>
    server.initialize(root = t"file:///project")
    server.initialized()
    server.open(t"file:///project/src/main.rs", t"rust", source)
    server.hover(t"file:///project/src/main.rs", Lsp.Position(10, 4))
```

Requests return the same types which the handlers of a server return: `Optional[Hover]`,
`CompletionList`, `List[Location]`. Notifications, which are `open`, `edit`, `save` and
`close`, return nothing. An error response from the server is raised as an `Lsp.Error` which
carries the reason its wire code names, and is not awaited forever. Requests are answered on a
task of their own, so several may be in flight at once, and may come back in any order.

What a server says unbidden, such as diagnostics, window messages and progress, reaches an
`Lsp.Listener`. Its methods all default to doing nothing, so a client implements only what it
acts on. A listener is supplied contextually:

```scala
given diagnostics: Lsp.Listener = new Lsp.Listener:
  override def diagnostics(uri: Text, version: Optional[Int], reports: List[Lsp.Diagnostic])
  :   Unit =
    reports.each { report => Err.println(t"$uri: ${report.message}") }
```

### Proxying a server

A proxy is both halves at once. It serves an editor over its own standard input and output,
while it holds a session with a real language server. It forwards everything between them, and
amends what it chooses to. `Lsp.proxy` runs that exchange, and its block registers the
amendments:

```scala
object DemoProxy:
  import Lsp.*

  def main(args: Array[Text]): Unit = cli:
    execute:
      supervise:
        Lsp.proxy(Lsp.Server(sh"rust-analyzer")):
          rewrite.capabilities(_.copy(hoverProvider = true))

          rewrite.hover: hover =>
            hover.copy(contents = MarkupContent(value = t"_(proxied)_ ${hover.contents.value}"))

          rewrite.diagnostics(_.filter(_.severity != DiagnosticSeverity.Hint))
      Exit.Ok
```

Everything which is not registered is forwarded byte for byte: methods which this library does
not model, capabilities it does not know about, and the editor's own request ids, which are
never rewritten. The relay is asynchronous in both directions, so a proxy changes what an
exchange says, and not how it behaves.

Each `rewrite` combinator decodes the payload into the same type which the server side uses,
applies the function, and encodes it again:

```tabular
column  Combinator
column  Amends
column  As
row  `rewrite.capabilities`  the result of `initialize`  `ServerCapabilities`
row  `rewrite.hover`  the result of `textDocument/hover`  `Hover`
row  `rewrite.completions`  the result of `textDocument/completion`  `CompletionList`
row  `rewrite.definitions`  the result of `textDocument/definition`  `List[Location]`
row  `rewrite.symbols`  the result of `textDocument/documentSymbol`  `List[DocumentSymbol]`
row  `rewrite.codeActions`  the result of `textDocument/codeAction`  `List[CodeAction]`
row  `rewrite.codeLenses`  the result of `textDocument/codeLens`  `List[CodeLens]`
row  `rewrite.inlayHints`  the result of `textDocument/inlayHint`  `List[InlayHint]`
row  `rewrite.signatureHelp`  the result of `textDocument/signatureHelp`  `SignatureHelp`
row  `rewrite.diagnostics`  the reports in `textDocument/publishDiagnostics`  `List[Diagnostic]`
row  `rewrite.result`  the result of the named method  `Json`
row  `rewrite.notification`  the parameters of the named notification from the server  `Json`
```

A result which the server sent as `null`, having nothing to say, is passed on as it stands, so
a rewriter never has to spell out the empty case. For whole messages there are two more hooks,
`rewrite.outbound` and `rewrite.inbound`, each returning what should become of the message:

```scala fuel
parse
##
object QuietProxy:
  import Lsp.*

  def serve(): Unit = supervise:
    Lsp.proxy(Lsp.Server(sh"rust-analyzer")):
      rewrite.outbound: (method, message) =>
        if method == t"textDocument/inlineValue" then Transit.Drop else Transit.Forward
```

A hook answers with one of four `Transit` values:

```tabular
column  Transit
column  Does
row  `Transit.Forward`  passes the message on unchanged
row  `Transit.Rewrite`  passes on a different message
row  `Transit.Drop`  swallows it
row  `Transit.Answer`  answers a request here, so that the server upstream never sees it
```

A proxy is not limited to what the editor asks. The session with the server upstream is in
scope as `upstream` throughout the registration block, so a hook may ask a question of its own,
typically for the project state which only the server knows. `rewrite.connected` runs once, on
a task of its own, as soon as that session is open:

```scala fuel
parse
##
object ClasspathProxy:
  import Lsp.*
  import formatting.compactJsonFormatting

  def serve(): Unit = supervise:
    Lsp.proxy(Lsp.Server(sh"jdtls")):
      val classpath: Promise[Json] = Promise()

      rewrite.connected:
        classpath.offer(upstream.execute(t"java.project.getClasspaths").or(List[Text]().in[Json]))

      rewrite.inbound: (method, message) =>
        if method == t"language/status"
        then async(Err.println(t"classpath: ${classpath.await()}"))

        Transit.Forward
```

Answers come back to the caller which asked for them, and never reach the editor. A response to
a request which the proxy made itself is routed to whoever awaits it, and is not relayed.

Mind which thread each hook runs on. A request awaited on the task which reads the server's
messages, which is the task which would deliver the answer, would wait forever:

```tabular
column  Hook
column  Runs on
column  May await
row  `rewrite.inbound`  the task that reads the server's messages  no; make the request in an `async` block, as above
row  `rewrite.outbound`  the loop that reads the editor  yes, though the editor waits with it
row  `rewrite.connected`  a task of its own  yes, freely
```

### Running the server

`Lsp.listen` serves standard input until it is exhausted. A server object therefore supplies a
small `main` in the resident-daemon idiom, as `DemoServer` above does: the `cli` entry point,
an `execute` block, and `supervise` for the tasks which the server runs. An editor configured
to launch the object as its language server exchanges JSON-RPC with it over the pipe, and each
request arrives at the matching handler.

### Fast startup

The `cli` entry point runs the server as a resident [daemon](daemons.md). The first invocation
launches a background JVM, and every later one connects to the process which is already
running. An editor starts and restarts a language server often, so it matters to avoid the
JVM's startup cost on each launch, and to keep the optimizations which the just-in-time
compiler has accumulated. Packaging the server's JAR with the `xek` builder, from
[propensive/xek](https://github.com/propensive/xek), assembles a small native launcher which
starts, or connects to, the daemon, and forwards standard input, output and signals to it:

```sh
xek build demo-server.jar
```

### A thin launcher

By default the launcher bundles every dependency into one fat JAR. Adding a dependency on the
on-demand dependency loader, and wrapping the body of the entry point in `externalize`,
distributes the server as a thin launcher instead. Its dependencies are fetched and cached the
first time they are needed. `externalize` hashes each dependency as the server's own module
compiles, and records the list in the compiled artifact. When that artifact is later repackaged,
each dependency which is published somewhere is referenced by URL and hash rather than inlined,
so the launcher stays small.

### Related

- [Daemons](daemons.md) describes the resident process which `cli` runs a server as.
- [CLI](cli.md) describes the `cli` and `execute` entry points.
- [RPC](rpc.md) describes the JSON-RPC and `Content-Length` framing which the protocol uses.
- [Debugging](debugging.md) serves the sibling Debug Adapter Protocol.
- [Concurrency](concurrency.md) describes `supervise`, `async` and `Promise`.
- [Processes](processes.md) describes the `sh"…"` command which launches a server.
