## MCP

### About

The [Model Context Protocol](https://modelcontextprotocol.io/) is how an AI assistant reaches
outside itself, by calling tools, reading resources and using prompts which a server provides.

An MCP server here is an object with annotated methods. A method marked `@tool` becomes a
callable tool, and its parameters and result are described to the client by
[JSON schemas](json.md#schemas) derived from their types. `@resource` exposes content at a URI.
`@prompt` supplies a conversation template. The protocol itself, which is JSON-RPC over
streamable HTTP, with sessions and listings, is generated from those definitions.

### On MCP

An MCP server describes itself. Tools are advertised with their names, descriptions and input
schemas, and the client, which is the harness of a language model, decides what to call from
those descriptions. Writing that self-description by hand duplicates the code it describes, and
the two drift. The schema says one thing and the implementation another, and the model's tool
calls fail in ways it cannot see.

Deriving the description from the implementation removes the gap. The method *is* the tool. Its
name, its parameter types and its result type generate the listing, and dispatch goes straight to
it. The schema cannot disagree with the code, which is
[correctness](../philosophy/correctness.md) by construction.

Everything comes from the `soundness` package:

```scala
import soundness.*

import classloaders.threadContextClassloader
import codepages.utf8Codepage
import errorDiagnostics.stackTracesDiagnostics
import internetAccess.online
import logging.silentLogging
import probates.awaitProbate
import strategies.throwUnsafely
import threads.virtualThreads
import webserverErrorPages.minimalErrorPage
```

The samples on this page serve a protocol over HTTP, so they are compiled but not run.

### A server

A server extends `Mcp.Server`, names itself, and defines its capabilities as annotated methods.
Each connected client gets a `Session`, created by `initialize`, in which the server may keep
state for that client:

```scala
val colors = Map(t"sky" -> t"blue", t"grass" -> t"green")

object AssistantTools extends Mcp.Server():
  class Session() extends Mcp.Session
  def initialize(): Session = new Session()

  def name: Text = t"assistant-tools"
  def description: Text = t"Utilities for the assistant"
  def version: Semver = v"1.0.0"
  def prompts: List[Mcp.Prompt] = Nil

  @tool
  @about("Look up the color of a named thing")
  def color(name: Text): Text = colors.at(name).or(t"unknown")
```

The input schema of the tool, a `name` of type string, and its result schema are derived from
the signature. `@about` supplies the description which the model reads when it decides whether
to call the tool. These are the annotations which the methods of a server may carry:

```tabular
column  Annotation
column  On
column  Makes it
row  `@tool`  a method  a callable tool
row  `@resource(uri)`  a method  content served at the URI
row  `@prompt`  a method  a conversation template
row  `@about(text)`  a method or a parameter  described, for the model
row  `@title(text)`  a method  titled, for display
row  `@ui(uri)`  a tool  a tool whose user interface is the resource at the URI
```

### Talking back to the client

A tool which takes a `using Mcp.Client` can speak to the client while it runs. `log` streams
progress notifications. `elicit` asks the user for structured input in the middle of a call,
with its form described by a derived schema:

```scala
object Deployer extends Mcp.Server():
  class Session() extends Mcp.Session
  def initialize(): Session = new Session()
  def name: Text = t"deployer"
  def description: Text = t"Deploys the application"
  def version: Semver = v"1.0.0"
  def prompts: List[Mcp.Prompt] = Nil

  @tool
  def deploy(target: Text)(using client: Mcp.Client): Text =
    client.log(t"Preparing deployment to $target")
    case class Confirmation(proceed: Boolean)
    client.elicit[Confirmation](t"Deploy to production?")
    t"deployed to $target"
```

A parameter is required unless its type is an [`Optional`](optional-values.md), or it has a
default argument. A client may omit those, and the tool sees `Unset` or the default. An `@about`
on a parameter describes it in the input schema of the tool:

```scala
@tool
@about(t"Lists recent runs")
def runs(@about(t"only runs in this workspace") workspace: Optional[Text], limit: Int = 20)
:   List[Text] =
  Nil
```

A failure inside a tool is something the model can read and act on. A malformed call is not:

```tabular
column  When
column  The client receives
row  a tool throws  a result marked `isError`, carrying the exception's message
row  an unknown tool is called  a JSON-RPC error
row  a required parameter is omitted  a JSON-RPC error
```

### Resources and prompts

`@resource` serves content at a URI. The content is anything which is streamable as text or
bytes. `@prompt` returns a conversation, written with the `human` and `agent` interpolators which
[LLM](llm.md) describes. The MIME type of a resource may be given with the annotation's
`mimeType`, and is otherwise chosen for it:

```tabular
column  A resource
column  Has the MIME type
row  named by a tool's `@ui`  `text/html;profile=mcp-app`
row  streamable as text  `text/plain`
row  streamable only as bytes  `application/octet-stream`
```

```scala
object Documentation extends Mcp.Server():
  class Session() extends Mcp.Session
  def initialize(): Session = new Session()
  def name: Text = t"documentation"
  def description: Text = t"The project's documentation"
  def version: Semver = v"1.0.0"
  def prompts: List[Mcp.Prompt] = Nil

  @resource("docs://readme", mimeType = t"text/markdown")
  @about("The project readme")
  def readme: Text = t"# Readme\n\nStart with `make build`."

  @prompt
  def review(topic: Text): List[Discourse] =
    List(human"Please review the $topic and summarize any problems.")
```

### Serving

The server speaks the streamable HTTP transport of MCP: JSON-RPC requests by POST, an event
stream by GET, and sessions tracked by a header. It mounts inside an ordinary
[HTTP server](http-server.md) handler:

```scala
def serve(): Unit = supervise:
  SocketServer(8080).handle:
    request.path match
      case % /: t"mcp" => AssistantTools.serve
      case _           => Http.Response(Http.NotFound)(t"Not found")
```

A client configured with that endpoint discovers the tools, resources and prompts. Everything it
sees was derived from the methods it will call.

### Related

- [ACP](acp.md) is the sibling protocol, for an editor to drive an agent.
- [LLM](llm.md) describes the `human` and `agent` interpolators, and the models which call
  these tools.
- [RPC](rpc.md) describes the JSON-RPC and server-sent events which the transport uses.
- [HTTP server](http-server.md) hosts the endpoint.
- [JSON](json.md#schemas) describes the schemas which describe each tool.
- [Annotations](annotations.md) describes how annotations are read at compiletime.
