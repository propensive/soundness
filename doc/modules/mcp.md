## MCP

### About

The [Model Context Protocol](https://modelcontextprotocol.io/) is how an AI assistant reaches
outside itself — calling tools, reading resources, using prompts that a server provides. A
Soundness MCP server is an object with annotated methods: a method marked `@tool` becomes a
callable tool, its parameters and result described to the client by
[JSON schemas](json.md) derived from their types; `@resource` exposes content at a URI; `@prompt`
supplies conversation templates. The protocol — JSON-RPC over streamable HTTP, sessions, listings —
is generated from those definitions.

### On MCP

An MCP server describes itself: tools are advertised with their names, descriptions and input
schemas, and the client — a language model's harness — decides what to call from those
descriptions. Writing that self-description by hand duplicates the code it describes, and the two
drift; the schema says one thing, the implementation another, and the model's tool calls fail in
ways it cannot see.

Tools declared as typed methods, with their schemas derived, are [correctness](../philosophy/correctness.md) by construction: the schema cannot disagree with the code.

Deriving the description from the implementation removes the gap. The method *is* the tool: its
name, its parameter types, its result type generate the listing, and dispatch goes straight to it.
Everything comes from the `soundness` package:

```scala
import soundness.*
```

### A server

A server extends `Mcp.Server`, names itself, and defines its capabilities as annotated methods.
Each connected client gets a `Session`, created by `initialize`, in which the server may keep
per-client state:

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

The tool's input schema — a `name` of type string — and its result schema are derived from the
signature; `@about` supplies the description the model reads when deciding to call it.

### Talking back to the client

A tool that takes a `using Mcp.Client` can speak to the client while it runs: `log` streams progress
notifications, and `elicit` asks the user for structured input mid-call, its form described by a
derived schema:

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

### Resources and prompts

`@resource` serves content at a URI — anything streamable as text or bytes, an
[HTML](html.md) document included — and `@prompt` returns a conversation written with the `human`
and `agent` interpolators:

```scala
object Documentation extends Mcp.Server():
  class Session() extends Mcp.Session
  def initialize(): Session = new Session()
  def name: Text = t"documentation"
  def description: Text = t"The project's documentation"
  def version: Semver = v"1.0.0"
  def prompts: List[Mcp.Prompt] = Nil

  @resource("docs://readme")
  @about("The project readme")
  def readme: Text = t"# Readme\n\nStart with `make build`."

  @prompt
  def review(topic: Text): List[Discourse] =
    List(human"Please review the $topic and summarize any problems.")
```

### Serving

The server speaks MCP's streamable HTTP transport — JSON-RPC requests by POST, an event stream by
GET, sessions tracked by header — and mounts inside an ordinary [HTTP server](http-server.md)
handler:

```scala
def serve(): Unit = supervise:
  SocketServer(8080).handle:
    request.path match
      case % /: t"mcp" => AssistantTools.serve
      case _           => Http.Response(Http.NotFound)(t"Not found")
```

A client configured with that endpoint discovers the tools, resources and prompts, and everything
it sees was derived from the methods it will call.
