## ACP

<!-- doccheck: language captureChecking -->

### About

The [Agent Client Protocol](https://agentclientprotocol.com/) is how an editor — or any program —
drives a coding agent: Claude Code, Gemini CLI, and a growing set of others speak it over the
agent's standard input and output. The client half of the protocol, version 1, is implemented for
programmatic use: spawn an agent, open a session, send a prompt, and watch its work stream back —
messages, thoughts, tool calls, plans — while answering the requests it makes of you: permission
to act, file access, terminals.

### On ACP

The protocol is JSON-RPC both ways at once. The client calls the agent — `initialize`,
`session/new`, `session/prompt` — and the agent calls back, mid-turn: a `session/update`
notification for every chunk of progress, and requests that block its work until the client
answers. A prompt is therefore not a function call but a *turn*: one long-lived request whose
response arrives only after everything the agent did inside it.

The turn is modeled directly. `prompt` blocks until the agent reports why the turn ended, while
the updates stream concurrently to handlers registered before connecting; the capabilities the
client advertises are derived from those registrations, so the declaration can never disagree
with the implementation. A connection needs a few capabilities of its own: a way to run the reader
and writer tasks, an error strategy, a working directory for the agent, and somewhere to print:

```scala
import soundness.*

import errorDiagnostics.stackTracesDiagnostics
import probates.awaitProbate
import stdios.javaLangSystemStdio
import strategies.throwUnsafely
import threading.virtualThreading
import workingDirectories.javaBaseWorkingDirectory
```

Every request and notification is a typed value rather than a JSON document, in the spirit of [honest signatures](../philosophy/honest-signatures.md).

### Connecting

`Acp.connect` spawns the agent, exchanges `initialize`, and lends a connection for the duration
of a block. The first block registers handlers; the second holds the live connection, reached
through `Acp.connection`. Because the exchange runs as concurrent tasks, the whole thing sits
inside a `supervise` block:

```scala
supervise:
  Acp.connect(Acp.Agent(sh"claude-code-acp")):
    Acp.updated:
      Acp.update match
        case Acp.AgentMessageChunk(Acp.TextContent(text, _)) => Out.print(text)
        case other                                           => ()

    Acp.permission:
      Acp.options.filter(_.kind == Acp.PermissionOptionKind.AllowOnce).prim
      . lay(Acp.Cancelled): option => Acp.Selected(option.optionId)

  . apply:
      val session = Acp.connection.newSession(workingDirectory[Text])
      Acp.connection.prompt(session.sessionId, t"Fix the failing test")
```

The value of the second block is the value of `connect`; here it is the turn's stop reason. When
the block returns, the exchange is torn down and a subprocess agent is killed, so nothing outlives
the agent it speaks to. An agent that is already running — behind a socket, or a fixture in a
test — connects through `Acp.Agent.streams(input, output)` instead of a command.

An unregistered capability is never advertised: a client that registers no `readFile` handler
reports no filesystem support, and a conformant agent will not ask. `Acp.capabilities` adjusts the
derived declaration as a last resort, applied at initialization.

### Updates

Every handler reads its payload through a contextual accessor — `Acp.update`, `Acp.options`,
`Acp.path` — and every one is lent the `Acp.sessionId` it concerns. An update is one of the
protocol's `SessionUpdate` cases:

```scala
def register(using Acp.Registry^): Unit = Acp.updated:
  Acp.update match
    case Acp.AgentMessageChunk(Acp.TextContent(text, _)) => Out.print(text)
    case Acp.AgentThoughtChunk(Acp.TextContent(text, _)) => Out.print(t"(thinking) $text")
    case Acp.UserMessageChunk(content)   => ()
    case call: Acp.ToolCall              => Out.println(t"${call.title} (${call.kind})")
    case update: Acp.ToolCallUpdate      => Out.println(t"${update.toolCallId}: ${update.status}")
    case Acp.Plan(entries)               => entries.each { entry => Out.println(entry.content) }
    case Acp.AvailableCommandsUpdate(commands) => ()
    case Acp.CurrentModeUpdate(modeId)   => ()
```

### Permissions

An agent about to act asks for permission, offering a list of options. The handler chooses one
with `Acp.Selected`, or answers `Acp.Cancelled`. The option kinds are `AllowOnce`, `AllowAlways`,
`RejectOnce` and `RejectAlways`, so a policy is a filter over them:

```scala
def allowAlways(using Acp.Registry^): Unit = Acp.permission:
  Acp.options.filter(_.kind == Acp.PermissionOptionKind.AllowAlways).prim
  . lay(Acp.Cancelled): option => Acp.Selected(option.optionId)
```

### Filesystem and terminals

The agent works with *your* view of the world if you offer one. `Acp.readFile` serves the agent's
requests to read a file — including unsaved editor state, if you have any — and `Acp.writeFile`
its requests to write one. A read handler sees the `path`, and optionally a starting `line` and a
`limit` on the number of lines; a write handler sees the `path` and the `content`:

```scala
import charEncoders.utf8Encoder
import charDecoders.utf8Decoder
import filesystemOptions.createNonexistentParents

def files(using Acp.Registry^): Unit =
  Acp.readFile:
    Acp.path.as[Path on Linux].open[File]()(file.read[Text])

  Acp.writeFile:
    Acp.path.as[Path on Linux].create[File](): handle ?=>
      handle.write(Acp.content)
```

Terminals are advertised as a single capability, so the five operations — create a terminal,
read its output, wait for it to exit, kill it, and release it — are supplied together as one
`Acp.Terminals` bundle to `Acp.terminals`.

### Sessions

`newSession` takes the working directory the agent should use and, optionally, a list of MCP
servers (`Acp.McpServer`) to connect it to. `loadSession` resumes a session by its id, replaying
its history through the update handler. An agent that offers modes reports them in the session's
`modes`, and `setMode` switches between them. An agent that requires authentication says so with
an `Acp.Error` whose reason is `AuthRequired`; `authenticate` answers with one of the methods the
agent advertised at `initialize`.

### Stop reasons

`prompt` returns an `Acp.StopReason`: `EndTurn` when the agent finished, `MaxTokens` or
`MaxTurnRequests` when it ran out of budget, `Refusal` when it declined, and `Cancelled` when the
client canceled the turn.

### Cancellation

`Acp.connection.cancel(sessionId)` notifies the agent, and the turn completes with the
`Cancelled` stop reason. Permission requests pending at that moment — or arriving before the next
turn — are answered `Cancelled` automatically, as the protocol requires; opening a new turn resets
the session.

### Errors

Every fault the agent reports, and every fault in the exchange, is an `Acp.Error` whose reason is
one of the JSON-RPC codes — `Parse`, `InvalidRequest`, `MethodNotFound`, `InvalidParams`,
`Internal`, `AuthRequired` — or `Incompatible`, raised when the agent answers `initialize` with a
protocol version other than 1. A fault raised inside a handler is converted into an error response
carrying the reason's wire code, so a client bug is reported to the agent rather than hanging the
turn. `connect` takes an optional `Acp.Observer`, which sees every message in both directions as
it crosses the transport, for logging or a protocol trace.

### Content

ACP adopts MCP's content vocabulary — `TextContent`, `ImageContent`, `AudioContent`,
`ResourceLink`, `EmbeddedResource` — and the types are shared with [MCP](mcp.md), so content
crosses between the two protocols without translation.
