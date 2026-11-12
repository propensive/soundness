## ACP

### About

The [Agent Client Protocol](https://agentclientprotocol.com/) is how an editor, or any program,
drives a coding agent. Claude Code, Gemini CLI and a growing set of others speak it over the
agent's standard input and output.

The client half of the protocol, version 1, is implemented here for use from a program. It
spawns an agent, opens a session, sends a prompt, and watches the agent's work stream back as
messages, thoughts, tool calls and plans. Meanwhile it answers the requests which the agent
makes of it: permission to act, access to files, and terminals.

### On ACP

The protocol is JSON-RPC in both directions at once. The client calls the agent, with
`initialize`, `session/new` and `session/prompt`. The agent calls back in the middle of a turn,
with a `session/update` notification for every chunk of progress, and with requests which block
its work until the client answers. A prompt is therefore not a function call but a *turn*: one
long-lived request whose response arrives only after everything the agent did inside it.

The turn is modeled directly. `prompt` blocks until the agent reports why the turn ended, while
the updates stream concurrently to handlers which were registered before connecting. The
capabilities which the client advertises are derived from those registrations, so the
declaration can never disagree with the implementation. Every request and notification is a
typed value, not a JSON document, in the spirit of
[honest signatures](../philosophy/honest-signatures.md).

A connection needs a few [capabilities][capability] of its own: a way to run the reader and
writer tasks, an error [strategy], a working directory for the agent, and somewhere to print:

```scala fuel
feature experimental.captureChecking
##
import soundness.*
import environments.javaBaseEnvironment

import errorDiagnostics.stackTracesDiagnostics
import probates.awaitProbate
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap
import strategies.throwUnsafely
import threads.virtualThreads
import workingDirectories.javaBaseWorkingDirectory
```

The samples on this page drive an agent in another process, so they are compiled but not run.

### Connecting

`Acp.connect` spawns the agent, exchanges `initialize`, and lends a connection for the duration
of a block. The first block registers handlers. The second holds the live connection, which is
reached through `Acp.connection`. The exchange runs as concurrent tasks, so the whole thing sits
inside a `supervise` block:

```scala
supervise:
  Acp.connect(Acp.Endpoint(sh"claude-code-acp")):
    Acp.updated:
      Acp.update match
        case Acp.AgentMessageChunk(Acp.TextContent(text, _)) => Out.print(text)
        case other                                           => ()

    Acp.permission:
      Acp.options.filter(_.kind == Acp.PermissionOptionKind.AllowOnce).head
      . lay(Acp.Cancelled): option => Acp.Selected(option.optionId)

  . apply:
      val session = Acp.connection.newSession(summon[WorkingDirectory].directory())
      Acp.connection.prompt(session.sessionId, t"Fix the failing test")
```

The value of the second block is the value of `connect`. Here it is the stop reason of the turn.
When the block returns, the exchange is torn down, and a subprocess agent is killed, so nothing
outlives the agent it speaks to. An agent which is already running, behind a socket or as a
fixture in a test, connects through `Acp.Endpoint.streams(input, output)` instead of a command.

A capability which is not registered is never advertised. A client which registers no
`readFile` handler reports no filesystem support, and a conformant agent will not ask.
`Acp.capabilities` adjusts the derived declaration as a last resort, and is applied at
initialization.

### Updates

Every handler reads its payload through contextual accessors, and every one is lent the
`Acp.sessionId` it concerns:

```tabular
column  Handler
column  Payload accessors
column  Answer
row  `Acp.updated`  `Acp.update`  nothing
row  `Acp.permission`  `Acp.toolCall`, `Acp.options`  `Acp.Selected` or `Acp.Cancelled`
row  `Acp.readFile`  `Acp.path`, `Acp.line`, `Acp.limit`  the file's `Text`
row  `Acp.writeFile`  `Acp.path`, `Acp.content`  nothing
row  `Acp.terminals`  the arguments of each operation  one `Acp.Terminals` bundle
```

An update is one of the `SessionUpdate` cases of the protocol:

```scala
def register(using Acp.Registry): Unit = Acp.updated:
  Acp.update match
    case Acp.AgentMessageChunk(Acp.TextContent(text, _)) => Out.print(text)
    case Acp.AgentThoughtChunk(Acp.TextContent(text, _)) => Out.print(t"(thinking) $text")
    case Acp.UserMessageChunk(content)   => ()
    case call: Acp.ToolCall              => Out.println(t"${call.title} (${call.kind.let(_.show).or(t"unknown")})")
    case update: Acp.ToolCallUpdate      => Out.println(t"${update.toolCallId}: ${update.status.let(_.show).or(t"unknown")}")
    case Acp.Plan(entries)               => entries.each { entry => Out.println(entry.content) }
    case Acp.AvailableCommandsUpdate(commands) => ()
    case Acp.CurrentModeUpdate(modeId)   => ()
```

### Permissions

An agent which is about to act asks for permission, and offers a list of options. The handler
chooses one with `Acp.Selected`, or answers `Acp.Cancelled`. Each option has one of four kinds of
`Acp.PermissionOptionKind`:

```tabular
column  Decision
column  Once
column  Always
row  Allow  `AllowOnce`  `AllowAlways`
row  Reject  `RejectOnce`  `RejectAlways`
```

A policy is therefore a filter over them:

```scala
def allowAlways(using Acp.Registry): Unit = Acp.permission:
  Acp.options.filter(_.kind == Acp.PermissionOptionKind.AllowAlways).head
  . lay(Acp.Cancelled): option => Acp.Selected(option.optionId)
```

### Filesystem and terminals

The agent works with *your* view of the world, if you offer one. `Acp.readFile` serves the
agent's requests to read a file, which can include the unsaved state of an editor. `Acp.writeFile`
serves its requests to write one. A read handler sees the `path`, and optionally a starting
`line` and a `limit` on the number of lines. A write handler sees the `path` and the `content`:

```scala
import codepages.utf8Codepage
import charsets.utf8Charset
import textSanitizers.skipSanitizer
import filesystemBackends.javaBaseFilesystem
import filesystemOptions.createNonexistentParents

def files(using Acp.Registry): Unit =
  Acp.readFile:
    Acp.path.as[Path on Linux].open[File]()(file.read[Text])

  Acp.writeFile:
    Acp.path.as[Path on Linux].create[File](): handle ?=>
      handle.write(Acp.content)
```

Terminals are advertised as a single capability. The five operations, which create a terminal,
read its output, wait for it to exit, kill it and release it, are supplied together as one
`Acp.Terminals` bundle to `Acp.terminals`.

### Sessions

A connection manages sessions with four methods:

```tabular
column  Method
column  Takes
column  Does
row  `newSession`  a working directory, and optionally `Acp.McpServer`s  opens a session
row  `loadSession`  a session id, a working directory, and optionally `Acp.McpServer`s  resumes the session, replaying its history through the update handler
row  `setMode`  a session id and a mode id  switches between the modes reported in the session's `modes`
row  `authenticate`  the id of a method advertised at `initialize`  authenticates with the agent
```

An agent which requires authentication says so with an `Acp.Error` whose reason is
`AuthRequired`, and `authenticate` answers it.

### Stop reasons

`prompt` returns an `Acp.StopReason`:

```tabular
column  Stop reason
column  Meaning
row  `EndTurn`  the agent finished
row  `MaxTokens`  the agent ran out of tokens
row  `MaxTurnRequests`  the agent ran out of requests for the turn
row  `Refusal`  the agent declined
row  `Cancelled`  the client canceled the turn
```

### Cancellation

`Acp.connection.cancel(sessionId)` notifies the agent, and the turn completes with the
`Cancelled` stop reason. A permission request which is pending at that moment, or which arrives
before the next turn, is answered `Cancelled` automatically, as the protocol requires. Opening a
new turn resets the session.

### Errors

Every fault which the agent reports, and every fault in the exchange, is an `Acp.Error`, whose
reason is one of these:

```tabular
column  Reason
column  Wire code
column  Raised when
row  `Parse`  -32700  a message could not be parsed as JSON
row  `InvalidRequest`  -32600  a message was not a valid JSON-RPC request
row  `MethodNotFound`  -32601  a method name was not recognized
row  `InvalidParams`  -32602  the parameters were not valid for the method
row  `Internal`  -32603  an internal error occurred
row  `AuthRequired`  -32000  the agent requires authentication
row
  cell  `Incompatible`
  cell
  cell  the agent answers `initialize` with a protocol version other than 1
```

A fault raised inside a handler is converted into an error response which carries the wire code
of the reason, so a bug in the client is reported to the agent, and does not hang the turn.
`connect` takes an optional `Acp.Observer`, which sees every message in both directions as it
crosses the transport, for logging or a protocol trace.

### Content

ACP adopts the content vocabulary of MCP: `TextContent`, `ImageContent`, `AudioContent`,
`ResourceLink` and `EmbeddedResource`. The types are shared with [MCP](mcp.md), so content
crosses between the two protocols without translation.

### Related

- [MCP](mcp.md) is the sibling protocol, through which an agent reaches tools.
- [LLM](llm.md) speaks to language models directly, without an agent in between.
- [Processes](processes.md) describes the `sh"…"` command which spawns an agent.
- [Concurrency](concurrency.md) describes `supervise`, which the exchange runs inside.
- [Filesystem](filesystem.md) describes the file operations which the handlers use.
- [RPC](rpc.md) describes the JSON-RPC which the protocol is built on.
