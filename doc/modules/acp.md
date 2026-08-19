## ACP

### About

The [Agent Client Protocol](https://agentclientprotocol.com/) is how an editor — or any program —
drives a coding agent: Claude Code, Gemini CLI, and a growing set of others speak it over the
agent's standard input and output. Espionage implements the client half of the protocol, version
1, for programmatic use: spawn an agent, open a session, send a prompt, and watch its work stream
back — messages, thoughts, tool calls, plans — while answering the requests it makes of you:
permission to act, file access, terminals.

### On ACP

The protocol is JSON-RPC both ways at once. The client calls the agent — `initialize`,
`session/new`, `session/prompt` — and the agent calls back, mid-turn: a `session/update`
notification for every chunk of progress, and requests that block its work until the client
answers. A prompt is therefore not a function call but a *turn*: one long-lived request whose
response arrives only after everything the agent did inside it.

Espionage models the turn directly. `prompt` blocks until the agent reports why the turn ended,
while the updates stream concurrently to handlers registered before connecting; the capabilities
the client advertises are derived from those registrations, so the declaration can never disagree
with the implementation. Everything comes from the `soundness` package:

```scala
import soundness.*
```

### Connecting

`Acp.connect` spawns the agent, exchanges `initialize`, and lends a connection for the duration
of a block. The first block registers handlers; the second holds the live connection:

```scala
Acp.connect(Acp.Agent(sh"claude-code-acp")):
  Acp.updated:
    Acp.update match
      case Acp.AgentMessageChunk(Acp.TextContent(text, _)) => Out.print(text)
      case other                                           => ()

  Acp.permission:
    Acp.options.filter(_.kind == Acp.PermissionOptionKind.AllowOnce).prim
    . lay(Acp.Cancelled): option => Acp.Selected(option.optionId)

. apply:
    val session = connection.newSession(workingDirectory[Text])
    val stopReason = connection.prompt(session.sessionId, t"Fix the failing test")
```

An unregistered capability is never advertised: a client that registers no `readFile` handler
reports no filesystem support, and a conformant agent will not ask.

### Filesystem and terminals

The agent works with *your* view of the world if you offer one: `Acp.readFile` and
`Acp.writeFile` serve the agent's file requests (including unsaved editor state, if you have
any), and `Acp.terminals` supplies the five terminal operations as one bundle, since the protocol
advertises them as a single capability.

### Cancellation

`connection.cancel(sessionId)` notifies the agent, and the turn completes with the `Cancelled`
stop reason. Permission requests pending at that moment — or arriving before the next turn — are
answered `Cancelled` automatically, as the protocol requires; opening a new turn resets the
session.

### Content

ACP adopts MCP's content vocabulary — `TextContent`, `ImageContent`, `AudioContent`,
`ResourceLink`, `EmbeddedResource` — and Espionage shares those types with
[synesthesia](mcp.md), so content crosses between the two protocols without translation.
