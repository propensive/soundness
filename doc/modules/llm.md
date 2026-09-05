## Language Models

### About

A conversation with a large language model is typed at every step: the provider and model are
values, a conversation is a *session* that accumulates its history and token usage, a reply is a
value that says why the model stopped, streamed output is pulled off the live connection, tools
are ordinary Scala methods the model may call, and a structured answer is decoded to a case
class against a schema derived from it. One vocabulary serves Anthropic's Messages API, OpenAI's
Chat Completions and Responses APIs — and any server compatible with the former, such as vLLM,
Ollama or Groq — and Google's Gemini.

### On language models

Each provider's API is a JSON dialect: a request with the model, the messages so far and a
handful of tuning knobs; a reply with content blocks, a stop reason and token counts; a streaming
variant that sends deltas; and a tool-calling protocol that returns arguments as JSON for the
program to act on and report back. The dialects differ in every detail and agree in every
concept, and the concepts are what a program cares about.

Soundness models the concepts once — messages, replies, usage, tools, stop reasons — and
translates them onto each wire in a per-provider *dialect*. A conversation is a session
confined to a block by [capture checking](../philosophy/capture-checking.md): the pure values it
produces leave freely, while the session itself and any stream in flight cannot, so a connection
cannot be held open by accident. Everything comes from the `soundness` package, with the HTTP
backend, network access and logging a client needs:

```scala
import soundness.*
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import strategies.throwUnsafely
import charEncoders.utf8Encoder
```

### Providers and models

A provider value names the model and carries the API key, and its tuning is applied by methods
that each return a new value, so a configured model is a plain immutable value that can be
shared and reused:

```scala
val key = t"sk-ant-…"

val claude = Anthropic(t"claude-sonnet-4-5", key).prompted(t"Be terse.").limit(1024)
val gpt = OpenAI(t"gpt-5", t"sk-…").warmth(0.2)
val gemini = Gemini(t"gemini-2.5-pro", t"AIza…")
val local = OpenAI.compatible(url"http://localhost:11434/v1", t"llama3")
```

`prompted` sets the system prompt, `limit` the maximum output tokens, `warmth` the temperature,
`sampling` the nucleus threshold, `stopping` the stop sequences, `iterating` the tool-loop limit,
`primed` a history to start from, and `on` an alternative base URL. A knob left unset is omitted
from the request rather than sent as a default, so the provider's own defaults apply. An
OpenAI-compatible server takes an optional key, and OpenAI's Responses API is reached from an
`OpenAI` value with `responses`.

### A conversation

A session is opened on a provider with `session`, and inside the block the `llm` accessor reaches
it. `ask` sends a message and returns the model's `Reply`; the session records both, so a second
`ask` continues the conversation:

<!-- doccheck: skip -->
```scala
val reply: Llm.Reply = claude.session:
  llm.ask(t"What is the tallest mountain in Estonia?")
  llm.ask(t"And how tall is it?")

reply.text    // the assistant's answer, as text
reply.stop    // Llm.Stop.Ended, or why the model stopped
reply.usage   // Llm.Usage(input, output, …): the tokens the last turn cost
```

A `Reply` is pure data — its message, its stop reason, its usage, and the model and identifier
the provider reported — so it may leave the block, as may a snapshot of `llm.history` or the
folded `llm.usage`. The session itself may not: returning it, or anything that captures it, is a
compile error, which is what makes it impossible to use a session after its block has closed
the connection.

### Streaming

`stream` sends a message and returns a `Response` whose `text` is an iterator of fragments,
pulled synchronously off the connection as they arrive, and whose `events` are the finer-grained
deltas. When the stream has been consumed, `reply()` assembles the complete reply — even after
partial consumption, since the remainder is read on demand — and commits the turn to the
history. A stream that is abandoned commits nothing, so a failed turn is simply retried:

<!-- doccheck: skip -->
```scala
claude.session:
  val response = llm.stream(t"Write a limerick about Tallinn.")
  response.text.each(Out.print(_))
  response.reply()
```

The response, like the session, is confined to the block.

### Tools

A tool is a method annotated `@ability`, described with `@about`, on an object that a `Toolkit`
is built from. The JSON schema for each tool's parameters is derived from the method's parameter
types, so the model is told exactly what the method accepts, and a contextual parameter the
method needs is summoned where the toolkit is constructed rather than exposed to the model:

```scala
object Broker:
  val quotes = Map(t"AAPL" -> 189.3, t"MSFT" -> 412.7)

  @ability
  @about(t"Look up the current price of a stock ticker")
  def price(ticker: Text): Double = quotes.at(ticker).or(0.0)

val broker = Toolkit(Broker)
broker.specs.map(_.name)   // List(t"price")
```

With a `Toolkit` given in scope, `ask` runs the whole loop: it offers the tools to the model, runs
the tool the model calls, reports the result, and continues until the model answers in text or
the iteration limit is reached. A tool that fails, or a call with malformed arguments, is
reported to the model as an error result it can recover from, rather than aborting the
conversation:

<!-- doccheck: skip -->
```scala
claude.session:
  given Toolkit = broker
  llm.ask(t"Compare AAPL and MSFT.")   // calls price twice, then answers
```

Every turn the loop makes is committed to the history, so the record shows the calls the model
made and what it was told. A loop that does not converge raises `Llm.Error` with the reason
`ToolLoopExceeded`, at the limit set by `iterating`. The `@about` annotation is shared with the
[MCP](mcp.md) integration, so one description serves a method exposed both as an MCP tool and
as a model ability.

### Structured answers

`elicit` asks for an answer of a given type. The type's JSON schema is derived and presented to
the model as the one tool it must call, and the arguments it supplies are decoded to the type —
so the result is a value, not text to be parsed, on every provider, including local servers that
have no native structured-output mode:

```scala
case class Verdict(ticker: Text, rating: Text, confidence: Double)
```

<!-- doccheck: skip -->
```scala
local.session:
  llm.elicit[Verdict](t"Summarize your recommendation for AAPL.")
```

A reply that ignores the forced tool, or arguments that do not decode, raise `Llm.Error` with
the reason `Malformed`, so a malformed answer is an error to handle rather than a value that
looks right.

### Messages and content

The vocabulary the dialects share is small. A `Llm.Message` has a `Role` — user or assistant —
and a list of `Llm.Content`: text, an image or document from inline bytes or a URL, a tool call
with its arguments, a tool result, the model's thinking where the provider exposes it, and an
`Opaque` block preserving verbatim any content a provider sends that is not otherwise modeled,
so nothing is lost in translation:

```scala
val question = Llm.Message(Llm.Role.User, t"How many bones are in the human hand?")

val illustrated = Llm.Message
  ( Llm.Role.User,
    List
      ( Llm.Content.Textual(t"What is in this picture?"),
        Llm.Content.Graphic(Llm.Content.Source.Remote(url"https://example.com/hand.png")) ) )
```

`ask` and `stream` accept a message as readily as text, which is how images and documents are
sent.

### Errors and retries

Every failure is a `Llm.Error` whose reason is one of a fixed set — `Unreachable`,
`Unauthorized`, `Invalid`, `NotFound`, `TooLarge`, `RateLimited`, `Overloaded`, `Malformed`,
`Interrupted`, `ToolLoopExceeded` — or `Provider`, carrying a code the provider reported that
maps to none of them, so the wire's own error is never lost. A rate limit or an overload is
retried automatically, honoring the delay the provider asks for, before it is raised:

<!-- doccheck: skip -->
```scala
recover:
  case Llm.Error(Llm.Error.Reason.Unauthorized, detail) => Out.println(t"check the API key")
  case Llm.Error(Llm.Error.Reason.TooLarge, detail)     => Out.println(t"shorten the conversation")
. protect:
    claude.session(llm.ask(t"Hello"))
```

### Counting tokens

Anthropic reports the token count of a prospective conversation without running it, which is
how a program decides whether a document fits before sending it:

<!-- doccheck: skip -->
```scala
claude.countTokens(List(question), system = t"Be terse.")   // the input tokens it would cost
```

### Testing without a provider

Because every provider is a dialect over an HTTP backend, a program that talks to a model is
tested by substituting the backend with one that returns scripted responses, and the dialect
tests in the library do exactly that: seventy-odd offline tests replay recorded replies, streamed
deltas and error envelopes through each dialect, and prove with compile-time checks that a
session or a live stream cannot escape its block. A program built on the same vocabulary inherits
that testability: the model is a value, and a value can be faked.
