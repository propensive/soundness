## Language Models

### About

A conversation with a large language model is typed at every step. The provider and the model
are values. A conversation is a [session] which accumulates its history and its usage of tokens.
A reply is a value which says why the model stopped. Streamed output is pulled off the live
connection. Tools are ordinary Scala methods which the model may call. A structured answer is
decoded to a case class, against a schema derived from it.

One vocabulary serves Anthropic's Messages API, OpenAI's Chat Completions and Responses APIs,
any server compatible with the former, such as vLLM, Ollama or Groq, and Google's Gemini.

### On language models

The API of each provider is a JSON dialect: a request with the model, the messages so far and a
handful of tuning knobs; a reply with content blocks, a stop reason and token counts; a streaming
variant which sends deltas; and a protocol for tool calls, which returns arguments as JSON for
the program to act on and report back. The dialects differ in every detail and agree in every
concept, and the concepts are what a program cares about.

The concepts are modeled once here, as messages, replies, usage, tools and stop reasons, and
translated onto each wire by a *dialect* for each provider. A conversation is a session which
lives in a block. The pure values it produces leave freely, while the session itself, and any
stream in flight, belong to the block, so a connection is not held open by accident.

Everything comes from the `soundness` package, with the HTTP backend, network access and logging
which a client needs:

```scala fuel
feature experimental.captureChecking
##
import soundness.*
import errorDiagnostics.stackTracesDiagnostics
import stdios.javaLangSystemStdio
import termcapDefinitions.basicTermcap
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging
import strategies.throwUnsafely
import codepages.utf8Codepage
```

The samples on this page call a provider over the network, so they are compiled but not run.

### Providers and models

A provider value names the model and carries the API key. Its tuning is applied by methods which
each return a new value, so a configured model is a plain [immutable] value which can be shared
and reused:

```scala
val key = t"sk-ant-…"

val claude = Anthropic(t"claude-sonnet-4-5", key).prompted(t"Be terse.").limit(1024)
val gpt = OpenAI(t"gpt-5", t"sk-…").warmth(0.2)
val gemini = Gemini(t"gemini-2.5-pro", t"AIza…")
val local = OpenAI.compatible(url"http://localhost:11434/v1", t"llama3")
```

Each method sets one knob:

```tabular
column  Method
column  Sets
row  `prompted`  the system prompt
row  `limit`  the maximum output tokens
row  `warmth`  the temperature
row  `sampling`  the nucleus threshold
row  `stopping`  the stop sequences
row  `iterating`  the tool-loop limit
row  `primed`  a history to start from
row  `on`  an alternative base URL
```

A knob left unset is omitted from the request, and is not sent as a default, so the provider's
own defaults apply. An OpenAI-compatible server takes an optional key. OpenAI's Responses API is
reached from an `OpenAI` value with `responses`.

### A conversation

A session is opened on a provider with `session`, and inside the block the `llm` accessor reaches
it. `ask` sends a message and returns the model's `Reply`. The session records both, so a second
`ask` continues the conversation:

```scala fuel
typecheck
##
val reply: Llm.Reply = claude.session:
  llm.ask(t"What is the tallest mountain in Estonia?")
  llm.ask(t"And how tall is it?")

val answer: Text = reply.text
val stop: Llm.Stop = reply.stop
val usage: Llm.Usage = reply.usage
```

`text` is the assistant's answer. `stop` is `Llm.Stop.Ended`, or the reason the model stopped
early. `usage` holds the tokens which the last turn cost.

A `Reply` is pure data: its message, its stop reason, its usage, and the model and identifier
which the provider reported. It may therefore leave the block, as may a snapshot of
`llm.history` or the folded `llm.usage`. The session itself belongs to the block. The connection
closes when the block ends, so the session must not be kept beyond it.

### Streaming

`stream` sends a message and returns a `Response`. Its `text` is an [iterator](collections.md)
of fragments, pulled synchronously off the connection as they arrive, and its `events` are the
finer-grained deltas. When the stream has been consumed, `reply()` assembles the complete reply
and commits the turn to the history. It does so even after partial consumption, since the
remainder is read on demand. A stream which is abandoned commits nothing, so a failed turn is
simply retried:

```scala fuel
typecheck
##
claude.session:
  val response = llm.stream(t"Write a limerick about Tallinn.")
  response.text.each(Out.print(_))
  response.reply()
```

The response, like the session, is confined to the block.

### Tools

A tool is a method annotated `@ability` and described with `@about`, on an object which a
`Toolkit` is built from. The JSON schema for the parameters of each tool is derived from the
parameter types of the method, so the model is told exactly what the method accepts. A contextual
parameter which the method needs is summoned where the toolkit is constructed, and is not
exposed to the model:

```scala
object Broker:
  val quotes = Map(t"AAPL" -> 189.3, t"MSFT" -> 412.7)

  @ability
  @about(t"Look up the current price of a stock ticker")
  def price(ticker: Text): Double = quotes.at(ticker).or(0.0)

val broker = Toolkit(Broker)
```

```scala fuel
run
##
broker.specs.each(spec => Out.println(spec.name))
```

With a `Toolkit` given in scope, `ask` runs the whole loop. It offers the tools to the model,
runs the tool which the model calls, reports the result, and continues until the model answers
in text or the iteration limit is reached. A tool which fails, or a call with malformed
arguments, is reported to the model as an error result which it can recover from, and does not
abort the conversation:

```scala fuel
typecheck
##
claude.session:
  given Toolkit = broker
  llm.ask(t"Compare AAPL and MSFT.")
```

Here the model would call `price` twice, and then answer. Every turn which the loop makes is
committed to the history, so the record shows the calls the model made and what it was told. A
loop which does not converge raises `Llm.Error` with the reason `ToolLoopExceeded`, at the limit
set by `iterating`. The `@about` annotation is shared with [MCP](mcp.md), so one description
serves a method exposed both as an MCP tool and as a model ability.

### Structured answers

`solicit` asks for an answer of a given type. The JSON schema of the type is derived, and the
document which the model supplies is decoded to the type. The result is a value, not text to be
parsed. How the schema reaches the model is the business of the dialect. Anthropic's wire
constrains the reply to it natively, and elsewhere it is presented as the one tool which the
model must call. The caller names a type and receives a value, on every provider, including
local servers which have no mode for structured output:

```scala
case class Verdict(ticker: Text, rating: Text, confidence: Double)
```

```scala fuel
typecheck
##
local.session:
  llm.solicit[Verdict](t"Summarize your recommendation for AAPL.")
```

A reply which ignores the forced tool, or a document which does not decode, raises `Llm.Error`
with the reason `Malformed`. A malformed answer is an error to handle, and not a value which looks
right.

### Batches

Many questions which need no answer today go through Anthropic's Message Batches API, which runs
them asynchronously at half the price. `elicitAll` takes a list of requests, each a prompt with
an identifier of the caller's choosing, checked by `Llm.Id.request` before anything is sent. It
returns a batch: a pure record of its identifier, status and counts, whose answers are decoded to
the named type once it has ended:

```scala fuel
typecheck
##
val requests = List
  ( Llm.Request(Llm.Id.request(t"aapl"), t"Summarize your recommendation for AAPL."),
    Llm.Request(Llm.Id.request(t"msft"), t"Summarize your recommendation for MSFT.") )

val batch = claude.prompted(t"Be terse.").elicitAll[Verdict](requests)

batch.await().outcomes().each: outcome =>
  outcome.result match
    case verdict: Verdict => Out.println(t"${outcome.id}: ${verdict.rating}")
    case error: Llm.Error => Out.println(t"${outcome.id} failed: ${error.message}")
```

Outcomes arrive in the provider's order, not the order of submission, which is why each carries
the identifier of its request. A batch survives the process which submitted it.
`claude.batch[Verdict](id)` finds it again by its identifier, with its current status.
Identifiers are typed by what they name, as an `Llm.Id[Llm.Request]`, an `Llm.Id[Llm.Reply]` or
an `Llm.Id[Anthropic.Batch[?]]`, so the identifier of a reply can never be handed to a method
which expects a batch's.

A prompt shared by every request in a batch, or by every turn of a long session, is worth caching
at the provider. `claude.caching` marks the system prompt as a cache breakpoint, and
`Llm.Usage.cacheRead` reports how much of each request was read from the cache.

### Messages and content

The vocabulary which the dialects share is small. An `Llm.Message` has a `Role`, which is user
or assistant, and a list of `Llm.Content`, so nothing is lost in translation:

```tabular
column  Content
column  Holds
row  `Textual`  text
row  `Graphic`  an image, from inline bytes or a URL
row  `Document`  a document, from inline bytes or a URL
row  `ToolUse`  a tool call, with its arguments
row  `ToolResult`  a tool result
row  `Thinking`  the model's thinking, where the provider exposes it
row  `Redacted`  thinking the provider returns only in encrypted form
row  `Opaque`  any content a provider sends that is not otherwise modeled, verbatim
```

An image or a document comes from an `Llm.Content.Source`: `Inline` bytes with a media type, or
a `Remote` URL:

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

Every failure is an `Llm.Error` whose reason is one of a fixed set, or `Provider`, which carries
a code the provider reported which maps to none of them, so the wire's own error is never lost. A
rate limit or an overload is retried automatically, up to three times and honoring the delay
which the provider asks for, before it is raised:

```tabular
column  Reason
column  Meaning
row  `Unreachable`  the provider could not be reached
row  `Unauthorized`  the API key was missing, invalid or expired
row  `Invalid`  the provider rejected the request as invalid
row  `NotFound`  the model or endpoint does not exist
row  `TooLarge`  the conversation exceeds the model's context window
row  `RateLimited`  the provider imposed a rate limit; retried first
row  `Overloaded`  the provider is temporarily overloaded; retried first
row  `Malformed`  the provider's reply could not be interpreted
row  `Interrupted`  the stream ended before the message was complete
row  `ToolLoopExceeded`  the tool-use loop exceeded its iteration limit
row  `Provider`  a code the provider reported that maps to none of the others
```

A handler matches on the reason:

```scala fuel
typecheck
##
recover:
  case error: Llm.Error => error.reason match
    case Llm.Error.Reason.Unauthorized => Out.println(t"check the API key")
    case Llm.Error.Reason.TooLarge     => Out.println(t"shorten the conversation")
    case _                             => Out.println(error.detail)
. protect:
    claude.session(llm.ask(t"Hello"))
```

### Counting tokens

Anthropic reports the token count of a prospective conversation without running it, which is how
a program decides whether a document fits before sending it:

```scala fuel
typecheck
##
val tokens: Int = claude.countTokens(List(question), system = t"Be terse.")
```

### Testing without a provider

Every provider is a dialect over an HTTP backend, so a program which talks to a model is tested
by substituting a backend which returns scripted responses. The dialect tests in the library do
exactly that. Seventy-odd offline tests replay recorded replies, streamed deltas and error
envelopes through each dialect, and prove with compiletime checks that a session or a live
stream cannot escape its block. A program built on the same vocabulary inherits that
testability. The model is a value, and a value can be faked.

### Related

- [MCP](mcp.md) exposes the same annotated methods as tools for any assistant.
- [ACP](acp.md) drives a coding agent, instead of a model directly.
- [HTTP client](http-client.md) describes the backend which every dialect speaks over.
- [JSON](json.md#schemas) describes the schemas derived for tools and structured answers.
- [Errors](errors.md) describes `recover` and the other strategies which meet an `Llm.Error`.
