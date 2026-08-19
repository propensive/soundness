                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package sibylline

import scala.caps
import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*, charDecoders.utf8Decoder, charEncoders.utf8Encoder,
    textSanitizers.strictSanitizer
import jacinta.*
import obligatory.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.{chunks, memoize, via}

object Llm:
  // The two turn-taking roles a conversation alternates between. The system prompt is not a
  // role: two of the four wire APIs carry it outside the message list, so it is a field of the
  // `Exchange` rather than a message.
  enum Role:
    case User, Assistant

  object Content:
    // Where binary content lives: inline bytes, rendered as base64 at the wire, or a URL the
    // provider fetches itself. Some wire APIs distinguish these structurally; the others render
    // whichever form as a (data-)URL. The base64 encoding happens in the dialects, not here.
    enum Source:
      case Inline(data: Data, mediaType: MediaType)
      case Remote(url: HttpUrl)

  // One block of a message's content, in the neutral vocabulary every dialect translates to and
  // from. `Opaque` carries any provider block the vocabulary does not model, verbatim: a dialect
  // replays its own provider's opaque blocks when history is re-sent, and drops another
  // provider's, so a transcript is never corrupted and never lies.
  enum Content:
    case Textual(text: Text)
    case Graphic(source: Content.Source)
    case Document(source: Content.Source)
    case ToolUse(id: Text, tool: Text, arguments: Json)
    case ToolResult(id: Text, content: List[Content], failure: Boolean = false)
    case Thinking(text: Text, signature: Optional[Text] = Unset)
    case Redacted(data: Text)
    case Opaque(provider: Text, json: Json)

  object Message:
    def apply(role: Role, text: Text): Message = Message(role, List(Content.Textual(text)))

  case class Message(role: Role, content: List[Content])

  // Why the model stopped emitting. Decoding is total: a stop reason this vocabulary does not
  // model becomes `Other`, and the code the provider sent is never lost. `ToolCall` is what the
  // tool loop dispatches on.
  enum Stop:
    case Ended
    case Exhausted
    case ToolCall
    case Refused
    case Sequence(text: Text)
    case Filtered(detail: Optional[Text])
    case Other(code: Text)

  // Token accounting. Only input and output counts are universal; the remainder are reported by
  // some providers only, so a session total folds them optionally: absent plus absent stays
  // absent, and a count is never invented.
  case class Usage
    ( input:      Int,
      output:     Int,
      cacheRead:  Optional[Int] = Unset,
      cacheWrite: Optional[Int] = Unset,
      reasoning:  Optional[Int] = Unset ):

    def + (that: Usage): Usage =
      def sum(left: Optional[Int], right: Optional[Int]): Optional[Int] =
        left.lay(right)(_ + right.or(0))

      Usage
        ( input + that.input,
          output + that.output,
          sum(cacheRead, that.cacheRead),
          sum(cacheWrite, that.cacheWrite),
          sum(reasoning, that.reasoning) )

  // One completed assistant turn: pure data, so it may leave the session block.
  case class Reply
    ( message: Message,
      stop:    Stop,
      usage:   Usage,
      model:   Optional[Text] = Unset,
      id:      Optional[Text] = Unset ):

    def text: Text =
      message.content.bind:
        case Content.Textual(text) => List(text)
        case _                     => List()

      . join

    def toolCalls: List[Content.ToolUse] =
      message.content.bind:
        case content: Content.ToolUse => List(content)
        case _                        => List()

  // Whether the model may, must, must not, or must specifically call a tool.
  enum ToolChoice:
    case Auto
    case Forbidden
    case Required
    case Named(tool: Text)

  // The request knobs every wire API expresses. Provider-specific knobs (thinking budgets,
  // caching, safety settings) are typed on the provider's own target value, not here.
  case class Settings
    ( maxTokens:     Optional[Int]        = Unset,
      temperature:   Optional[Double]     = Unset,
      topP:          Optional[Double]     = Unset,
      stopSequences: List[Text]           = List(),
      toolChoice:    Optional[ToolChoice] = Unset )

  // A tool the model may call: its name, what it does, and the JSON schema of its arguments.
  // The `Toolkit` macro produces these from typed Scala methods; the core only carries them.
  case class Tool(name: Text, description: Text, parameters: JsonSchema)

  // A pure description of one request: everything a dialect needs to encode a wire call.
  case class Exchange
    ( system:   Optional[Text],
      history:  List[Message],
      tools:    List[Tool],
      settings: Settings )

  object Event:
    // A fragment of an open content block. `Arguments` carries partial JSON text, complete only
    // when the block closes.
    enum Increment:
      case Textual(text: Text)
      case Thinking(text: Text)
      case Signature(text: Text)
      case Arguments(json: Text)

  // The neutral streaming vocabulary, index-addressed: every wire API's event stream reduces to
  // blocks that open, grow by increments, and close, with message-level updates alongside.
  enum Event:
    case Started(id: Optional[Text], model: Optional[Text])
    case Opened(index: Int, content: Content)
    case Delta(index: Int, increment: Event.Increment)
    case Closed(index: Int)
    case Update(stop: Optional[Stop], usage: Optional[Usage])
    case Finished

  // The seam a provider adapter implements: encoding an exchange to its wire form, making the
  // call, and translating the reply — or the stream of frames — back into the neutral
  // vocabulary. A dialect is constructed by its provider's `Sessional`, capturing the HTTP
  // capabilities and the caller's tactic; the session only ever speaks these two methods.
  trait Dialect:
    def name: Text
    def exchange(turn: Exchange): Reply
    def stream(turn: Exchange): Iterator[Event]^{this}

  object Error:
    // The numbers are the `SN-990.e` subcodes, and are frozen: codes added later append.
    enum Reason(val number: Int) extends Clarification:
      case Unreachable          extends Reason(1)
      case Unauthorized         extends Reason(2)
      case Invalid              extends Reason(3)
      case NotFound             extends Reason(4)
      case TooLarge             extends Reason(5)
      case RateLimited          extends Reason(6)
      case Overloaded           extends Reason(7)
      case Malformed            extends Reason(8)
      case Interrupted          extends Reason(9)
      case ToolLoopExceeded     extends Reason(10)
      case Provider(code0: Text) extends Reason(11)

    given communicable: Reason is Communicable =
      case Reason.Unreachable      => m"the provider could not be reached"
      case Reason.Unauthorized     => m"the API key was missing, invalid or expired"
      case Reason.Invalid          => m"the provider rejected the request as invalid"
      case Reason.NotFound         => m"the model or endpoint does not exist"
      case Reason.TooLarge         => m"the conversation exceeds the model's context window"
      case Reason.RateLimited      => m"the provider imposed a rate limit"
      case Reason.Overloaded       => m"the provider is temporarily overloaded"
      case Reason.Malformed        => m"the provider's reply could not be interpreted"
      case Reason.Interrupted      => m"the stream ended before the message was complete"
      case Reason.ToolLoopExceeded => m"the tool-use loop exceeded its iteration limit"
      case Reason.Provider(code)   => m"the provider reported the error code $code"

  // `status` is the HTTP status when there was one, and `retry` the `retry-after` delay in
  // seconds on a rate-limit or overload: the two fields a caller needs to decide whether and
  // when to try again.
  case class Error
    ( reason: Error.Reason,
      detail: Text,
      status: Optional[Int]  = Unset,
      retry:  Optional[Long] = Unset )
    ( using Diagnostics )
  extends fulminate.Error(990, reason.number)
    ( m"the model request failed because $reason: $detail" )

  // A free function of the companion, not a method: a method would carry its instance in its
  // prefix, and a lambda built from it would hide capabilities overlapping the tactic it is
  // applied to. `safely` keeps the inner decode's tactic pure, so no separation overlap arises.
  private[sibylline] def parsed(text: Text)(using Diagnostics): Json raises Error =
    import zephyrine.Buffering

    safely(text.read[Json]).or:
      abort(Error(Error.Reason.Malformed, t"the streamed tool arguments were not valid JSON"))

  // The shared HTTP engine: one retry policy for every dialect. Rate limits and overloads
  // (429, 503, 529) are retried a few times, honouring `retry-after` when the provider sends
  // one and backing off exponentially when it does not; any other failure is decoded through
  // the dialect's own error mapping and raised.
  private[sibylline] def fetch(fail: (Http.Status, Optional[Json]) => Error)
    ( send: => Http.Response )
    ( using Tactic[Error], Diagnostics )
  :   Http.Response =

    def attempt(remaining: Int, backoff: Long): Http.Response =
      val response = send

      if response.status.category == Http.Status.Category.Successful then response
      else response.status.code match
        case 429 | 503 | 529 if remaining > 0 =>
          val delay: Long =
            safely(response.headers.retryAfter.prim.let(_.as[Long])).or(backoff)

          // A plain blocking sleep: this module is JVM-only, the whole call is synchronous on
          // the caller's thread, and no `Monitor` is demanded of one-shot callers.
          Thread.sleep(delay*1000)
          attempt(remaining - 1, backoff*2)

        case _ =>
          abort(fail(response.status, body(response)))

    attempt(3, 1)

  // The response body as JSON, or `Unset` for a body that is not JSON at all — a crashed
  // gateway, an interposed proxy. The body's own stream, not `receive`, which consults the
  // status first and would abort with an `Http.Error` that has already discarded the envelope.
  private def body(response: Http.Response)(using Diagnostics): Optional[Json] =
    safely(response.body.stream.memoize.read[Text].as[Json])

  private[sibylline] def receive(response: Http.Response)(using Tactic[Error], Diagnostics)
  :   Json =

    body(response).lest(Error(Error.Reason.Malformed, t"the reply was not valid JSON"))

  // The response body as raw server-sent-event frames, one `Text` per event, decoded
  // incrementally off the live connection.
  private[sibylline] def frames(consume response: Http.Response)
    ( using tactic: Tactic[Error], diagnostics: Diagnostics )
  :   Iterator[Text]^ =

    given decodeTactic: (Tactic[CharDecoder.Error]^) = tactic.contramap: _ =>
      Error(Error.Reason.Malformed, t"the stream was not valid UTF-8")

    response.body.stream.via(summon[CharDecoder]).chunks.frames[Sse]

  private[sibylline] object Accumulator:
    // The in-progress form of one content block: the block as it was opened, the text it has
    // accumulated, and any auxiliary text (a thinking signature, or partial JSON arguments).
    private[sibylline] class Block(content0: Content):
      @scala.caps.unsafe.untrackedCaptures
      var content: Content = content0

      val text: StringBuilder = StringBuilder()
      val extra: StringBuilder = StringBuilder()

      @scala.caps.unsafe.untrackedCaptures
      var open: Boolean = true

  // The fold from a stream of events to a completed `Reply`. Every event the `Response` yields
  // passes through `absorb` exactly once, so partial consumption never loses the final message.
  // Mutable and single-owner: confined to its `Response`.
  private[sibylline] class Accumulator():
    private val blocks: scm.TreeMap[Int, Accumulator.Block] = scm.TreeMap()

    @scala.caps.unsafe.untrackedCaptures
    private var stop0: Optional[Stop] = Unset

    @scala.caps.unsafe.untrackedCaptures
    private var usage0: Optional[Usage] = Unset

    @scala.caps.unsafe.untrackedCaptures
    private var model0: Optional[Text] = Unset

    @scala.caps.unsafe.untrackedCaptures
    private var id0: Optional[Text] = Unset

    @scala.caps.unsafe.untrackedCaptures
    private var finished0: Boolean = false

    def absorb(event: Event): Unit = event match
      case Event.Started(id, model) =>
        id0 = id
        model0 = model

      case Event.Opened(index, content) =>
        blocks(index) = Accumulator.Block(content)

      case Event.Delta(index, increment) =>
        val block = blocks.getOrElseUpdate(index, Accumulator.Block(Content.Textual(t"")))

        increment match
          case Event.Increment.Textual(text)   => block.text.append(text.s)
          case Event.Increment.Thinking(text)  => block.text.append(text.s)
          case Event.Increment.Signature(text) => block.extra.append(text.s)
          case Event.Increment.Arguments(json) => block.extra.append(json.s)

      case Event.Closed(index) =>
        blocks.get(index).foreach(_.open = false)

      case Event.Update(stop, usage) =>
        stop.let(stop0 = _)
        usage.let: usage => usage0 = usage0.lay(usage)(_ + usage)

      case Event.Finished =>
        finished0 = true

    // Folds the accumulated blocks into the completed assistant message. An unclosed block or a
    // stream that ended without finishing raises `Interrupted`: the message would be a lie.
    def reply()(using Diagnostics): Reply raises Error =
      if !finished0 then abort(Error(Error.Reason.Interrupted, t"the stream ended early"))

      val content: List[Content] = List.of:
        blocks.values.toList.map: block =>
          if block.open
          then abort(Error(Error.Reason.Interrupted, t"a content block was never closed"))

          val accumulated: Text = block.text.toString.tt
          val auxiliary: Text = block.extra.toString.tt

          block.content match
            case Content.Textual(text) =>
              Content.Textual(t"$text$accumulated")

            case Content.Thinking(text, signature) =>
              Content.Thinking
                ( t"$text$accumulated",
                  if auxiliary == t"" then signature else auxiliary )

            case Content.ToolUse(id, tool, arguments) =>
              if auxiliary == t"" then Content.ToolUse(id, tool, arguments)
              else Content.ToolUse(id, tool, parsed(auxiliary))

            case other =>
              other

      Reply(Message(Role.Assistant, content), stop0.or(Stop.Ended), usage0.or(Usage(0, 0)),
            model0, id0)

  // The live, in-flight assistant turn: it borrows the session whose history it will commit to,
  // and (through the dialect's iterator) the connection it is reading, so capture checking
  // confines it to the session's block. Draining it — directly, or via `reply()` — is what
  // commits the turn; an abandoned or failed stream commits nothing, so a retry re-asks from
  // the previous turn boundary.
  class Response private[sibylline]
    ( session: Session^, message: Message, source: Iterator[Event]^ )
    ( using tactic: Tactic[Error], diagnostics: Diagnostics )
  extends caps.ExclusiveCapability:

    private val accumulator: Accumulator = Accumulator()

    @scala.caps.unsafe.untrackedCaptures
    private var reply0: Optional[Reply] = Unset

    private val tracked: Iterator[Event]^{source} =
      source.map: event =>
        accumulator.absorb(event)
        event

    // The full event stream, once: every event pulled here also feeds the accumulator.
    def events: Iterator[Event]^{this} = tracked

    // Just the text deltas, for `response.text.each(Out.print(_))`.
    def text: Iterator[Text]^{this} = tracked.collect:
      case Event.Delta(_, Event.Increment.Textual(text)) => text

    // Drains whatever remains, folds the accumulator into the assistant message, commits the
    // user and assistant turns to the session's history atomically, and returns pure data.
    // Idempotent: a second call returns the same `Reply` without touching the stream.
    def reply(): Reply =
      reply0.or:
        while tracked.hasNext do tracked.next()
        val result = accumulator.reply()
        session.commit(message, result)
        reply0 = result
        result

  // A conversation: the one mutable holder of its history and usage totals. Exclusive and
  // stateful: `ask`, `stream` and `record` are `update` methods, and a streamed `Response`
  // borrows the session, so separation checking statically forbids interleaving a new turn
  // while a stream is undrained — history cannot fork mid-turn.
  class Session private[sibylline]
    ( dialect:  Dialect^,
      system:   Optional[Text],
      tools:    List[Tool],
      settings: Settings,
      priming:  List[Message] )
    ( using tactic: Tactic[Error], diagnostics: Diagnostics )
  extends caps.ExclusiveCapability, caps.Stateful:

    @scala.caps.unsafe.untrackedCaptures
    private var history0: List[Message] = priming

    @scala.caps.unsafe.untrackedCaptures
    private var usage0: Usage = Usage(0, 0)

    // Pure reads: their results may leave the session block.
    def history: List[Message] = history0
    def usage: Usage = usage0

    private def exchange(message: Message): Exchange =
      Exchange(system, List.of(history0.stdlib :+ message), tools, settings)

    // Seed or amend the history without a round trip: replaying a transcript, or a tool loop
    // recording synthesized turns.
    update def record(message: Message): Unit =
      history0 = List.of(history0.stdlib :+ message)

    update def ask(text: Text): Reply = ask(Message(Role.User, text))

    // One full, non-streamed round trip: the user turn and the assistant's reply are committed
    // to history together, after the call has succeeded.
    update def ask(message: Message): Reply =
      val reply = dialect.exchange(exchange(message))
      commit(message, reply)
      reply

    update def stream(text: Text): Response^{this, caps.any} = stream(Message(Role.User, text))

    // A streamed turn: nothing is committed until the returned `Response` is drained. The
    // response borrows the session it will commit to *and* reads a stream from the session's own
    // dialect — self-aliasing by design, which the checker reads as a clash between the handle
    // and the iterator. The one rim in this file; both are delivered into a single value whose
    // `^{this, caps.any}` result ties them back to this session.
    update def stream(message: Message): Response^{this, caps.any} =
      caps.unsafe.unsafeAssumeSeparate:
        Response(this, message, dialect.stream(exchange(message)))

    private[sibylline] update def commit(message: Message, reply: Reply): Unit =
      history0 = List.of(history0.stdlib :+ message :+ reply.message)
      usage0 = usage0 + reply.usage
