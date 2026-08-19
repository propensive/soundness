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

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*, charEncoders.utf8Encoder
import jacinta.*, formatting.compactJsonFormatting, dynamicJsonAccess.enabled
import monotonous.*, alphabets.base64Standard
import obligatory.*
import prepositional.*
import rudiments.*
import spectacular.*
import telekinesis.*, postables.jsonPostable
import urticose.*
import vacuous.*

object Anthropic:
  // The wire headers the Messages API takes, named so that `xApiKey = key` renders as
  // `x-api-key`. Anchored here so that using `Anthropic` needs no imports.
  given xApiKey: ("xApiKey" is Directive of Text) = identity(_)
  given anthropicVersion: ("anthropicVersion" is Directive of Text) = identity(_)
  given anthropicBeta: ("anthropicBeta" is Directive of Text) = identity(_)

  // Not a case class, and this `apply` ascribes its result: a case class's synthetic `apply`
  // returns a type refined with each argument's singleton, and no `Sessional` instance matches
  // a refinement of its `Self`, so `Anthropic(…).session` would not resolve at any call site.
  def apply(model: Text, key: Text): Anthropic =
    new Anthropic
      ( model, key, url"https://api.anthropic.com", t"2023-06-01", Unset, Unset, Llm.Settings(),
        List(), List() )

  // A named instance class rather than an anonymous given: an anonymous subclass would freshen
  // the capability types in its inferred `Result` member.
  class Sessional
    ( using online:      Online,
            backend:     Http.Backend,
            loggable:    (Http.Event is Loggable)^,
            tactic:      Tactic[Llm.Error],
            diagnostics: Diagnostics )
  extends aperture.Sessional:
    type Self = Anthropic

    // A fresh capability (`^`, not `^{caps.any}`): each `session` call's handle is its own
    // existential, so returning it (or anything capturing it) from the block is a level
    // violation the capture checker rejects.
    type Result = Llm.Session^

    def session[result](target: Anthropic)(lambda: (session: Result) ?=> result): result =
      // Nothing needs tearing down when the scope ends: the Messages API is stateless per
      // request, so the session is only memory, reclaimed with the handle.
      // The dialect is laundered pure at this one boundary, as `LspSessional.exchange` does
      // with its connection: it is built from the same tactic and HTTP capabilities the
      // session itself holds, so tracking it doubles every capability into an overlap, while
      // confinement is already guaranteed by the session's fresh `Result` capability, which
      // the block cannot leak. Passed directly rather than bound to a `val`: a binding would
      // demote the stateful handle to a read-only alias `Session^{any}` cannot subsume.
      lambda
        ( using Llm.Session
            ( caps.unsafe.unsafeAssumePure(AnthropicDialect(target)), target.system,
                target.tools, target.settings, target.priming ) )

  given sessional
  :   ( online:      Online,
        backend:     Http.Backend,
        loggable:    (Http.Event is Loggable)^,
        tactic:      Tactic[Llm.Error],
        diagnostics: Diagnostics )
  =>  ( Sessional^{online, loggable, tactic, caps.any} ) =

    Sessional()

  private[sibylline] object Payload:
    given encodable: Payload is Json.Encodable = Json.EncodableDerivation.derived

  // The request body, spelled exactly as the wire wants it: absent `Optional` fields are
  // omitted from the JSON entirely, which is what the API requires of unset knobs.
  private[sibylline] case class Payload
    ( model:          Text,
      messages:       List[Json],
      max_tokens:     Int,
      system:         Optional[Text]       = Unset,
      temperature:    Optional[Double]     = Unset,
      top_p:          Optional[Double]     = Unset,
      stop_sequences: Optional[List[Text]] = Unset,
      stream:         Optional[Boolean]    = Unset,
      tools:          Optional[List[Json]] = Unset,
      tool_choice:    Optional[Json]       = Unset,
      thinking:       Optional[Json]       = Unset )

  private[sibylline] object Tokens:
    // Sealed: the optional-field decodable takes the tactic both directly and inside its
    // decodable thunk, which separation checking reads as overlapping uses of one capability.
    given decodable: Tactic[Json.Error] => Tokens is Json.Decodable =
      caps.unsafe.unsafeAssumeSeparate(Json.DecodableDerivation.derived[Tokens])

    def usage(tokens: Tokens): Llm.Usage =
      Llm.Usage
        ( tokens.input_tokens.or(0), tokens.output_tokens.or(0),
          tokens.cache_read_input_tokens, tokens.cache_creation_input_tokens )

  // The `usage` object, on replies and on `message_start`/`message_delta` stream events, with
  // every field optional: `message_delta` reports only `output_tokens`.
  private[sibylline] case class Tokens
    ( input_tokens:                Optional[Int] = Unset,
      output_tokens:               Optional[Int] = Unset,
      cache_read_input_tokens:     Optional[Int] = Unset,
      cache_creation_input_tokens: Optional[Int] = Unset )

  // Free functions of the companion, each a single decode of a single value against a single
  // tactic, sealed with `unsafeAssumeSeparate`: `Json#as` takes the `Tactic[Json.Error]` both
  // directly and inside the capture set of the decodable it summons, so a *capability* tactic —
  // which is what `contramap` produces — reads as two overlapping uses.
  private def text(json: Json): Text raises Json.Error =
    caps.unsafe.unsafeAssumeSeparate(json.as[Text])

  private def integer(json: Json): Int raises Json.Error =
    caps.unsafe.unsafeAssumeSeparate(json.as[Int])

  private def list(json: Json): List[Json] raises Json.Error =
    caps.unsafe.unsafeAssumeSeparate(json.as[List[Json]])

  private def tokens(json: Json): Tokens raises Json.Error =
    caps.unsafe.unsafeAssumeSeparate(json.as[Tokens])

  private[sibylline] def frame(text: Text): Sse raises Sse.Error =
    caps.unsafe.unsafeAssumeSeparate(text.as[Sse])

  // How the wire spells each neutral stop reason, decoded totally: an unrecognized reason
  // becomes `Other` and the code the API sent is never lost.
  private[sibylline] def stop(code: Text, sequence: Optional[Text]): Llm.Stop = code match
    case t"end_turn"      => Llm.Stop.Ended
    case t"max_tokens"    => Llm.Stop.Exhausted
    case t"tool_use"      => Llm.Stop.ToolCall
    case t"refusal"       => Llm.Stop.Refused
    case t"stop_sequence" => Llm.Stop.Sequence(sequence.or(t""))
    case other            => Llm.Stop.Other(other)

  // One neutral content block, encoded as the wire wants it — or `Unset` for another
  // provider's opaque block, which this dialect cannot honestly replay.
  private[sibylline] def encode(content: Llm.Content): Optional[Json] = content match
    case Llm.Content.Textual(text) =>
      Json.make(`type` = t"text".in[Json], text = text.in[Json])

    case Llm.Content.Graphic(source) =>
      Json.make(`type` = t"image".in[Json], source = origin(source))

    case Llm.Content.Document(source) =>
      Json.make(`type` = t"document".in[Json], source = origin(source))

    case Llm.Content.ToolUse(id, tool, arguments) =>
      Json.make
        ( `type` = t"tool_use".in[Json], id = id.in[Json], name = tool.in[Json],
          input  = arguments )

    case Llm.Content.ToolResult(id, content, failure) =>
      Json.make
        ( `type`      = t"tool_result".in[Json],
          tool_use_id = id.in[Json],
          content     = content.bind { block => encode(block).let(List(_)).or(List()) }.in[Json],
          is_error    = (if failure then failure else Unset).in[Json] )

    case Llm.Content.Thinking(text, signature) =>
      Json.make
        ( `type`    = t"thinking".in[Json], thinking = text.in[Json],
          signature = signature.in[Json] )

    case Llm.Content.Redacted(data) =>
      Json.make(`type` = t"redacted_thinking".in[Json], data = data.in[Json])

    case Llm.Content.Opaque(provider, json) =>
      if provider == t"anthropic" then json else Unset

  private def origin(source: Llm.Content.Source): Json = source match
    case Llm.Content.Source.Inline(data, mediaType) =>
      Json.make
        ( `type`     = t"base64".in[Json], media_type = mediaType.show.in[Json],
          data       = data.serialize[Base64].in[Json] )

    case Llm.Content.Source.Remote(url) =>
      Json.make(`type` = t"url".in[Json], url = url.show.in[Json])

  private[sibylline] def message(message: Llm.Message): Json =
    val role = message.role match
      case Llm.Role.User      => t"user"
      case Llm.Role.Assistant => t"assistant"

    val content = message.content.bind: block => encode(block).let(List(_)).or(List())
    Json.make(role = role.in[Json], content = content.in[Json])

  private[sibylline] def choice(choice: Llm.ToolChoice): Json = choice match
    case Llm.ToolChoice.Auto        => Json.make(`type` = t"auto".in[Json])
    case Llm.ToolChoice.Forbidden   => Json.make(`type` = t"none".in[Json])
    case Llm.ToolChoice.Required    => Json.make(`type` = t"any".in[Json])

    case Llm.ToolChoice.Named(tool) =>
      Json.make(`type` = t"tool".in[Json], name = tool.in[Json])

  private[sibylline] def tool(tool: Llm.Tool): Json =
    Json.make
      ( name         = tool.name.in[Json],
        description  = tool.description.in[Json],
        input_schema = tool.parameters.in[Json] )

  // One wire content block, decoded into the neutral vocabulary. A block this vocabulary does
  // not model is preserved verbatim as `Opaque`, so a transcript never lies.
  private[sibylline] def block(json: Json)(using Diagnostics): Llm.Content raises Json.Error =
    safely(text(json.`type`)).or(t"") match
      case t"text"     => Llm.Content.Textual(text(json.text))
      case t"tool_use" => Llm.Content.ToolUse(text(json.id), text(json.name), json.input)

      case t"thinking" =>
        Llm.Content.Thinking(text(json.thinking), safely(text(json.signature)))

      case t"redacted_thinking" => Llm.Content.Redacted(text(json.data))
      case _                    => Llm.Content.Opaque(t"anthropic", json)

  // The whole non-streamed reply.
  private[sibylline] def reply(json: Json)(using Diagnostics): Llm.Reply raises Json.Error =
    Llm.Reply
      ( Llm.Message(Llm.Role.Assistant, list(json.content).map(block(_))),
        stop(safely(text(json.stop_reason)).or(t"end_turn"), safely(text(json.stop_sequence))),
        Tokens.usage(tokens(json.usage)),
        safely(text(json.model)),
        safely(text(json.id)) )

  // One SSE frame, translated into neutral events. `message_start` carries the input-token
  // count and `message_delta` the *cumulative* output count, so usage is emitted as input-only
  // and output-only updates, which the accumulator sums without double-counting.
  private[sibylline] def events(sse: Sse)(using Tactic[Llm.Error], Diagnostics): List[Llm.Event] =
    given jsonTactic: (Tactic[Json.Error]^) = summon[Tactic[Llm.Error]].contramap: _ =>
      Llm.Error(Llm.Error.Reason.Malformed, t"a stream event had an unexpected shape")

    val json: Json = Llm.parsed(sse.data.stdlib.join(t"\n"))

    sse.event match
      case t"message_start" =>
        val usage = safely(tokens(json.message.usage)).let(Tokens.usage(_))

        List
          ( Llm.Event.Started(safely(text(json.message.id)), safely(text(json.message.model))),
            Llm.Event.Update(Unset, usage.let(_.copy(output = 0))) )

      case t"content_block_start" =>
        List(Llm.Event.Opened(integer(json.index), block(json.content_block)))

      case t"content_block_delta" =>
        val index = integer(json.index)

        val increment = text(json.delta.`type`) match
          case t"text_delta"      => Llm.Event.Increment.Textual(text(json.delta.text))

          case t"input_json_delta" =>
            Llm.Event.Increment.Arguments(text(json.delta.partial_json))

          case t"thinking_delta"  => Llm.Event.Increment.Thinking(text(json.delta.thinking))
          case t"signature_delta" => Llm.Event.Increment.Signature(text(json.delta.signature))
          case other              => Llm.Event.Increment.Textual(t"")

        List(Llm.Event.Delta(index, increment))

      case t"content_block_stop" =>
        List(Llm.Event.Closed(integer(json.index)))

      case t"message_delta" =>
        val stopped =
          safely(text(json.delta.stop_reason))
          . let(stop(_, safely(text(json.delta.stop_sequence))))

        val usage = safely(tokens(json.usage)).let(Tokens.usage(_)).let(_.copy(input = 0))
        List(Llm.Event.Update(stopped, usage))

      case t"message_stop" => List(Llm.Event.Finished)
      case t"ping"         => List()

      case t"error" =>
        abort(failure(Http.Status.InternalServerError, json))

      case other => List()

  // The wire error envelope, `{"type": "error", "error": {"type": …, "message": …}}`, mapped
  // totally onto `Llm.Error`: an unrecognized error type keeps its code under `Provider`.
  private[sibylline] def failure(status: Http.Status, json: Optional[Json])(using Diagnostics)
  :   Llm.Error =

    val kind: Optional[Text] = json.let: json => safely(text(json.error.`type`))

    val detail: Text =
      json.let { json => safely(text(json.error.message)) }.or(t"the request failed")

    val reason = kind match
      case t"authentication_error"  => Llm.Error.Reason.Unauthorized
      case t"permission_error"      => Llm.Error.Reason.Unauthorized
      case t"invalid_request_error" => Llm.Error.Reason.Invalid
      case t"not_found_error"       => Llm.Error.Reason.NotFound
      case t"request_too_large"     => Llm.Error.Reason.TooLarge
      case t"rate_limit_error"      => Llm.Error.Reason.RateLimited
      case t"overloaded_error"      => Llm.Error.Reason.Overloaded
      case code: Text               => Llm.Error.Reason.Provider(code)

      case _ => status.code match
        case 401 | 403 => Llm.Error.Reason.Unauthorized
        case 404       => Llm.Error.Reason.NotFound
        case 413       => Llm.Error.Reason.TooLarge
        case 429       => Llm.Error.Reason.RateLimited
        case 503 | 529 => Llm.Error.Reason.Overloaded
        case _         => Llm.Error.Reason.Invalid

    Llm.Error(reason, detail, status.code)

// The provider target: a pure value — no connection, no session state — so it may be a `val`,
// shared and reused across sessions. `Anthropic(model, key).session: session ?=> …` opens one.
class Anthropic private
  ( val model:    Text,
    key:          Text,
    base:         HttpUrl,
    version:      Text,
    beta:         Optional[Text],
    val system:   Optional[Text],
    val settings: Llm.Settings,
    val priming:  List[Llm.Message],
    val tools:    List[Llm.Tool] )
:

  private def copy
    ( system:   Optional[Text]    = system,
      settings: Llm.Settings      = settings,
      priming:  List[Llm.Message] = priming,
      tools:    List[Llm.Tool]    = tools,
      base:     HttpUrl           = base,
      version:  Text              = version,
      beta:     Optional[Text]    = beta )
  :   Anthropic =

    new Anthropic(model, key, base, version, beta, system, settings, priming, tools)

  def prompted(system: Text): Anthropic = copy(system = system)
  def limit(maxTokens: Int): Anthropic = copy(settings = settings.copy(maxTokens = maxTokens))

  def warmth(temperature: Double): Anthropic =
    copy(settings = settings.copy(temperature = temperature))

  def sampling(topP: Double): Anthropic = copy(settings = settings.copy(topP = topP))

  def stopping(sequences: Text*): Anthropic =
    copy(settings = settings.copy(stopSequences = List(sequences*)))

  def iterating(limit: Int): Anthropic = copy(settings = settings.copy(iterations = limit))
  def primed(messages: List[Llm.Message]): Anthropic = copy(priming = messages)
  def tooled(tools: List[Llm.Tool]): Anthropic = copy(tools = tools)
  def on(base: HttpUrl): Anthropic = copy(base = base)
  def versioned(version: Text): Anthropic = copy(version = version)
  def trying(beta: Text): Anthropic = copy(beta = beta)

  // Built structurally from the base URL's origin rather than by parsing text, so addressing
  // an endpoint cannot fail and no `UrlError` reaches the caller.
  private[sibylline] def address(path: Text): HttpUrl =
    Url(base.origin, t"${base.location}/$path")

  private[sibylline] def payload(turn: Llm.Exchange, streaming: Boolean): Json =
    val stops = turn.settings.stopSequences
    val tools = turn.tools.map(Anthropic.tool(_))

    Anthropic.Payload
      ( model          = model,
        messages       = turn.history.map(Anthropic.message(_)),
        max_tokens     = turn.settings.maxTokens.or(4096),
        system         = turn.system,
        temperature    = turn.settings.temperature,
        top_p          = turn.settings.topP,
        stop_sequences = if stops.stdlib.isEmpty then Unset else stops,
        stream         = if streaming then true else Unset,
        tools          = if turn.tools.stdlib.isEmpty then Unset else tools,
        tool_choice    = turn.settings.toolChoice.let(Anthropic.choice(_)) )

    . in[Json]

  // The `count_tokens` endpoint: how much of the context window a prospective exchange would
  // spend, without spending it.
  def countTokens(messages: List[Llm.Message], system: Optional[Text] = Unset)
    ( using online:      Online,
            backend:     Http.Backend,
            loggable:    (Http.Event is Loggable)^,
            tactic:      Tactic[Llm.Error],
            diagnostics: Diagnostics )
  :   Int =

    given connectTactic: (Tactic[Connect.Error]^) =
      summon[Tactic[Llm.Error]].contramap: _ =>
        Llm.Error(Llm.Error.Reason.Unreachable, t"the provider could not be reached")

    val body =
      Json.make
        ( model    = model.in[Json],
          messages = messages.map(Anthropic.message(_)).in[Json],
          system   = system.in[Json] )

    // The send thunk captures the same tactic `fetch` raises through — the one overlap every
    // retrying call shares; each is a single synchronous round trip.
    val response =
      caps.unsafe.unsafeAssumeSeparate:
        Llm.fetch(Anthropic.failure(_, _)):
          submit(address(t"v1/messages/count_tokens"), body)

    safely(Anthropic.integer(Llm.receive(response).input_tokens)).lest:
      Llm.Error(Llm.Error.Reason.Malformed, t"the token count was missing from the reply")

  private[sibylline] def submit(endpoint: HttpUrl, body: Json)
    ( using Online, Http.Backend, (Http.Event is Loggable)^, Tactic[Connect.Error] )
  :   Http.Response =

    import Anthropic.{xApiKey, anthropicVersion, anthropicBeta}

    beta.lay(endpoint.submit(Http.Post, xApiKey = key, anthropicVersion = version)(body)):
      beta =>
        endpoint.submit
          ( Http.Post, xApiKey = key, anthropicVersion = version, anthropicBeta = beta )
          ( body )

// The `Llm.Dialect` for the Messages API: constructed per session by the `Sessional`,
// capturing the HTTP capabilities and the caller's tactic, so the session itself never
// touches HTTP.
private[sibylline] class AnthropicDialect(target: Anthropic)
  ( using online:      Online,
          backend:     Http.Backend,
          loggable:    (Http.Event is Loggable)^,
          tactic:      Tactic[Llm.Error],
          diagnostics: Diagnostics )
extends Llm.Dialect, caps.ExclusiveCapability:

  def name: Text = t"anthropic"

  private def endpoint: HttpUrl = target.address(t"v1/messages")

  private given connectTactic: (Tactic[Connect.Error]^) = tactic.contramap: _ =>
    Llm.Error(Llm.Error.Reason.Unreachable, t"the provider could not be reached")

  private given jsonTactic: (Tactic[Json.Error]^) = tactic.contramap: _ =>
    Llm.Error(Llm.Error.Reason.Malformed, t"the reply had an unexpected shape")

  private given sseTactic: (Tactic[Sse.Error]^) = tactic.contramap: _ =>
    Llm.Error(Llm.Error.Reason.Malformed, t"a server-sent event was not valid")

  def exchange(turn: Llm.Exchange): Llm.Reply =
    // As in `countTokens`: the send thunk captures the tactic `fetch` raises through.
    val response =
      caps.unsafe.unsafeAssumeSeparate:
        Llm.fetch(Anthropic.failure(_, _)):
          target.submit(endpoint, target.payload(turn, streaming = false))

    Anthropic.reply(Llm.receive(response))

  def stream(turn: Llm.Exchange): Iterator[Llm.Event]^{this} =
    // As in `exchange`: the send thunk captures the tactic `fetch` raises through.
    val response =
      caps.unsafe.unsafeAssumeSeparate:
        Llm.fetch(Anthropic.failure(_, _)):
          target.submit(endpoint, target.payload(turn, streaming = true))

    Llm.frames(response).flatMap: frame => Anthropic.events(Anthropic.frame(frame)).stdlib.iterator
