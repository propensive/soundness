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

object Gemini:
  // The wire header the Gemini API takes: `xGoogApiKey = key` renders as `x-goog-api-key`.
  given xGoogApiKey: ("xGoogApiKey" is Directive of Text) = identity(_)

  // Not a case class: a case class's synthetic `apply` returns a singleton-refined type no
  // `Sessional` instance matches, so `Gemini(…).session` would not resolve.
  def apply(model: Text, key: Text): Gemini =
    new Gemini
      ( model, key, url"https://generativelanguage.googleapis.com", Unset, Llm.Settings(),
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
    type Self = Gemini

    // A fresh capability (`^`, not `^{caps.any}`): each `session` call's handle is its own
    // existential, so returning it (or anything capturing it) from the block is a level
    // violation the capture checker rejects.
    type Result = Llm.Session^

    def session[result](target: Gemini)(lambda: (session: Result) ?=> result): result =
      // The dialect is laundered pure at this one boundary, exactly as in
      // `Anthropic.Sessional`: confinement is the fresh `Result` capability's job.
      lambda
        ( using Llm.Session
            ( caps.unsafe.unsafeAssumePure(GeminiDialect(target)), target.system,
                target.tools, target.settings, target.priming ) )

  given sessional
  :   ( online:      Online,
        backend:     Http.Backend,
        loggable:    (Http.Event is Loggable)^,
        tactic:      Tactic[Llm.Error],
        diagnostics: Diagnostics )
  =>  ( Sessional^{online, loggable, tactic, caps.any} ) =

    Sessional()

  private def text(json: Json): Text raises Json.Error =
    caps.unsafe.unsafeAssumeSeparate(json.as[Text])

  private def integer(json: Json): Int raises Json.Error =
    caps.unsafe.unsafeAssumeSeparate(json.as[Int])

  private def list(json: Json): List[Json] raises Json.Error =
    caps.unsafe.unsafeAssumeSeparate(json.as[List[Json]])

  private[sibylline] def frame(text: Text): Sse raises Sse.Error =
    caps.unsafe.unsafeAssumeSeparate(text.as[Sse])

  // Gemini reports finish reasons in upper snake case. A reply containing a function call is
  // `ToolCall` regardless: the API says `STOP` for those too.
  private[sibylline] def stop(code: Text): Llm.Stop = code match
    case t"STOP"       => Llm.Stop.Ended
    case t"MAX_TOKENS" => Llm.Stop.Exhausted

    case t"SAFETY" | t"RECITATION" | t"BLOCKLIST" | t"PROHIBITED_CONTENT" | t"SPII" =>
      Llm.Stop.Filtered(code)

    case other => Llm.Stop.Other(other)

  private[sibylline] def usage(json: Json)(using Diagnostics): Optional[Llm.Usage] =
    safely:
      Llm.Usage
        ( integer(json.promptTokenCount),
          safely(integer(json.candidatesTokenCount)).or(0),
          safely(integer(json.cachedContentTokenCount)),
          Unset,
          safely(integer(json.thoughtsTokenCount)) )

  // One neutral content block as a Gemini *part* — or `Unset` for a block with no Gemini
  // form. Function calls carry no id on this wire, so the tool's name serves as the neutral
  // id, and a result addresses its call by that name.
  private def part(content: Llm.Content): Optional[Json] = content match
    case Llm.Content.Textual(text) =>
      Json.make(text = text.in[Json])

    case Llm.Content.Graphic(source)  => origin(source)
    case Llm.Content.Document(source) => origin(source)

    case Llm.Content.ToolUse(id, tool, arguments) =>
      Json.make
        ( functionCall = Json.make(name = tool.in[Json], args = arguments.in[Json]) )

    case Llm.Content.ToolResult(id, content, failure) =>
      val body: Text = content.bind:
        case Llm.Content.Textual(text) => List(text)
        case _                         => List()

      . join

      Json.make
        ( functionResponse = Json.make
                               ( name     = id.in[Json],
                                 response = Json.make(result = body.in[Json]) ) )

    case Llm.Content.Opaque(provider, json) =>
      if provider == t"gemini" then json else Unset

    case _ => Unset

  private def origin(source: Llm.Content.Source): Optional[Json] = source match
    case Llm.Content.Source.Inline(data, mediaType) =>
      Json.make
        ( inlineData = Json.make
                         ( mimeType = mediaType.show.in[Json],
                           data     = data.serialize[Base64].in[Json] ) )

    case Llm.Content.Source.Remote(url) =>
      Json.make(fileData = Json.make(fileUri = url.show.in[Json]))

  private[sibylline] def content(message: Llm.Message): Json =
    val role = message.role match
      case Llm.Role.User      => t"user"
      case Llm.Role.Assistant => t"model"

    val parts = message.content.bind(part(_).let(List(_)).or(List()))
    Json.make(role = role.in[Json], parts = parts.in[Json])

  private def mode(choice: Llm.ToolChoice): Json = choice match
    case Llm.ToolChoice.Auto      => Json.make(mode = t"AUTO".in[Json])
    case Llm.ToolChoice.Forbidden => Json.make(mode = t"NONE".in[Json])
    case Llm.ToolChoice.Required  => Json.make(mode = t"ANY".in[Json])

    case Llm.ToolChoice.Named(tool) =>
      Json.make
        ( mode                 = t"ANY".in[Json],
          allowedFunctionNames = (List(tool): List[Text]).in[Json] )

  private def calling(choice: Optional[Llm.ToolChoice]): Json =
    choice.let { choice => Json.make(functionCallingConfig = mode(choice)) }.in[Json]

  private[sibylline] def payload(turn: Llm.Exchange): Json =
    val declarations = turn.tools.map: tool =>
      Json.make
        ( name        = tool.name.in[Json],
          description = tool.description.in[Json],
          parameters  = tool.parameters.in[Json] )

    val stops = turn.settings.stopSequences

    val config =
      Json.make
        ( temperature     = turn.settings.temperature.in[Json],
          topP            = turn.settings.topP.in[Json],
          maxOutputTokens = turn.settings.maxTokens.in[Json],
          stopSequences   = (if stops.stdlib.isEmpty then Unset else stops).in[Json] )

    val instruction: Optional[Json] = turn.system.let: system =>
      Json.make(parts = (List(Json.make(text = system.in[Json])): List[Json]).in[Json])

    val tools: Optional[Json] =
      if turn.tools.stdlib.isEmpty then Unset
      else (List(Json.make(functionDeclarations = declarations.in[Json])): List[Json]).in[Json]

    Json.make
      ( contents          = turn.history.map(content(_)).in[Json],
        systemInstruction = instruction.in[Json],
        generationConfig  = config,
        tools             = tools.in[Json],
        toolConfig        = calling(turn.settings.toolChoice) )

  // One Gemini part as a neutral content block.
  private def block(json: Json)(using Diagnostics): Llm.Content raises Json.Error =
    safely(text(json.text)).let(Llm.Content.Textual(_)).or:
      safely(json.functionCall).let: call =>
        Llm.Content.ToolUse(text(call.name), text(call.name), call.args)

      . or(Llm.Content.Opaque(t"gemini", json))

  private[sibylline] def reply(json: Json)(using Diagnostics): Llm.Reply raises Json.Error =
    val candidate = json.candidates(0)
    val content: List[Llm.Content] = list(candidate.content.parts).map(block(_))

    val called = content.stdlib.exists:
      case Llm.Content.ToolUse(_, _, _) => true
      case _                            => false

    val stopped: Llm.Stop =
      if called then Llm.Stop.ToolCall
      else safely(text(candidate.finishReason)).let(stop(_)).or(Llm.Stop.Ended)

    Llm.Reply
      ( Llm.Message(Llm.Role.Assistant, content),
        stopped,
        usage(json.usageMetadata).or(Llm.Usage(0, 0)),
        safely(text(json.modelVersion)),
        safely(text(json.responseId)) )

  // One streamed frame — a whole `GenerateContentResponse` fragment — as neutral events.
  // Text fragments grow block zero; a function call arrives whole, as its own block, opened
  // and closed in one frame. Usage is cumulative on this wire, so the running total *replaces*
  // the progress state and lands once, when the frames end: there is no terminal sentinel.
  private[sibylline] def events(progress: Llm.Progress, sse: Sse)
    ( using Tactic[Llm.Error], Diagnostics )
  :   List[Llm.Event] =

    given jsonTactic: (Tactic[Json.Error]^) = summon[Tactic[Llm.Error]].contramap: _ =>
      Llm.Error(Llm.Error.Reason.Malformed, t"a stream frame had an unexpected shape")

    val json: Json = Llm.parsed(sse.data.stdlib.join(t"\n"))

    val started: List[Llm.Event] =
      if progress.begun then List() else
        progress.begun = true
        List(Llm.Event.Started(safely(text(json.responseId)), safely(text(json.modelVersion))))

    usage(json.usageMetadata).let(progress.usage = _)

    safely(text(json.candidates(0).finishReason)).let: reason => progress.stop = stop(reason)

    val parts: List[Json] = safely(list(json.candidates(0).content.parts)).or(List())

    val blocks: List[Llm.Event] = parts.bind: part =>
      safely(text(part.text)).let: fragment =>
        val opened: List[Llm.Event] =
          if progress.open(0) then List(Llm.Event.Opened(0, Llm.Content.Textual(t""))) else List()

        (opened.stdlib :+ Llm.Event.Delta(0, Llm.Event.Increment.Textual(fragment))).to(List)

      . or:
          safely(part.functionCall).let: call =>
            val index = progress.next()
            progress.stop = Llm.Stop.ToolCall

            List
              ( Llm.Event.Opened
                  ( index,
                    Llm.Content.ToolUse(text(call.name), text(call.name), call.args) ),
                Llm.Event.Closed(index) )

          . or(List())

    (started.stdlib ++ blocks.stdlib).to(List)

  // The Google error envelope, `{"error": {"code": …, "message": …, "status": …}}`.
  private[sibylline] def failure(status: Http.Status, json: Optional[Json])(using Diagnostics)
  :   Llm.Error =

    val detail: Text =
      json.let { json => safely(text(json.error.message)) }.or(t"the request failed")

    val code: Optional[Text] = json.let: json => safely(text(json.error.status))

    val reason = status.code match
      case 401 | 403 => Llm.Error.Reason.Unauthorized
      case 404       => Llm.Error.Reason.NotFound
      case 429       => Llm.Error.Reason.RateLimited
      case 503       => Llm.Error.Reason.Overloaded
      case _         => code.let(Llm.Error.Reason.Provider(_)).or(Llm.Error.Reason.Invalid)

    Llm.Error(reason, detail, status.code)

// The provider target: a pure value, shared and reused across sessions.
class Gemini private
  ( val model:    Text,
    key:          Text,
    base:         HttpUrl,
    val system:   Optional[Text],
    val settings: Llm.Settings,
    val priming:  List[Llm.Message],
    val tools:    List[Llm.Tool] ):

  private def copy
    ( system:   Optional[Text]    = system,
      settings: Llm.Settings      = settings,
      priming:  List[Llm.Message] = priming,
      tools:    List[Llm.Tool]    = tools,
      base:     HttpUrl           = base )
  :   Gemini =

    new Gemini(model, key, base, system, settings, priming, tools)

  def prompted(system: Text): Gemini = copy(system = system)
  def limit(maxTokens: Int): Gemini = copy(settings = settings.copy(maxTokens = maxTokens))

  def warmth(temperature: Double): Gemini =
    copy(settings = settings.copy(temperature = temperature))

  def sampling(topP: Double): Gemini = copy(settings = settings.copy(topP = topP))

  def stopping(sequences: Text*): Gemini =
    copy(settings = settings.copy(stopSequences = List(sequences*)))

  def iterating(limit: Int): Gemini = copy(settings = settings.copy(iterations = limit))
  def primed(messages: List[Llm.Message]): Gemini = copy(priming = messages)
  def tooled(tools: List[Llm.Tool]): Gemini = copy(tools = tools)
  def on(base: HttpUrl): Gemini = copy(base = base)

  private[sibylline] def address(streaming: Boolean): HttpUrl =
    val method = if streaming then t"streamGenerateContent?alt=sse" else t"generateContent"
    Url(base.origin, t"${base.location}/v1beta/models/$model:$method")

  private[sibylline] def submit(endpoint: HttpUrl, body: Json)
    ( using Online, Http.Backend, (Http.Event is Loggable)^, Tactic[Connect.Error] )
  :   Http.Response =

    import Gemini.xGoogApiKey
    endpoint.submit(Http.Post, xGoogApiKey = key)(body)

// The `Llm.Dialect` for the Gemini API.
private[sibylline] class GeminiDialect(target: Gemini)
  ( using online:      Online,
          backend:     Http.Backend,
          loggable:    (Http.Event is Loggable)^,
          tactic:      Tactic[Llm.Error],
          diagnostics: Diagnostics )
extends Llm.Dialect, caps.ExclusiveCapability:

  def name: Text = t"gemini"

  private given connectTactic: (Tactic[Connect.Error]^) = tactic.contramap: _ =>
    Llm.Error(Llm.Error.Reason.Unreachable, t"the provider could not be reached")

  private given jsonTactic: (Tactic[Json.Error]^) = tactic.contramap: _ =>
    Llm.Error(Llm.Error.Reason.Malformed, t"the reply had an unexpected shape")

  private given sseTactic: (Tactic[Sse.Error]^) = tactic.contramap: _ =>
    Llm.Error(Llm.Error.Reason.Malformed, t"a server-sent event was not valid")

  def exchange(turn: Llm.Exchange): Llm.Reply =
    // The send thunk captures the tactic `fetch` raises through, as in `AnthropicDialect`.
    val response =
      caps.unsafe.unsafeAssumeSeparate:
        Llm.fetch(Gemini.failure(_, _)):
          target.submit(target.address(streaming = false), Gemini.payload(turn))

    Gemini.reply(Llm.receive(response))

  def stream(turn: Llm.Exchange): Iterator[Llm.Event]^{this, caps.any} =
    val response =
      caps.unsafe.unsafeAssumeSeparate:
        Llm.fetch(Gemini.failure(_, _)):
          target.submit(target.address(streaming = true), Gemini.payload(turn))

    val progress = Llm.Progress()

    // A sentinel closes out the message after the last frame: this wire has no terminal event.
    (Llm.frames(response) ++ Iterator(Llm.Terminal)).flatMap: frame =>
      if frame == Llm.Terminal then progress.finish().stdlib.iterator
      else Gemini.events(progress, Gemini.frame(frame)).stdlib.iterator
