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
import denominative.nil
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
import symbolism.*
import telekinesis.*, postables.jsonPostable
import urticose.*
import vacuous.*

object OpenAI:
  // Not a case class: a case class's synthetic `apply` returns a singleton-refined type no
  // `Sessional` instance matches, so `OpenAI(…).session` would not resolve.
  def apply(model: Text, key: Text): OpenAI =
    new OpenAI(model, key, url"https://api.openai.com/v1", Unset, Llm.Settings(), List(), List())

  // The same wire dialect served by vLLM, Ollama, Groq, Mistral and the rest: only the base
  // URL changes, and a local server may need no key at all.
  def compatible(base: HttpUrl, model: Text, key: Optional[Text] = Unset): OpenAI =
    new OpenAI(model, key, base, Unset, Llm.Settings(), List(), List())

  // A named instance class rather than an anonymous given: an anonymous subclass would freshen
  // the capability types in its inferred `Result` member.
  class Sessional
    ( using online:      Online,
            backend:     Http.Backend,
            loggable:    (Http.Event is Loggable)^,
            tactic:      Tactic[Llm.Error],
            diagnostics: Diagnostics )
  extends aperture.Sessional:
    type Self = OpenAI

    // A fresh capability (`^`, not `^{caps.any}`): each `session` call's handle is its own
    // existential, so returning it (or anything capturing it) from the block is a level
    // violation the capture checker rejects.
    type Result = Llm.Session^

    def session[result](target: OpenAI)(lambda: (session: Result) ?=> result): result =
      // The dialect is laundered pure at this one boundary, exactly as in
      // `Anthropic.Sessional`: confinement is the fresh `Result` capability's job.
      lambda
        ( using Llm.Session
            ( caps.unsafe.unsafeAssumePure(ChatDialect(target)), target.system,
                target.tools, target.settings, target.priming ) )

  given sessional
  :   ( online:      Online,
        backend:     Http.Backend,
        loggable:    (Http.Event is Loggable)^,
        tactic:      Tactic[Llm.Error],
        diagnostics: Diagnostics )
  =>  ( Sessional^{online, loggable, tactic, caps.any} ) =

    Sessional()

  object Responses:
    // The Responses API target, distinct from Chat Completions: the same credentials and
    // knobs, a different wire shape. Obtained with `OpenAI(model, key).responses`.
    class Sessional
      ( using online:      Online,
              backend:     Http.Backend,
              loggable:    (Http.Event is Loggable)^,
              tactic:      Tactic[Llm.Error],
              diagnostics: Diagnostics )
    extends aperture.Sessional:
      type Self = Responses
      type Result = Llm.Session^

      def session[result](target: Responses)(lambda: (session: Result) ?=> result): result =
        lambda
          ( using Llm.Session
              ( caps.unsafe.unsafeAssumePure(ResponsesDialect(target.chat)), target.chat.system,
                  target.chat.tools, target.chat.settings, target.chat.priming ) )

    given sessional
    :   ( online:      Online,
          backend:     Http.Backend,
          loggable:    (Http.Event is Loggable)^,
          tactic:      Tactic[Llm.Error],
          diagnostics: Diagnostics )
    =>  ( Sessional^{online, loggable, tactic, caps.any} ) =

      Sessional()

  class Responses private[sibylline] (private[sibylline] val chat: OpenAI)

  private def text(json: Json): Text raises Json.Error =
    caps.unsafe.unsafeAssumeSeparate(json.as[Text])

  private def integer(json: Json): Int raises Json.Error =
    caps.unsafe.unsafeAssumeSeparate(json.as[Int])

  private def list(json: Json): List[Json] raises Json.Error =
    caps.unsafe.unsafeAssumeSeparate(json.as[List[Json]])

  private[sibylline] def frame(text: Text): Sse raises Sse.Error =
    caps.unsafe.unsafeAssumeSeparate(text.as[Sse])

  private[sibylline] def stop(code: Text): Llm.Stop = code match
    case t"stop"           => Llm.Stop.Ended
    case t"length"         => Llm.Stop.Exhausted
    case t"tool_calls"     => Llm.Stop.ToolCall
    case t"function_call"  => Llm.Stop.ToolCall
    case t"content_filter" => Llm.Stop.Filtered(Unset)
    case other             => Llm.Stop.Other(other)

  private[sibylline] def usage(json: Json)(using Diagnostics): Optional[Llm.Usage] =
    safely:
      Llm.Usage
        ( integer(json.prompt_tokens), integer(json.completion_tokens),
          safely(integer(json.prompt_tokens_details.cached_tokens)) )

  // A binary source as Chat Completions wants it: a URL, or a base64 data URL.
  private def address(source: Llm.Content.Source): Text = source match
    case Llm.Content.Source.Remote(url) => url.show

    case Llm.Content.Source.Inline(data, mediaType) =>
      t"data:${mediaType.show};base64,${data.serialize[Base64]}"

  // One user-side content block as a Chat content *part*. Documents and another provider's
  // opaque blocks have no Chat form, and are dropped.
  private def part(content: Llm.Content): Optional[Json] = content match
    case Llm.Content.Textual(text) =>
      Json.make(`type` = t"text".in[Json], text = text.in[Json])

    case Llm.Content.Graphic(source) =>
      Json.make
        ( `type`    = t"image_url".in[Json],
          image_url = Json.make(url = address(source).in[Json]) )

    case _ => Unset

  // One neutral message as its Chat Completions wire messages. One-to-many: an assistant
  // message's tool-use blocks become a `tool_calls` array, and a user message's tool results
  // become `role: "tool"` messages of their own.
  private[sibylline] def messages(message: Llm.Message): List[Json] = message.role match
    case Llm.Role.Assistant =>
      val calls: List[Json] = message.content.bind:
        case Llm.Content.ToolUse(id, tool, arguments) =>
          List:
            Json.make
              ( id       = id.in[Json],
                `type`   = t"function".in[Json],
                function = Json.make
                             ( name      = tool.in[Json],
                               arguments = arguments.encode.in[Json] ) )

        case _ => List()

      val body: Text = message.content.bind:
        case Llm.Content.Textual(text) => List(text)
        case _                         => List()

      . join

      List:
        Json.make
          ( role       = t"assistant".in[Json],
            content    = (if body == t"" then Unset else body).in[Json],
            tool_calls = (if calls.nil then Unset else calls).in[Json] )

    case Llm.Role.User =>
      val results: List[Json] = message.content.bind:
        case Llm.Content.ToolResult(id, content, failure) =>
          val body: Text = content.bind:
            case Llm.Content.Textual(text) => List(text)
            case _                         => List()

          . join

          List:
            Json.make
              ( role         = t"tool".in[Json],
                tool_call_id = id.in[Json],
                content      = body.in[Json] )

        case _ => List()

      val parts: List[Json] = message.content.bind(part(_).let(List(_)).or(List()))

      val plain: Optional[Json] = message.content match
        case List(Llm.Content.Textual(text)) => text.in[Json]

        case _ =>
          if parts.nil then Unset else parts.in[Json]

      val turn: List[Json] = plain match
        case content: Json => List(Json.make(role = t"user".in[Json], content = content))
        case _             => List()

      results + turn

  private[sibylline] def tool(tool: Llm.Tool): Json =
    Json.make
      ( `type`   = t"function".in[Json],
        function = Json.make
                     ( name        = tool.name.in[Json],
                       description = tool.description.in[Json],
                       parameters  = tool.parameters.in[Json] ) )

  private[sibylline] def choice(choice: Llm.ToolChoice): Json = choice match
    case Llm.ToolChoice.Auto      => t"auto".in[Json]
    case Llm.ToolChoice.Forbidden => t"none".in[Json]
    case Llm.ToolChoice.Required  => t"required".in[Json]

    case Llm.ToolChoice.Named(tool) =>
      Json.make
        ( `type`   = t"function".in[Json],
          function = Json.make(name = tool.in[Json]) )

  // The non-streamed Chat reply: `choices[0].message`, its tool calls (arguments arrive as a
  // *string* of JSON), the finish reason and the usage.
  private[sibylline] def reply(json: Json)(using Diagnostics)
  :   Llm.Reply raises Json.Error raises Llm.Error =

    val message = json.choices(0).message

    val texts: List[Llm.Content] =
      safely(text(message.content)).let { text => List(Llm.Content.Textual(text)) }.or(List())

    val calls: List[Llm.Content] = safely(list(message.tool_calls)).or(List()).map: call =>
      Llm.Content.ToolUse
        ( text(call.id), text(call.function.name), Llm.parsed(text(call.function.arguments)) )

    Llm.Reply
      ( Llm.Message(Llm.Role.Assistant, texts + calls),
        safely(text(json.choices(0).finish_reason)).let(stop(_)).or(Llm.Stop.Ended),
        usage(json.usage).or(Llm.Usage(0, 0)),
        safely(text(json.model)),
        safely(text(json.id)) )

  // One Chat streaming chunk as neutral events. Text deltas live at index 0; each tool call
  // occupies `1 + `its wire index. The terminal `[DONE]` frame yields nothing here — the
  // stream's `Progress` closes out the message when the frames end.
  private[sibylline] def events(progress: Llm.Progress, sse: Sse)
    ( using Tactic[Llm.Error], Diagnostics )
  :   List[Llm.Event] =

    given jsonTactic: (Tactic[Json.Error]^) = summon[Tactic[Llm.Error]].contramap: _ =>
      Llm.Error(Llm.Error.Reason.Malformed, t"a stream chunk had an unexpected shape")

    val data: Text = sse.data.prim.or(t"")

    if data == t"[DONE]" then List() else
      val json: Json = Llm.parsed(data)

      val started: List[Llm.Event] =
        if progress.begun then List() else
          progress.begun = true
          List(Llm.Event.Started(safely(text(json.id)), safely(text(json.model))))

      usage(json.usage).let(progress.usage = _)
      val delta: Optional[Json] = safely(json.choices(0).delta)

      safely(text(json.choices(0).finish_reason)).let: reason => progress.stop = stop(reason)

      val texts: List[Llm.Event] = delta.let: delta =>
        safely(text(delta.content)).let: content =>
          val opened: List[Llm.Event] =
            if progress.open(0) then List(Llm.Event.Opened(0, Llm.Content.Textual(t""))) else List()

          opened + List(Llm.Event.Delta(0, Llm.Event.Increment.Textual(content)))

      . or(List())

      val calls: List[Llm.Event] = delta.let: delta =>
        safely(list(delta.tool_calls)).or(List()).bind: call =>
          val index = 1 + integer(call.index)

          val opened: List[Llm.Event] =
            if progress.open(index) then
              List:
                Llm.Event.Opened
                  ( index,
                    Llm.Content.ToolUse
                      ( safely(text(call.id)).or(t""),
                        safely(text(call.function.name)).or(t""),
                        Llm.parsed(t"{}") ) )
            else
              List()

          val fragment: List[Llm.Event] =
            safely(text(call.function.arguments)).let: arguments =>
              List(Llm.Event.Delta(index, Llm.Event.Increment.Arguments(arguments)))

            . or(List())

          opened + fragment

      . or(List())

      started + texts + calls

  // The OpenAI error envelope, `{"error": {"message": …, "type": …, "code": …}}`, mapped
  // through the status first: the codes vary by deployment, the statuses do not.
  private[sibylline] def failure(status: Http.Status, json: Optional[Json])(using Diagnostics)
  :   Llm.Error =

    val detail: Text =
      json.let { json => safely(text(json.error.message)) }.or(t"the request failed")

    val code: Optional[Text] = json.let: json => safely(text(json.error.code))

    val reason = status.code match
      case 401 | 403 => Llm.Error.Reason.Unauthorized
      case 404       => Llm.Error.Reason.NotFound
      case 413       => Llm.Error.Reason.TooLarge
      case 429       => Llm.Error.Reason.RateLimited
      case 503       => Llm.Error.Reason.Overloaded

      case _ =>
        if code == t"context_length_exceeded" then Llm.Error.Reason.TooLarge
        else code.let(Llm.Error.Reason.Provider(_)).or(Llm.Error.Reason.Invalid)

    Llm.Error(reason, detail, status.code)

// The provider target for Chat Completions: a pure value, shared and reused across sessions.
class OpenAI private
  ( val model:    Text,
    key:          Optional[Text],
    base:         HttpUrl,
    val system:   Optional[Text],
    val settings: Llm.Settings,
    val priming:  List[Llm.Message],
    val tools:    List[Llm.Tool] ):

  private def copy
    ( system:   Optional[Text]    = system,
      settings: Llm.Settings      = settings,
      priming:  List[Llm.Message] = priming,
      tools:    List[Llm.Tool]    = tools )
  :   OpenAI =

    new OpenAI(model, key, base, system, settings, priming, tools)

  def prompted(system: Text): OpenAI = copy(system = system)
  def limit(maxTokens: Int): OpenAI = copy(settings = settings.copy(maxTokens = maxTokens))

  def warmth(temperature: Double): OpenAI =
    copy(settings = settings.copy(temperature = temperature))

  def sampling(topP: Double): OpenAI = copy(settings = settings.copy(topP = topP))

  def stopping(sequences: Text*): OpenAI =
    copy(settings = settings.copy(stopSequences = List(sequences*)))

  def iterating(limit: Int): OpenAI = copy(settings = settings.copy(iterations = limit))
  def primed(messages: List[Llm.Message]): OpenAI = copy(priming = messages)
  def tooled(tools: List[Llm.Tool]): OpenAI = copy(tools = tools)

  // The same target, spoken to through the Responses API instead of Chat Completions.
  def responses: OpenAI.Responses = OpenAI.Responses(this)

  private[sibylline] def address(path: Text): HttpUrl =
    Url(base.origin, t"${base.location}/$path")

  private[sibylline] def submit(endpoint: HttpUrl, body: Json)
    ( using Online, Http.Backend, (Http.Event is Loggable)^, Tactic[Connect.Error] )
  :   Http.Response =

    key.lay(endpoint.submit(Http.Post)(body)): key =>
      endpoint.submit(Http.Post, authorization = Auth.Bearer(key))(body)

  // The Chat Completions request body. The system prompt is a leading `system` message, and
  // streamed requests ask for the final usage chunk.
  private[sibylline] def payload(turn: Llm.Exchange, streaming: Boolean): Json =
    val system: List[Json] = turn.system.let: system =>
      List(Json.make(role = t"system".in[Json], content = system.in[Json]))

    . or(List())

    val messages = system + turn.history.bind(OpenAI.messages(_))
    val tools = turn.tools.map(OpenAI.tool(_))
    val stops = turn.settings.stopSequences

    Json.make
      ( model                 = model.in[Json],
        messages              = messages.in[Json],
        max_completion_tokens = turn.settings.maxTokens.in[Json],
        temperature           = turn.settings.temperature.in[Json],
        top_p                 = turn.settings.topP.in[Json],
        stop                  = (if stops.nil then Unset else stops).in[Json],
        stream                = (if streaming then streaming else Unset).in[Json],
        stream_options        = streamOptions(streaming),
        tools                 = (if tools.nil then Unset else tools).in[Json],
        tool_choice           = turn.settings.toolChoice.let(OpenAI.choice(_)).in[Json] )

  private def streamOptions(streaming: Boolean): Json =
    (if streaming then Json.make(include_usage = true.in[Json]) else Unset).in[Json]

// The `Llm.Dialect` for Chat Completions.
private[sibylline] class ChatDialect(target: OpenAI)
  ( using online:      Online,
          backend:     Http.Backend,
          loggable:    (Http.Event is Loggable)^,
          tactic:      Tactic[Llm.Error],
          diagnostics: Diagnostics )
extends Llm.Dialect, caps.ExclusiveCapability:

  def name: Text = t"openai"

  private def endpoint: HttpUrl = target.address(t"chat/completions")

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
        Llm.fetch(OpenAI.failure(_, _)):
          target.submit(endpoint, target.payload(turn, streaming = false))

    OpenAI.reply(Llm.receive(response))

  def stream(turn: Llm.Exchange): Iterator[Llm.Event]^{this} =
    val response =
      caps.unsafe.unsafeAssumeSeparate:
        Llm.fetch(OpenAI.failure(_, _)):
          target.submit(endpoint, target.payload(turn, streaming = true))

    val progress = Llm.Progress()

    // The frames are followed by one sentinel, on whose arrival the translation closes out
    // the message — chunk streams end at `[DONE]` with no closing events of their own.
    // `.stdlib.iterator`: this method's contract is a stdlib `Iterator`, which the native `List`
    // has no accessor for — the boundary is the return type, not the interior.
    (Llm.frames(response) ++ Iterator(Llm.Terminal)).flatMap: frame =>
      if frame == Llm.Terminal then progress.finish().stdlib.iterator
      else OpenAI.events(progress, OpenAI.frame(frame)).stdlib.iterator

private[sibylline] object ResponsesDialect:
  private def text(json: Json): Text raises Json.Error =
    caps.unsafe.unsafeAssumeSeparate(json.as[Text])

  private def integer(json: Json): Int raises Json.Error =
    caps.unsafe.unsafeAssumeSeparate(json.as[Int])

  private def list(json: Json): List[Json] raises Json.Error =
    caps.unsafe.unsafeAssumeSeparate(json.as[List[Json]])

  private def usage(json: Json)(using Diagnostics): Optional[Llm.Usage] =
    safely:
      Llm.Usage
        ( integer(json.input_tokens), integer(json.output_tokens),
          safely(integer(json.input_tokens_details.cached_tokens)) )

  // One neutral message as its Responses input items: a `message` item for the readable
  // content, plus `function_call`/`function_call_output` items for tool traffic.
  private def items(message: Llm.Message): List[Json] = message.role match
    case Llm.Role.Assistant =>
      val calls: List[Json] = message.content.bind:
        case Llm.Content.ToolUse(id, tool, arguments) =>
          List:
            Json.make
              ( `type`    = t"function_call".in[Json],
                call_id   = id.in[Json],
                name      = tool.in[Json],
                arguments = arguments.encode.in[Json] )

        case _ => List()

      val body: Text = message.content.bind:
        case Llm.Content.Textual(text) => List(text)
        case _                         => List()

      . join

      val turn: List[Json] =
        if body == t"" then List()
        else
          val part = Json.make(`type` = t"output_text".in[Json], text = body.in[Json])

          List:
            Json.make
              ( `type`  = t"message".in[Json],
                role    = t"assistant".in[Json],
                content = (List(part): List[Json]).in[Json] )

      turn + calls

    case Llm.Role.User =>
      val results: List[Json] = message.content.bind:
        case Llm.Content.ToolResult(id, content, failure) =>
          val body: Text = content.bind:
            case Llm.Content.Textual(text) => List(text)
            case _                         => List()

          . join

          List:
            Json.make
              ( `type`  = t"function_call_output".in[Json],
                call_id = id.in[Json],
                output  = body.in[Json] )

        case _ => List()

      val parts: List[Json] = message.content.bind:
        case Llm.Content.Textual(text) =>
          List(Json.make(`type` = t"input_text".in[Json], text = text.in[Json]))

        case Llm.Content.Graphic(Llm.Content.Source.Remote(url)) =>
          List:
            Json.make(`type` = t"input_image".in[Json], image_url = url.show.in[Json])

        case _ => List()

      val turn: List[Json] =
        if parts.nil then List()
        else
          List:
            Json.make
              ( `type`  = t"message".in[Json],
                role    = t"user".in[Json],
                content = parts.in[Json] )

      results + turn

  private def tool(tool: Llm.Tool): Json =
    Json.make
      ( `type`      = t"function".in[Json],
        name        = tool.name.in[Json],
        description = tool.description.in[Json],
        parameters  = tool.parameters.in[Json] )

  private[sibylline] def payload(target: OpenAI, turn: Llm.Exchange, streaming: Boolean)
  :   Json =

    val tools = turn.tools.map(tool(_))

    Json.make
      ( model             = target.model.in[Json],
        input             = turn.history.bind(items(_)).in[Json],
        instructions      = turn.system.in[Json],
        max_output_tokens = turn.settings.maxTokens.in[Json],
        temperature       = turn.settings.temperature.in[Json],
        top_p             = turn.settings.topP.in[Json],
        stream            = (if streaming then streaming else Unset).in[Json],
        tools             = (if tools.nil then Unset else tools).in[Json],
        tool_choice       = turn.settings.toolChoice.let(choice(_)).in[Json] )

  // Unlike Chat Completions, a named Responses tool choice is flat, not nested.
  private def choice(choice: Llm.ToolChoice): Json = choice match
    case Llm.ToolChoice.Named(tool) =>
      Json.make(`type` = t"function".in[Json], name = tool.in[Json])

    case other => OpenAI.choice(other)

  // One output item as neutral content blocks.
  private def blocks(item: Json)(using Diagnostics)
  :   List[Llm.Content] raises Json.Error raises Llm.Error =

    safely(text(item.`type`)).or(t"") match
      case t"message" =>
        list(item.content).bind: part =>
          safely(text(part.`type`)).or(t"") match
            case t"output_text" => List(Llm.Content.Textual(text(part.text)))
            case t"refusal"     => List(Llm.Content.Textual(text(part.refusal)))
            case _              => List(Llm.Content.Opaque(t"openai-responses", part))

      case t"function_call" =>
        List:
          Llm.Content.ToolUse
            ( text(item.call_id), text(item.name), Llm.parsed(text(item.arguments)) )

      case t"reasoning" => List()
      case _            => List(Llm.Content.Opaque(t"openai-responses", item))

  private[sibylline] def reply(json: Json)(using Diagnostics)
  :   Llm.Reply raises Json.Error raises Llm.Error =

    val content: List[Llm.Content] = list(json.output).bind(blocks(_))

    val called = content.exists:
      case Llm.Content.ToolUse(_, _, _) => true
      case _                            => false

    val stop: Llm.Stop =
      if called then Llm.Stop.ToolCall
      else safely(text(json.status)).or(t"completed") match
        case t"completed"  => Llm.Stop.Ended

        case t"incomplete" =>
          safely(text(json.incomplete_details.reason)).or(t"") match
            case t"max_output_tokens" => Llm.Stop.Exhausted
            case t"content_filter"    => Llm.Stop.Filtered(Unset)
            case other                => Llm.Stop.Other(other)

        case other => Llm.Stop.Other(other)

    Llm.Reply
      ( Llm.Message(Llm.Role.Assistant, content),
        stop,
        usage(json.usage).or(Llm.Usage(0, 0)),
        safely(text(json.model)),
        safely(text(json.id)) )

  // One semantic streaming event as neutral events, dispatched on the SSE `event:` field.
  private[sibylline] def events(progress: Llm.Progress, sse: Sse)
    ( using Tactic[Llm.Error], Diagnostics )
  :   List[Llm.Event] =

    given jsonTactic: (Tactic[Json.Error]^) = summon[Tactic[Llm.Error]].contramap: _ =>
      Llm.Error(Llm.Error.Reason.Malformed, t"a stream event had an unexpected shape")

    val json: Json = Llm.parsed(sse.data.prim.or(t"{}"))

    sse.event match
      case t"response.created" =>
        progress.begun = true

        List:
          Llm.Event.Started
            ( safely(text(json.response.id)), safely(text(json.response.model)) )

      case t"response.output_item.added" =>
        val index = integer(json.output_index)
        progress.open(index)

        safely(text(json.item.`type`)).or(t"") match
          case t"message" =>
            List(Llm.Event.Opened(index, Llm.Content.Textual(t"")))

          case t"function_call" =>
            List:
              Llm.Event.Opened
                ( index,
                  Llm.Content.ToolUse
                    ( safely(text(json.item.call_id)).or(t""),
                      safely(text(json.item.name)).or(t""),
                      Llm.parsed(t"{}") ) )

          case _ =>
            List:
              Llm.Event.Opened(index, Llm.Content.Opaque(t"openai-responses", json.item))

      case t"response.output_text.delta" =>
        List:
          Llm.Event.Delta
            ( integer(json.output_index), Llm.Event.Increment.Textual(text(json.delta)) )

      case t"response.function_call_arguments.delta" =>
        List:
          Llm.Event.Delta
            ( integer(json.output_index), Llm.Event.Increment.Arguments(text(json.delta)) )

      case t"response.output_item.done" =>
        val index = integer(json.output_index)
        progress.opened.remove(index)

        val called = safely(text(json.item.`type`)) == t"function_call"
        if called then progress.stop = Llm.Stop.ToolCall
        List(Llm.Event.Closed(index))

      case t"response.completed" =>
        usage(json.response.usage).let(progress.usage = _)
        List()

      case t"error" =>
        abort:
          Llm.Error
            ( Llm.Error.Reason.Provider(safely(text(json.code)).or(t"error")),
              safely(text(json.message)).or(t"the stream reported an error") )

      case _ => List()

// The `Llm.Dialect` for the Responses API: the same credentials, a different wire shape —
// item-based input, semantic streaming events. History is client-replayed, as in every other
// dialect; `previous_response_id` server-side threading may come later.
private[sibylline] class ResponsesDialect(target: OpenAI)
  ( using online:      Online,
          backend:     Http.Backend,
          loggable:    (Http.Event is Loggable)^,
          tactic:      Tactic[Llm.Error],
          diagnostics: Diagnostics )
extends Llm.Dialect, caps.ExclusiveCapability:

  def name: Text = t"openai-responses"

  private def endpoint: HttpUrl = target.address(t"responses")

  private given connectTactic: (Tactic[Connect.Error]^) = tactic.contramap: _ =>
    Llm.Error(Llm.Error.Reason.Unreachable, t"the provider could not be reached")

  private given jsonTactic: (Tactic[Json.Error]^) = tactic.contramap: _ =>
    Llm.Error(Llm.Error.Reason.Malformed, t"the reply had an unexpected shape")

  private given sseTactic: (Tactic[Sse.Error]^) = tactic.contramap: _ =>
    Llm.Error(Llm.Error.Reason.Malformed, t"a server-sent event was not valid")

  def exchange(turn: Llm.Exchange): Llm.Reply =
    val response =
      caps.unsafe.unsafeAssumeSeparate:
        Llm.fetch(OpenAI.failure(_, _)):
          target.submit(endpoint, ResponsesDialect.payload(target, turn, streaming = false))

    ResponsesDialect.reply(Llm.receive(response))

  def stream(turn: Llm.Exchange): Iterator[Llm.Event]^{this} =
    val response =
      caps.unsafe.unsafeAssumeSeparate:
        Llm.fetch(OpenAI.failure(_, _)):
          target.submit(endpoint, ResponsesDialect.payload(target, turn, streaming = true))

    val progress = Llm.Progress()

    // As in `ChatDialect.stream`: a sentinel closes out the message after the last frame, and
    // `.stdlib.iterator` bridges to the stdlib `Iterator` this method's contract returns.
    (Llm.frames(response) ++ Iterator(Llm.Terminal)).flatMap: frame =>
      if frame == Llm.Terminal then progress.finish().stdlib.iterator
      else ResponsesDialect.events(progress, OpenAI.frame(frame)).stdlib.iterator
