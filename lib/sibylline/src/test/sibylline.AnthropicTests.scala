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

import soundness.*

import charDecoders.utf8Decoder
import charEncoders.utf8Encoder
import dynamicAccess.dynamicJson
import errorDiagnostics.stackTracesDiagnostics
import internetAccess.online
import logging.silentLogging
import strategies.throwUnsafely
import textSanitizers.skipSanitizer

object FakeModel:
  // One request as the provider saw it. The body is `Unset` for a request that had none.
  case class Exchange
    ( method: Http.Method, path: Text, headers: List[Http.Header], body: Optional[Text] )

  def reply(json: Text): Http.Response = Http.Response(Http.Ok)(json)

  def failure(status: Http.Status, kind: Text, message: Text): Http.Response =
    Http.Response(status):
      t"""{"type": "error", "error": {"type": "$kind", "message": "$message"}}"""

  // A minimal successful Messages reply.
  def answer(text: Text): Http.Response = reply:
    t"""{"id": "msg_1", "model": "claude-sonnet-4-5", "role": "assistant",
         "content": [{"type": "text", "text": "$text"}],
         "stop_reason": "end_turn",
         "usage": {"input_tokens": 7, "output_tokens": 13}}"""

  // A complete streamed turn: the full Messages event sequence, including a `ping`, split
  // text deltas, and the usage convention (input on `message_start`, output on
  // `message_delta`).
  val streamed: Text = scala.List
    ( t"""event: message_start""",
      t"""data: {"type": "message_start", "message": {"id": "msg_2", "model": "claude-sonnet-4-5", "usage": {"input_tokens": 11, "output_tokens": 1}}}""",
      t"""""",
      t"""event: content_block_start""",
      t"""data: {"type": "content_block_start", "index": 0, "content_block": {"type": "text", "text": ""}}""",
      t"""""",
      t"""event: ping""",
      t"""data: {"type": "ping"}""",
      t"""""",
      t"""event: content_block_delta""",
      t"""data: {"type": "content_block_delta", "index": 0, "delta": {"type": "text_delta", "text": "fjord of "}}""",
      t"""""",
      t"""event: content_block_delta""",
      t"""data: {"type": "content_block_delta", "index": 0, "delta": {"type": "text_delta", "text": "Norway"}}""",
      t"""""",
      t"""event: content_block_stop""",
      t"""data: {"type": "content_block_stop", "index": 0}""",
      t"""""",
      t"""event: message_delta""",
      t"""data: {"type": "message_delta", "delta": {"stop_reason": "end_turn"}, "usage": {"output_tokens": 9}}""",
      t"""""",
      t"""event: message_stop""",
      t"""data: {"type": "message_stop"}""",
      t"""""" )
  . mkString("\n").tt

  // A streamed tool call whose arguments arrive as split partial JSON.
  val streamedTool: Text = scala.List
    ( t"""event: message_start""",
      t"""data: {"type": "message_start", "message": {"id": "msg_3", "model": "claude-sonnet-4-5", "usage": {"input_tokens": 5, "output_tokens": 1}}}""",
      t"""""",
      t"""event: content_block_start""",
      t"""data: {"type": "content_block_start", "index": 0, "content_block": {"type": "tool_use", "id": "toolu_1", "name": "price", "input": {}}}""",
      t"""""",
      t"""event: content_block_delta""",
      t"""data: {"type": "content_block_delta", "index": 0, "delta": {"type": "input_json_delta", "partial_json": "{\\"tick"}}""",
      t"""""",
      t"""event: content_block_delta""",
      t"""data: {"type": "content_block_delta", "index": 0, "delta": {"type": "input_json_delta", "partial_json": "er\\": \\"AAPL\\"}"}}""",
      t"""""",
      t"""event: content_block_stop""",
      t"""data: {"type": "content_block_stop", "index": 0}""",
      t"""""",
      t"""event: message_delta""",
      t"""data: {"type": "message_delta", "delta": {"stop_reason": "tool_use"}, "usage": {"output_tokens": 4}}""",
      t"""""",
      t"""event: message_stop""",
      t"""data: {"type": "message_stop"}""",
      t"""""" )
  . mkString("\n").tt

// A fake Messages API: routes on method and path, records the conversation — headers and
// bodies — and replies with canned envelopes, so the whole wire layer is exercised with no
// network. A *pure* function, so that `FakeModel` is itself pure: `Http.Backend` is required
// unadorned by everything that summons one.
class FakeModel(route: (Http.Method, Text, Int) -> Http.Response) extends Http.Backend:
  @scala.caps.unsafe.untrackedCaptures
  var exchanges: List[FakeModel.Exchange] = Nil

  def request
    ( url: Text, method: Http.Method, headers: List[Http.Header], body: Spring[Data]^ )
    ( using Tactic[Connect.Error] )
  :   Http.Response =

    val data = body().memoize
    val sent = if data.readable.isEmpty then Unset else data.read[Text]
    val path = url.skip(t"http://model.test".length)
    val attempt = exchanges.stdlib.size
    exchanges ::= FakeModel.Exchange(method, path, headers, sent)

    route(method, path, attempt)

object AnthropicTests extends Suite(m"Anthropic dialect tests"):
  import Llm.{Content, Role, Stop, Usage}

  val target: Anthropic = Anthropic(t"claude-sonnet-4-5", t"sk-test").on(url"http://model.test")

  def sent(fake: FakeModel): Json =
    fake.exchanges.stdlib.reverse.head.body.option.get.read[Json]

  def header(fake: FakeModel, name: Text): Optional[Text] =
    fake.exchanges.stdlib.reverse.head.headers.filter(_.key == name).prim.let(_.value)

  def run(): Unit =
    test(m"a one-shot ask decodes the reply"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.answer(t"Suur Munamägi"))
      val reply = target.session(llm.ask(t"Tallest mountain in Estonia?"))
      (reply.text, reply.stop, reply.usage, reply.id)
    . assert(_ == (t"Suur Munamägi", Stop.Ended, Usage(7, 13), t"msg_1"))

    test(m"the request carries the model, system and message"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.answer(t"yes"))
      target.prompted(t"Be terse.").limit(512).session(llm.ask(t"Ready?"))
      val json = sent(fake)

      ( json.model.as[Text], json.system.as[Text], json.max_tokens.as[Int],
        json.messages(0).role.as[Text], json.messages(0).content(0).text.as[Text] )
    . assert(_ == (t"claude-sonnet-4-5", t"Be terse.", 512, t"user", t"Ready?"))

    test(m"unset knobs are omitted from the request"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.answer(t"yes"))
      target.session(llm.ask(t"Ready?"))
      val body = fake.exchanges.stdlib.reverse.head.body.option.get

      ( body.contains(t"temperature"), body.contains(t"stop_sequences"),
        body.contains(t"tools") )
    . assert(_ == (false, false, false))

    test(m"the wire headers are sent"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.answer(t"yes"))
      target.session(llm.ask(t"Ready?"))
      (header(fake, t"x-api-key"), header(fake, t"anthropic-version"))
    . assert(_ == (t"sk-test", t"2023-06-01"))

    test(m"the endpoint is the Messages API"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.answer(t"yes"))
      target.session(llm.ask(t"Ready?"))
      val exchange = fake.exchanges.stdlib.reverse.head
      (exchange.method, exchange.path)
    . assert(_ == (Http.Post, t"/v1/messages"))

    test(m"a rate limit with retry-after is retried"):
      given fake: FakeModel = FakeModel: (_, _, attempt) =>
        if attempt == 0
        then Http.Response(Http.TooManyRequests, retryAfter = t"0"):
          t"""{"type": "error", "error": {"type": "rate_limit_error", "message": "slow down"}}"""
        else FakeModel.answer(t"eventually")

      val reply = target.session(llm.ask(t"Ready?"))
      (reply.text, fake.exchanges.stdlib.size)
    . assert(_ == (t"eventually", 2))

    test(m"an authentication failure raises Unauthorized with the status"):
      given fake: FakeModel = FakeModel: (_, _, _) =>
        FakeModel.failure(Http.Unauthorized, t"authentication_error", t"invalid x-api-key")

      val error = capture[Llm.Error](target.session(llm.ask(t"Ready?")))
      (error.reason, error.status)
    . assert(_ == (Llm.Error.Reason.Unauthorized, 401))

    test(m"an unrecognized error code is preserved under Provider"):
      given fake: FakeModel = FakeModel: (_, _, _) =>
        FakeModel.failure(Http.InternalServerError, t"novel_error", t"strange")

      capture[Llm.Error](target.session(llm.ask(t"Ready?"))).reason
    . assert(_ == Llm.Error.Reason.Provider(t"novel_error"))

    test(m"a streamed turn assembles text, usage and identity"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.reply(FakeModel.streamed))

      val reply = target.session(llm.stream(t"go").reply())
      (reply.text, reply.usage, reply.id, reply.stop)
    . assert(_ == (t"fjord of Norway", Usage(11, 9), t"msg_2", Stop.Ended))

    test(m"streamed deltas arrive incrementally"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.reply(FakeModel.streamed))

      target.session(llm.stream(t"go").text.to(List))
    . assert(_ == List(t"fjord of ", t"Norway"))

    test(m"a streamed request asks for a stream"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.reply(FakeModel.streamed))

      target.session(llm.stream(t"go").reply())
      sent(fake).stream.as[Boolean]
    . assert(_ == true)

    test(m"streamed tool arguments assemble across deltas"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.reply(FakeModel.streamedTool))

      val reply = target.session(llm.stream(t"go").reply())
      (reply.stop, reply.toolCalls)
    . assert:
        _ == (Stop.ToolCall, List(Content.ToolUse(t"toolu_1", t"price", j"""{"ticker": "AAPL"}""")))

    test(m"a tool result round-trips to the wire shape"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.answer(t"noted"))

      target.session:
        llm.ask
          ( Llm.Message
              ( Role.User,
                List(Content.ToolResult(t"toolu_1", List(Content.Textual(t"42.5")))) ) )

      val block = sent(fake).messages(0).content(0)
      (block.`type`.as[Text], block.tool_use_id.as[Text], block.content(0).text.as[Text])
    . assert(_ == (t"tool_result", t"toolu_1", t"42.5"))

    test(m"countTokens reads the count"):
      given fake: FakeModel = FakeModel: (_, path, _) =>
        if path == t"/v1/messages/count_tokens" then FakeModel.reply(t"""{"input_tokens": 42}""")
        else FakeModel.answer(t"no")

      target.countTokens(List(Llm.Message(Role.User, t"hello")))
    . assert(_ == 42)
