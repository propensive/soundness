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

import charEncoders.utf8Encoder
import dynamicJsonAccess.enabled
import errorDiagnostics.stackTracesDiagnostics
import internetAccess.online
import logging.silentLogging
import strategies.throwUnsafely

object OpenAITests extends Suite(m"OpenAI dialect tests"):
  import Llm.{Content, Role, Stop, Usage}

  val target: OpenAI = OpenAI.compatible(url"http://model.test/v1", t"gpt-test", t"sk-test")

  def sent(fake: FakeModel): Json =
    fake.exchanges.stdlib.reverse.head.body.option.get.read[Json]

  def answer(text: Text): Http.Response = FakeModel.reply:
    t"""{"id": "cmpl_1", "model": "gpt-test", "choices":
          [{"index": 0, "message": {"role": "assistant", "content": "$text"},
            "finish_reason": "stop"}],
         "usage": {"prompt_tokens": 3, "completion_tokens": 5}}"""

  val toolAnswer: Http.Response = FakeModel.reply:
    t"""{"id": "cmpl_2", "model": "gpt-test", "choices":
          [{"index": 0,
            "message": {"role": "assistant", "content": null, "tool_calls":
              [{"id": "call_1", "type": "function", "function":
                 {"name": "price", "arguments": "{\\"ticker\\": \\"AAPL\\"}"}}]},
            "finish_reason": "tool_calls"}],
         "usage": {"prompt_tokens": 4, "completion_tokens": 6}}"""

  // A streamed completion: chunked text, a finish-reason chunk, the `include_usage` final
  // chunk, and the `[DONE]` sentinel.
  val streamed: Text = scala.List
    ( t"""data: {"id": "cmpl_3", "model": "gpt-test", "choices": [{"index": 0, "delta": {"role": "assistant", "content": "fjord"}}]}""",
      t"""""",
      t"""data: {"id": "cmpl_3", "model": "gpt-test", "choices": [{"index": 0, "delta": {"content": " of Norway"}}]}""",
      t"""""",
      t"""data: {"id": "cmpl_3", "model": "gpt-test", "choices": [{"index": 0, "delta": {}, "finish_reason": "stop"}]}""",
      t"""""",
      t"""data: {"id": "cmpl_3", "model": "gpt-test", "choices": [], "usage": {"prompt_tokens": 11, "completion_tokens": 9}}""",
      t"""""",
      t"""data: [DONE]""",
      t"""""" )
  . mkString("\n").tt

  // A streamed tool call whose arguments arrive in fragments.
  val streamedTool: Text = scala.List
    ( t"""data: {"id": "cmpl_4", "model": "gpt-test", "choices": [{"index": 0, "delta": {"role": "assistant", "tool_calls": [{"index": 0, "id": "call_2", "function": {"name": "price", "arguments": ""}}]}}]}""",
      t"""""",
      t"""data: {"id": "cmpl_4", "model": "gpt-test", "choices": [{"index": 0, "delta": {"tool_calls": [{"index": 0, "function": {"arguments": "{\\"tick"}}]}}]}""",
      t"""""",
      t"""data: {"id": "cmpl_4", "model": "gpt-test", "choices": [{"index": 0, "delta": {"tool_calls": [{"index": 0, "function": {"arguments": "er\\": \\"AAPL\\"}"}}]}}]}""",
      t"""""",
      t"""data: {"id": "cmpl_4", "model": "gpt-test", "choices": [{"index": 0, "delta": {}, "finish_reason": "tool_calls"}]}""",
      t"""""",
      t"""data: [DONE]""",
      t"""""" )
  . mkString("\n").tt

  def run(): Unit =
    test(m"a one-shot ask decodes the reply"):
      given fake: FakeModel = FakeModel((_, _, _) => answer(t"Suur Munamägi"))
      val reply = target.session(llm.ask(t"Tallest mountain in Estonia?"))
      (reply.text, reply.stop, reply.usage, reply.id)
    . assert(_ == (t"Suur Munamägi", Stop.Ended, Usage(3, 5), t"cmpl_1"))

    test(m"the request goes to the configured base with Bearer auth"):
      given fake: FakeModel = FakeModel((_, _, _) => answer(t"yes"))
      target.session(llm.ask(t"Ready?"))
      val exchange = fake.exchanges.stdlib.reverse.head

      ( exchange.path,
        exchange.headers.filter(_.key == t"authorization").prim.let(_.value) )
    . assert(_ == (t"/v1/chat/completions", t"Bearer sk-test"))

    test(m"a keyless compatible target sends no authorization header"):
      given fake: FakeModel = FakeModel((_, _, _) => answer(t"yes"))
      OpenAI.compatible(url"http://model.test/v1", t"llama").session(llm.ask(t"Ready?"))
      fake.exchanges.stdlib.reverse.head.headers.filter(_.key == t"authorization").stdlib.size
    . assert(_ == 0)

    test(m"the system prompt becomes a leading system message"):
      given fake: FakeModel = FakeModel((_, _, _) => answer(t"yes"))
      target.prompted(t"Be terse.").session(llm.ask(t"Ready?"))
      val json = sent(fake)
      (json.messages(0).role.as[Text], json.messages(0).content.as[Text])
    . assert(_ == (t"system", t"Be terse."))

    test(m"a tool-call reply parses its string arguments"):
      given fake: FakeModel = FakeModel((_, _, _) => toolAnswer)
      val reply = target.session(llm.ask(t"Price AAPL"))
      (reply.stop, reply.toolCalls)
    . assert:
        _ == (Stop.ToolCall, List(Content.ToolUse(t"call_1", t"price", j"""{"ticker": "AAPL"}""")))

    test(m"tool history round-trips as tool_calls and tool messages"):
      given fake: FakeModel = FakeModel((_, _, _) => answer(t"noted"))

      val history = List
        ( Llm.Message
            ( Role.Assistant,
              List(Content.ToolUse(t"call_1", t"price", j"""{"ticker": "AAPL"}""")) ),
          Llm.Message(Role.User, List(Content.ToolResult(t"call_1", List(Content.Textual(t"42"))))) )

      target.primed(history).session(llm.ask(t"So?"))
      val json = sent(fake)

      ( json.messages(0).tool_calls(0).function.name.as[Text],
        json.messages(1).role.as[Text],
        json.messages(1).tool_call_id.as[Text],
        json.messages(2).role.as[Text] )
    . assert(_ == (t"price", t"tool", t"call_1", t"user"))

    test(m"a streamed turn assembles text, usage and identity"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.reply(streamed))
      val reply = target.session(llm.stream(t"go").reply())
      (reply.text, reply.usage, reply.id, reply.stop)
    . assert(_ == (t"fjord of Norway", Usage(11, 9), t"cmpl_3", Stop.Ended))

    test(m"a streamed request asks for usage"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.reply(streamed))
      target.session(llm.stream(t"go").reply())
      sent(fake).stream_options.include_usage.as[Boolean]
    . assert(_ == true)

    test(m"streamed tool-call arguments assemble across chunks"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.reply(streamedTool))
      val reply = target.session(llm.stream(t"go").reply())
      (reply.stop, reply.toolCalls)
    . assert:
        _ == (Stop.ToolCall, List(Content.ToolUse(t"call_2", t"price", j"""{"ticker": "AAPL"}""")))

    test(m"an error envelope maps through the status"):
      given fake: FakeModel = FakeModel: (_, _, _) =>
        Http.Response(Http.Unauthorized):
          t"""{"error": {"message": "bad key", "type": "invalid_request_error", "code": "invalid_api_key"}}"""

      capture[Llm.Error](target.session(llm.ask(t"Ready?"))).reason
    . assert(_ == Llm.Error.Reason.Unauthorized)

object ResponsesTests extends Suite(m"OpenAI Responses dialect tests"):
  import Llm.{Content, Stop, Usage}

  val target: OpenAI.Responses =
    OpenAI.compatible(url"http://model.test/v1", t"gpt-test", t"sk-test").responses

  def sent(fake: FakeModel): Json =
    fake.exchanges.stdlib.reverse.head.body.option.get.read[Json]

  def answer(text: Text): Http.Response = FakeModel.reply:
    t"""{"id": "resp_1", "model": "gpt-test", "status": "completed", "output":
          [{"type": "message", "role": "assistant", "content":
             [{"type": "output_text", "text": "$text"}]}],
         "usage": {"input_tokens": 3, "output_tokens": 5}}"""

  val streamed: Text = scala.List
    ( t"""event: response.created""",
      t"""data: {"type": "response.created", "response": {"id": "resp_2", "model": "gpt-test"}}""",
      t"""""",
      t"""event: response.output_item.added""",
      t"""data: {"type": "response.output_item.added", "output_index": 0, "item": {"type": "message", "role": "assistant"}}""",
      t"""""",
      t"""event: response.output_text.delta""",
      t"""data: {"type": "response.output_text.delta", "output_index": 0, "delta": "fjord"}""",
      t"""""",
      t"""event: response.output_text.delta""",
      t"""data: {"type": "response.output_text.delta", "output_index": 0, "delta": " of Norway"}""",
      t"""""",
      t"""event: response.output_item.done""",
      t"""data: {"type": "response.output_item.done", "output_index": 0, "item": {"type": "message"}}""",
      t"""""",
      t"""event: response.completed""",
      t"""data: {"type": "response.completed", "response": {"id": "resp_2", "usage": {"input_tokens": 11, "output_tokens": 9}}}""",
      t"""""" )
  . mkString("\n").tt

  def run(): Unit =
    test(m"a one-shot ask decodes output items"):
      given fake: FakeModel = FakeModel((_, _, _) => answer(t"Suur Munamägi"))
      val reply = target.session(llm.ask(t"Tallest mountain in Estonia?"))
      (reply.text, reply.stop, reply.usage, reply.id)
    . assert(_ == (t"Suur Munamägi", Stop.Ended, Usage(3, 5), t"resp_1"))

    test(m"the request uses input items and instructions"):
      given fake: FakeModel = FakeModel((_, _, _) => answer(t"yes"))
      target.chat.prompted(t"Be terse.").responses.session(llm.ask(t"Ready?"))
      val json = sent(fake)
      val exchange = fake.exchanges.stdlib.reverse.head

      ( exchange.path, json.instructions.as[Text], json.input(0).`type`.as[Text],
        json.input(0).content(0).`type`.as[Text] )
    . assert(_ == (t"/v1/responses", t"Be terse.", t"message", t"input_text"))

    test(m"a function_call output becomes a ToolCall reply"):
      given fake: FakeModel = FakeModel: (_, _, _) =>
        FakeModel.reply:
          t"""{"id": "resp_3", "model": "gpt-test", "status": "completed", "output":
                [{"type": "function_call", "call_id": "call_9", "name": "price",
                  "arguments": "{\\"ticker\\": \\"MSFT\\"}"}],
               "usage": {"input_tokens": 2, "output_tokens": 3}}"""

      val reply = target.session(llm.ask(t"Price MSFT"))
      (reply.stop, reply.toolCalls)
    . assert:
        _ == (Stop.ToolCall, List(Content.ToolUse(t"call_9", t"price", j"""{"ticker": "MSFT"}""")))

    test(m"a streamed turn assembles from semantic events"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.reply(streamed))
      val reply = target.session(llm.stream(t"go").reply())
      (reply.text, reply.usage, reply.id)
    . assert(_ == (t"fjord of Norway", Usage(11, 9), t"resp_2"))

object GeminiTests extends Suite(m"Gemini dialect tests"):
  import Llm.{Content, Role, Stop, Usage}

  val target: Gemini = Gemini(t"gemini-test", t"g-key").on(url"http://model.test")

  def sent(fake: FakeModel): Json =
    fake.exchanges.stdlib.reverse.head.body.option.get.read[Json]

  def answer(text: Text): Http.Response = FakeModel.reply:
    t"""{"responseId": "gen_1", "modelVersion": "gemini-test", "candidates":
          [{"content": {"role": "model", "parts": [{"text": "$text"}]},
            "finishReason": "STOP"}],
         "usageMetadata": {"promptTokenCount": 3, "candidatesTokenCount": 5}}"""

  val streamed: Text = scala.List
    ( t"""data: {"responseId": "gen_2", "modelVersion": "gemini-test", "candidates": [{"content": {"role": "model", "parts": [{"text": "fjord"}]}}], "usageMetadata": {"promptTokenCount": 11, "candidatesTokenCount": 2}}""",
      t"""""",
      t"""data: {"responseId": "gen_2", "candidates": [{"content": {"role": "model", "parts": [{"text": " of Norway"}]}, "finishReason": "STOP"}], "usageMetadata": {"promptTokenCount": 11, "candidatesTokenCount": 9}}""",
      t"""""" )
  . mkString("\n").tt

  def run(): Unit =
    test(m"a one-shot ask decodes the candidate"):
      given fake: FakeModel = FakeModel((_, _, _) => answer(t"Suur Munamägi"))
      val reply = target.session(llm.ask(t"Tallest mountain in Estonia?"))
      (reply.text, reply.stop, reply.usage, reply.id)
    . assert(_ == (t"Suur Munamägi", Stop.Ended, Usage(3, 5), t"gen_1"))

    test(m"the endpoint addresses the model and the key travels as a header"):
      given fake: FakeModel = FakeModel((_, _, _) => answer(t"yes"))
      target.session(llm.ask(t"Ready?"))
      val exchange = fake.exchanges.stdlib.reverse.head

      ( exchange.path,
        exchange.headers.filter(_.key == t"x-goog-api-key").prim.let(_.value) )
    . assert(_ == (t"/v1beta/models/gemini-test:generateContent", t"g-key"))

    test(m"the streamed endpoint asks for SSE"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.reply(streamed))
      target.session(llm.stream(t"go").reply())
      fake.exchanges.stdlib.reverse.head.path
    . assert(_ == t"/v1beta/models/gemini-test:streamGenerateContent?alt=sse")

    test(m"the request carries contents, system instruction and config"):
      given fake: FakeModel = FakeModel((_, _, _) => answer(t"yes"))
      target.prompted(t"Be terse.").limit(256).session(llm.ask(t"Ready?"))
      val json = sent(fake)

      ( json.contents(0).role.as[Text], json.contents(0).parts(0).text.as[Text],
        json.systemInstruction.parts(0).text.as[Text],
        json.generationConfig.maxOutputTokens.as[Int] )
    . assert(_ == (t"user", t"Ready?", t"Be terse.", 256))

    test(m"a functionCall part becomes a ToolCall reply keyed by name"):
      given fake: FakeModel = FakeModel: (_, _, _) =>
        FakeModel.reply:
          t"""{"responseId": "gen_3", "modelVersion": "gemini-test", "candidates":
                [{"content": {"role": "model", "parts":
                   [{"functionCall": {"name": "price", "args": {"ticker": "GOOG"}}}]},
                  "finishReason": "STOP"}],
               "usageMetadata": {"promptTokenCount": 2, "candidatesTokenCount": 3}}"""

      val reply = target.session(llm.ask(t"Price GOOG"))
      (reply.stop, reply.toolCalls)
    . assert:
        _ == (Stop.ToolCall, List(Content.ToolUse(t"price", t"price", j"""{"ticker": "GOOG"}""")))

    test(m"a tool result encodes as a functionResponse part"):
      given fake: FakeModel = FakeModel((_, _, _) => answer(t"noted"))

      target.session:
        llm.ask
          ( Llm.Message
              ( Role.User,
                List(Content.ToolResult(t"price", List(Content.Textual(t"42")))) ) )

      val part = sent(fake).contents(0).parts(0)
      (part.functionResponse.name.as[Text], part.functionResponse.response.result.as[Text])
    . assert(_ == (t"price", t"42"))

    test(m"a streamed turn accumulates text and takes the final usage"):
      given fake: FakeModel = FakeModel((_, _, _) => FakeModel.reply(streamed))
      val reply = target.session(llm.stream(t"go").reply())
      (reply.text, reply.usage, reply.id, reply.stop)
    . assert(_ == (t"fjord of Norway", Usage(11, 9), t"gen_2", Stop.Ended))

    test(m"a Google error envelope maps to the status"):
      given fake: FakeModel = FakeModel: (_, _, _) =>
        Http.Response(Http.TooManyRequests, retryAfter = t"0"):
          t"""{"error": {"code": 429, "message": "quota", "status": "RESOURCE_EXHAUSTED"}}"""

      capture[Llm.Error](target.session(llm.ask(t"Ready?"))).reason
    . assert(_ == Llm.Error.Reason.RateLimited)
