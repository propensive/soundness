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

import scala.collection.mutable as scm

import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely

object Scripted:
  def reply(text: Text, usage: Llm.Usage = Llm.Usage(3, 5)): Llm.Reply =
    Llm.Reply(Llm.Message(Llm.Role.Assistant, text), Llm.Stop.Ended, usage)

// A scripted dialect: answers each exchange from a queue and records what it was asked, so the
// session's history handling, commit atomicity and accumulation are exercised with no provider,
// no HTTP and no socket.
class Scripted
  ( answers: List[Llm.Reply] = List(), scripts: List[List[Llm.Event]] = List() )
extends Llm.Dialect:
  val calls: scm.ListBuffer[Llm.Exchange] = scm.ListBuffer()
  private val replies: scm.Queue[Llm.Reply] = scm.Queue(answers.stdlib*)
  private val streams: scm.Queue[List[Llm.Event]] = scm.Queue(scripts.stdlib*)

  def name: Text = t"scripted"

  def exchange(turn: Llm.Exchange): Llm.Reply =
    calls.append(turn)
    replies.dequeue()

  def stream(turn: Llm.Exchange): Iterator[Llm.Event]^{this} =
    calls.append(turn)
    streams.dequeue().stdlib.iterator

object Tests extends Suite(m"Sibylline tests"):
  import Llm.{Content, Event, Message, Role, Settings, Stop, Usage}
  import Event.Increment

  def session(consume dialect: Llm.Dialect^, system: Optional[Text] = Unset): Llm.Session^ =
    Llm.Session(dialect, system, List(), Settings(), List())

  def run(): Unit =
    suite(m"Conversation tests"):
      test(m"a one-shot ask returns the scripted reply"):
        val dialect = Scripted(List(Scripted.reply(t"Suur Munamägi")))
        session(dialect).ask(t"Tallest mountain in Estonia?").text
      . assert(_ == t"Suur Munamägi")

      test(m"asking commits both turns to history"):
        val dialect = Scripted(List(Scripted.reply(t"Suur Munamägi")))
        val handle = session(dialect)
        handle.ask(t"Tallest mountain in Estonia?")
        handle.history.stdlib.length
      . assert(_ == 2)

      test(m"a second ask sends the accumulated history"):
        val dialect = Scripted(List(Scripted.reply(t"one"), Scripted.reply(t"two")))
        val handle = session(dialect)
        handle.ask(t"first")
        handle.ask(t"second")
        dialect.calls.last.history.stdlib.length
      . assert(_ == 3)

      test(m"usage folds across turns"):
        val dialect =
          Scripted(List(Scripted.reply(t"one", Usage(3, 5)), Scripted.reply(t"two", Usage(7, 11))))

        val handle = session(dialect)
        handle.ask(t"first")
        handle.ask(t"second")
        handle.usage
      . assert(_ == Usage(10, 16))

      test(m"recorded messages are sent but cost nothing"):
        val dialect = Scripted(List(Scripted.reply(t"seen")))
        val handle = session(dialect)
        handle.record(Message(Role.User, t"context"))
        handle.record(Message(Role.Assistant, t"noted"))
        handle.ask(t"question")
        (dialect.calls.last.history.stdlib.length, handle.usage.input)
      . assert(_ == (3, 3))

      test(m"the system prompt reaches the dialect out-of-band"):
        val dialect = Scripted(List(Scripted.reply(t"yes")))
        val handle = session(dialect, system = t"Be terse.")
        handle.ask(t"Ready?")
        dialect.calls.last.system
      . assert(_ == t"Be terse.")

    suite(m"Vocabulary tests"):
      test(m"absent optional counts stay absent when folded"):
        Usage(1, 2) + Usage(3, 4)
      . assert(_ == Usage(4, 6))

      test(m"present optional counts fold into totals"):
        Usage(1, 2, cacheRead = 10) + Usage(3, 4, cacheRead = 5, reasoning = 7)
      . assert(_ == Usage(4, 6, cacheRead = 15, reasoning = 7))

      test(m"a reply's text concatenates only its textual blocks"):
        Scripted.reply(t"one").copy
          ( message = Message
              ( Role.Assistant,
                List
                  ( Content.Textual(t"one "),
                    Content.Thinking(t"hmm"),
                    Content.Textual(t"two") ) ) )
        . text
      . assert(_ == t"one two")

      test(m"a reply's tool calls are just its tool-use blocks"):
        val call = Content.ToolUse(t"id1", t"price", j"""{"ticker": "AAPL"}""")

        Scripted.reply(t"x").copy
          ( message = Message(Role.Assistant, List(Content.Textual(t"…"), call)) )
        . toolCalls
      . assert(_ == List(Content.ToolUse(t"id1", t"price", j"""{"ticker": "AAPL"}""")))

    suite(m"Streaming tests"):
      def script(chunks: Text*): List[Event] =
        val head = scala.Seq(Event.Started(t"msg_1", t"scripted-1"), Event.Opened(0, Content.Textual(t"")))
        val tail = scala.Seq(Event.Closed(0), Event.Update(Stop.Ended, Usage(2, 9)), Event.Finished)
        List.of((head ++ chunks.map { chunk => Event.Delta(0, Increment.Textual(chunk)) } ++ tail).toList)

      test(m"text deltas stream in order"):
        val dialect = Scripted(scripts = List(script(t"fjord", t" of ", t"Norway")))
        val handle = session(dialect)
        val response = handle.stream(t"go")
        response.text.to(List)
      . assert(_ == List(t"fjord", t" of ", t"Norway"))

      test(m"draining a response assembles the full reply"):
        val dialect = Scripted(scripts = List(script(t"fjord", t" of ", t"Norway")))
        val handle = session(dialect)
        handle.stream(t"go").reply().text
      . assert(_ == t"fjord of Norway")

      test(m"a partially-consumed stream still completes on reply"):
        val dialect = Scripted(scripts = List(script(t"fjord", t" of ", t"Norway")))
        val handle = session(dialect)
        val response = handle.stream(t"go")
        response.text.next()
        response.reply().text
      . assert(_ == t"fjord of Norway")

      test(m"reply is idempotent"):
        val dialect = Scripted(scripts = List(script(t"fjord")))
        val handle = session(dialect)
        val response = handle.stream(t"go")
        (response.reply(), response.reply())
      . assert { case (first, second) => first == second }

      test(m"draining commits both turns and the usage"):
        val dialect = Scripted(scripts = List(script(t"fjord")))
        val handle = session(dialect)
        handle.stream(t"go").reply()
        (handle.history.stdlib.length, handle.usage)
      . assert(_ == (2, Usage(2, 9)))

      test(m"an abandoned stream commits nothing"):
        val dialect = Scripted(scripts = List(script(t"fjord")))
        val handle = session(dialect)
        handle.stream(t"go")
        handle.history.stdlib.length
      . assert(_ == 0)

      test(m"the streamed reply carries its id and model"):
        val dialect = Scripted(scripts = List(script(t"fjord")))
        val handle = session(dialect)
        val reply = handle.stream(t"go").reply()
        (reply.id, reply.model)
      . assert(_ == (t"msg_1", t"scripted-1"))

      test(m"streamed tool arguments parse when the block closes"):
        val events: List[Event] =
          List
            ( Event.Started(Unset, Unset),
              Event.Opened(0, Content.ToolUse(t"id1", t"price", j"{}")),
              Event.Delta(0, Increment.Arguments(t"""{"tick""")),
              Event.Delta(0, Increment.Arguments(t"""er": "AAPL"}""")),
              Event.Closed(0),
              Event.Update(Stop.ToolCall, Unset),
              Event.Finished )

        val dialect = Scripted(scripts = List(events))
        val handle = session(dialect)
        val reply = handle.stream(t"go").reply()
        (reply.stop, reply.toolCalls)
      . assert(_ == (Stop.ToolCall, List(Content.ToolUse(t"id1", t"price", j"""{"ticker": "AAPL"}"""))))

      test(m"a stream that ends early raises Interrupted"):
        val events: List[Event] =
          List
            ( Event.Started(Unset, Unset),
              Event.Opened(0, Content.Textual(t"")),
              Event.Delta(0, Increment.Textual(t"fjo")) )

        val dialect = Scripted(scripts = List(events))
        val handle = session(dialect)
        capture[Llm.Error](handle.stream(t"go").reply()).reason
      . assert(_ == Llm.Error.Reason.Interrupted)

      test(m"an unclosed block raises Interrupted even when finished"):
        val events: List[Event] =
          List
            ( Event.Started(Unset, Unset),
              Event.Opened(0, Content.Textual(t"")),
              Event.Delta(0, Increment.Textual(t"fjo")),
              Event.Finished )

        val dialect = Scripted(scripts = List(events))
        val handle = session(dialect)
        capture[Llm.Error](handle.stream(t"go").reply()).reason
      . assert(_ == Llm.Error.Reason.Interrupted)

    AnthropicTests()
    OpenAITests()
    ResponsesTests()
    GeminiTests()
    ToolkitTests()
    CaptureTests()
