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

import dynamicAccess.dynamicJson
import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely

case class Verdict(ticker: Text, rating: Text)

object Broker:
  @ability
  @about("Look up the current price of a stock ticker")
  def price(ticker: Text): Double = if ticker == "AAPL" then 211.5 else 100.0

  @ability
  def shout(message: Text)(using suffix: Text): Text = t"$message$suffix"

object ToolkitTests extends Suite(m"Toolkit and tool-loop tests"):
  import Llm.{Content, Message, Role, Settings, Stop, Usage}

  given suffix: Text = "!"
  val kit: Toolkit = Toolkit(Broker)

  def session(consume dialect: Llm.Dialect^, settings: Settings = Settings()): Llm.Session^ =
    Llm.Session(dialect, Unset, List(), settings, List())

  def calling(id: Text, tool: Text, arguments: Json): Llm.Reply =
    Llm.Reply
      ( Message(Role.Assistant, List(Content.ToolUse(id, tool, arguments))),
        Stop.ToolCall,
        Usage(1, 1) )

  def run(): Unit =
    test(m"a toolkit's specs carry names, descriptions and schemas"):
      val spec = kit.specs.prim.option.get
      val properties = spec.parameters.in[Json].properties

      (spec.name, spec.description, properties.ticker.`type`.as[Text])
    . assert(_ == ("price", "Look up the current price of a stock ticker", "string"))

    test(m"invoking a tool decodes arguments and encodes the result"):
      kit.invoke("price", j"""{"ticker": "AAPL"}""")
    . assert(_ == j"211.5")

    test(m"a contextual parameter is summoned at the construction site"):
      kit.invoke("shout", j"""{"message": "hello"}""")
    . assert(_ == j""""hello!"""")

    test(m"an unknown tool raises Invalid"):
      capture[Llm.Error](kit.invoke("missing", j"{}")).reason
    . assert(_ == Llm.Error.Reason.Invalid)

    test(m"the ambient toolkit's specs are offered to the model"):
      given Toolkit = kit
      val dialect = Scripted(List(Scripted.reply(t"fine")))
      session(dialect).ask("Ready?")
      dialect.calls.head.tools.map(_.name)
    . assert(_ == List(t"price", t"shout"))

    test(m"the loop runs a called tool and reports its result"):
      given Toolkit = kit

      val dialect = Scripted:
        List
          ( calling("c1", "price", j"""{"ticker": "AAPL"}"""),
            Scripted.reply("It costs quite a lot.") )

      val handle = session(dialect)
      val reply = handle.ask("Price AAPL?")
      val followup = dialect.calls.last.history.stdlib.last.content

      (reply.text, dialect.calls.size, followup)
    . assert:
        _ == ( "It costs quite a lot.", 2,
               List(Content.ToolResult(t"c1", List(Content.Textual(t"211.5")))) )

    test(m"the loop commits every turn to history"):
      given Toolkit = kit

      val dialect = Scripted:
        List
          ( calling("c1", "price", j"""{"ticker": "AAPL"}"""),
            Scripted.reply("Done.") )

      val handle = session(dialect)
      handle.ask("Price AAPL?")
      handle.history.stdlib.size
    . assert(_ == 4)

    test(m"malformed arguments become an is_error result the model can see"):
      given Toolkit = kit

      val dialect = Scripted:
        List
          ( calling("c1", "price", j"""{"wrong": true}"""),
            Scripted.reply("Sorry.") )

      val handle = session(dialect)
      handle.ask("Price AAPL?")

      dialect.calls.last.history.stdlib.last.content.stdlib.head match
        case Content.ToolResult(id, _, failure) => (id, failure)
        case other                              => ("?", false)
    . assert(_ == ("c1", true))

    test(m"an unanswerable loop raises ToolLoopExceeded"):
      given Toolkit = kit

      val dialect = Scripted:
        List
          ( calling("c1", "price", j"""{"ticker": "AAPL"}"""),
            calling("c2", "price", j"""{"ticker": "AAPL"}""") )

      val handle = session(dialect, Settings(iterations = 1))
      capture[Llm.Error](handle.ask("Price AAPL?")).reason
    . assert(_ == Llm.Error.Reason.ToolLoopExceeded)

    test(m"elicit decodes the forced tool call's arguments"):
      val dialect =
        Scripted(List(calling(t"a1", t"answer", j"""{"ticker": "AAPL", "rating": "buy"}""")))

      session(dialect).elicit[Verdict]("Summarise your recommendation.")
    . assert(_ == Verdict("AAPL", "buy"))

    test(m"elicit forces the synthetic answer tool"):
      val dialect =
        Scripted(List(calling(t"a1", t"answer", j"""{"ticker": "AAPL", "rating": "buy"}""")))

      val handle = session(dialect)
      handle.elicit[Verdict]("Summarise.")
      val turn = dialect.calls.head
      (turn.tools.map(_.name), turn.settings.toolChoice)
    . assert(_ == (List(t"answer"), Llm.ToolChoice.Named("answer")))

    test(m"a reply that ignores the forced tool raises Malformed"):
      val dialect = Scripted(List(Scripted.reply(t"I refuse to be structured.")))
      capture[Llm.Error](session(dialect).elicit[Verdict]("Summarise.")).reason
    . assert(_ == Llm.Error.Reason.Malformed)
