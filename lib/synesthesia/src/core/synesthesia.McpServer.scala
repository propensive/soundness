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
┃    Soundness, version 0.53.0.                                                                    ┃
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
package synesthesia

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import distillate.*
import eucalyptus.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import inimitable.*
import jacinta.*
import obligatory.*
import parasite.*
import prepositional.*
import proscenium.*
import revolution.*
import rudiments.*
import spectacular.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

import scala.annotation.*
import scala.quoted.*

import errorDiagnostics.stackTraces

object McpServer:
  given associable: McpServer is Associable:
    type Operand = Http.Request
    type Target = Http.Response

    def association(request: Http.Request): McpServer =
      given mcpSessionId: ("mcpSessionId" is Directive of Text) = identity(_)
      request.headers.mcpSessionId.prim.let(McpServer(_)).or(McpServer(Uuid().encode))

    def associate(session: McpServer)(response: Http.Response): Http.Response =
      response + Http.Header("mcp-session-id", session.id)


  private val sessions: scm.HashMap[Text, McpServer] = scm.HashMap()

  def apply(session: Text): McpServer = sessions.establish(session)(new MyMcpServer(session))

abstract class McpServer(val id: Text):
  import Mcp.*

  def name: Text
  def description: Text
  def version: Semver
  def prompts: List[Prompt]

object McpInterface:
  given streamable: McpInterface is Streamable by Sse = _.stream

  inline def tools[interface <: McpServer]: List[Mcp.Tool] = ${Synesthesia.tools[interface]}

  inline def apply(server: McpServer): McpInterface =
    new McpInterface(server, tools[server.type])


class McpInterface(val server: McpServer, val tools: List[Mcp.Tool]) extends Mcp.Api:
  import Mcp.*

  protected var loggingLevel: LoggingLevel = LoggingLevel.Info

  def ping(): Unit = ()

  def initialize
    ( protocolVersion: Text,
      capabilities:    ClientCapabilities,
      clientInfo:      Implementation,
      _meta:           Optional[Json] )
  : Mcp.Initialize =

      Mcp.Initialize
        ( "2025-11-25",
          ServerCapabilities(),
          Implementation(server.name, version = server.version.encode),
          server.description )
      . tap(println(_))

  def `completion/complete`
    ( ref:      Reference,
      argument: Argument,
      context:  Optional[Context],
      _meta:    Optional[Json] )
  : Complete =

      ???


  def `logging/setLevel`(level: LoggingLevel, _meta: Optional[Json]): Unit =
    loggingLevel = level

  def `prompts/get`(name: Text, arguments: Optional[Map[Text, Text]], _meta: Optional[Json]): Unit =
    ???

  def `prompts/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListPrompts =
    ListPrompts(Unset, server.prompts)

  def `resources/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListResources = ListResources(Nil)

  def `resources/templates/list`(cursor: Optional[Cursor], _meta: Optional[Json]): ListResourceTemplates = ???

  def `resources/read`(uri: Text, _meta: Optional[Json]): ReadResource = ???

  def `resources/subscribe`(uri: Text, _meta: Optional[Json]): Unit = ???

  def `resources/unsubscribe`(uri: Text, _meta: Optional[Json]): Unit = ???

  def `tools/call`(name: Text, arguments: Optional[Map[Text, Json]], _meta: Optional[Json]): CallTool = ???

  def `tools/list`(_meta: Optional[Json]): ListTools =
    ListTools(tools)
    . tap: tools =>
      import jsonPrinters.indented
      println(tools.json.show)

  def `tasks/get`(taskId: Text, _meta: Optional[Json]): Task = ???

  def `tasks/result`(taskId: Text, _meta: Optional[Json]): Map[Text, Json] = ???

  def `tasks/list`(_meta: Optional[Json]): ListTasks = ListTasks(Nil)

  def `notifications/cancelled`
    ( request: Optional[TextInt], reason: Optional[Text], _meta: Optional[Json] )
  : Unit =

      ???


  def `notifications/progress`
    ( progressToken: TextInt,
      progress:      Double,
      total:         Optional[Double],
      message:       Optional[Text],
      _meta:         Optional[Json] )
  : Unit =

      ???


  def `notifications/initialized`(_meta: Optional[Json]): Unit = ()

  def `notifications/resources/list_changed`(_meta: Optional[Json]): Unit = ???

  def `notifications/resources/updated`(uri: Text, _meta: Optional[Json]): Unit = ???

  def `tasks/cancel`(taskId: Text, _meta: Optional[Json]): Task = ???

  def `notifications/roots/list_changed`(_meta: Optional[Json]): Unit = ???

  def `notifications/tasks/status`
    ( taskId:        Text,
      status:        TaskStatus,
      statusMessage: Optional[Text],
      createdAt:     Text,
      lastUpdatedAt: Text,
      ttl:           Int,
      pollInterval:  Optional[Int],
      _meta:         Optional[Json] )
  : Unit = ???

object MyMcpServer:
  val sessions: scm.HashMap[Text, MyMcpServer] = scm.HashMap()

  def apply(id: Text): MyMcpServer = sessions.establish(id):
    new MyMcpServer(id)

  given associable: McpInterface is Associable:
    type Operand = Http.Request
    type Target = Http.Response

    def association(request: Http.Request): McpInterface =
      given mcpSessionId: ("mcpSessionId" is Directive of Text) = identity(_)
      McpInterface(request.headers.mcpSessionId.prim.let(MyMcpServer(_)).or(MyMcpServer(Uuid().encode)))

    def associate(session: McpInterface)(response: Http.Response): Http.Response =
      response + Http.Header("mcp-session-id", session.server.id)

class MyMcpServer(id: Text) extends McpServer(id):
  import Mcp.*

  def name: Text = "Pyrus"
  def description: Text = "A simple server"
  def version: Semver = v"1.0.0"
  def prompts: List[Prompt] = Nil

  @tool
  def color(name: Text): Text = "purple"
