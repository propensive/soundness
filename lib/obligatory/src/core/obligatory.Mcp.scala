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
package obligatory

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
import parasite.*
import prepositional.*
import revolution.*
import rudiments.*
import telekinesis.*
import urticose.*
import vacuous.*
import zephyrine.*

import scala.annotation.*
import scala.quoted.*

import errorDiagnostics.stackTraces

object Mcp:
  type Cursor = Text
  case class TaskAugmentedRequestParams(task: Optional[TaskMetadata])
  case class Error(code: Int, message: Text, data: Optional[Json])

  object TextInt:
    given encodable: TextInt is Encodable in Json = _.id match
      case text: Text => text.json
      case int: Int   => int.json

  case class TextInt(id: Text | Int)

  val ParseError = -32700
  val InvalidRequest = -32600
  val MethodNotFound = -32601
  val InvalidParams = -32602
  val InternalError = -32603
  val UrlElicitationRequired = -32042

  case class TaskMetadata(ttl: Optional[Int])

  case class Icon(src: HttpUrl, mimeType: Text, sizes: List[Text], theme: Optional[Text])
  case class Icons(icons: List[Icon])

  case class BaseMetadata(name: Text, title: Optional[Text])

  case class Implementation(version: Text, description: Optional[Text], websiteUrl: Optional[Text])

  case class ClientInfo
              (name:        Text,
               title:       Text,
               version:     Text,
               description: Text,
               icons:       List[Icon])

  case class Initialize
              (protocolVersion: Text,
               capabilities:    Json,
               serverInfo:      ServerInfo,
               instructions:    Text)

  case class ServerInfo
              (name:        Text,
               title:       Text,
               version:     Text,
               description: Text,
               icons:       List[Icon],
               websiteUrl:  HttpUrl)

  case class InitializeResult
              (protocolVersion: Text,
               capabilities:    ServerCapabilities,
               serverInfo:      Implementation,
               instructions:    Optional[Text])

  case class ListChanged(listChanged: Optional[Boolean])
  case class Resources(subscribe: Optional[Boolean], listChanged: Optional[Boolean])

  case class Sampling(context: Optional[Json], tools: Optional[Json])
  case class Elicitation(form: Optional[Json], url: Optional[Json])
  case class RequestsElicitation(create: Optional[Json])
  case class Call(call: Optional[Json])
  case class RequestsSampling(createMessage: Optional[Json])
  case class Tasks(list: Optional[Json], cancel: Optional[Json], requests: Optional[Requests])

  case class Requests
              (sampling:    Optional[RequestsSampling],
               elicitation: Optional[RequestsElicitation],
               tools:       Optional[Call])

  case class ServerCapabilities
              (experimental: Optional[Map[Text, Json]],
               logging:      Optional[Json],
               completions:  Optional[Json],
               prompts:      Optional[ListChanged],
               resources:    Optional[Resources],
               tools:        Optional[ListChanged],
               tasks:        Optional[Tasks])

  case class ClientCapabilities
              (experimental: Optional[Map[Text, Json]],
               roots:        Optional[ListChanged],
               sampling:     Optional[Sampling],
               elicitation:  Optional[Elicitation],
               tasks:        Optional[Tasks])

trait Mcp:

  import Mcp.*

  @remote
  def `notifications/cancelled`(request: Optional[TextInt], reason: Optional[Text]): Unit

  @remote
  def initialize(protocolVersion: Text, capabilities: ClientCapabilities, clientInfo: Implementation): InitializeResult

  @remote
  def `notifications/initialized`(): Unit

  @remote
  def ping(): Json

  @remote
  def `notifications/progress`
       (progressToken: TextInt,
        progress:      Double,
        total:         Optional[Double],
        message:       Optional[Text])
  : Unit
