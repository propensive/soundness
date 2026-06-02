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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package apoplexy

import language.dynamics

import anticipation.*
import fulminate.*
import hellenism.*
import jacinta.*
import prepositional.*
import telekinesis.*
import vacuous.*

object Api:
  // Root constructor: `Api(cp"/spec.json")` reads the spec resource's `Locus`,
  // validates the spec at compile time, and returns `Api at "/"` carrying the
  // spec source so navigation macros can re-read it.
  transparent inline def apply(inline resource: Resource): Any = ${Apoplexy.root('resource)}

  def make(apiRequest: Api.Request): Api = new Api:
    def request: Api.Request = apiRequest

  // The runtime description of a navigated/invoked call. `base` is the server
  // URL from the spec; `path` is the still-templated path; `substitutions`
  // binds path templates to concrete values; `query` is the query string;
  // `body` is an optional JSON request body.
  case class Request
    ( method:        Http.Method,
      base:          Text,
      path:          Text,
      substitutions: Map[Text, Text]    = Map(),
      query:         List[(Text, Text)] = Nil,
      body:          Optional[Json]     = Unset )

  // The result of invoking an endpoint. Its refined type records `Result` (a
  // JSON-pointer to the 2xx response schema) and `Form` (the spec source),
  // which `as` reads to check a target type for conformance against the schema.
  object Response:
    def make(apiRequest: Api.Request): Api.Response = new Api.Response:
      def request: Api.Request = apiRequest

  trait Response:
    type Result
    type Form
    def request: Api.Request

    transparent inline def as[value]: value = ${Apoplexy.decode[value]('this)}

  object Error:
    object Reason:
      given Reason is Communicable =
        case Status(code) => m"the server responded with an unsuccessful status, $code"

    enum Reason(val number: Int) extends Clarification:
      case Status(code: Int) extends Reason(1)

  case class Error(reason: Api.Error.Reason)(using Diagnostics)
  extends fulminate.Error(914, reason.number)(m"the API request was not successful because $reason")

trait Api extends Dynamic, Locatable:
  def request: Api.Request

  transparent inline def selectDynamic(field: String): Any =
    ${Apoplexy.select('this, 'field)}

  transparent inline def applyDynamic(field: String)(inline args: Any*): Any =
    ${Apoplexy.applied('this, 'field, 'args)}

  transparent inline def applyDynamicNamed(field: String)(inline args: (String, Any)*): Any =
    ${Apoplexy.appliedNamed('this, 'field, 'args)}
