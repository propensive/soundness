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
import contingency.*
import distillate.*
import gossamer.*
import hellenism.*
import hieroglyph.*
import jacinta.*
import prepositional.*
import spectacular.*
import telekinesis.*
import urticose.*
import vacuous.*

import jacinta.postables.jsonIsPostable

object Api:
  // Root constructor: `Api(cp"/spec.json")` reads the spec resource's `Locus`,
  // validates the spec at compile time, and returns `Api at "/"` carrying the
  // spec source so navigation macros can re-read it.
  transparent inline def apply(inline resource: Resource): Any = ${Apoplexy.root('resource)}

  def make(apiRequest: Api.Request): Api = new Api:
    def request: Api.Request = apiRequest

  // The runtime send (invoked by the code `.call` emits): assemble the URL (base +
  // substituted path + query), build the `Http.Request`, and dispatch through the
  // telekinesis `HttpClient` — which uses whichever `Http.Backend` is in scope.
  def send(request: Api.Request)
    ( using Online,
            HttpEvent is Loggable,
            Tactic[ConnectError],
            Tactic[UrlError],
            CharEncoder,
            JsonPrinter )
    ( using client: HttpClient onto Origin["http" | "https"] )
  :   Http.Response =

    val substituted =
      request.substitutions.foldLeft(request.path): (path, entry) =>
        path.sub(t"{${entry(0)}}", entry(1))

    val full =
      if request.query.isEmpty then t"${request.base}$substituted"
      else
        val parameters =
          request.query.map: (key, value) =>
            t"${key.urlEncode}=${value.urlEncode}"

        t"${request.base}$substituted?${parameters.join(t"&")}"

    val url = full.decode[HttpUrl]

    val headers: List[Http.Header] = request.body.lay(Nil): json =>
      List(Http.Header(t"content-type", summon[Json is Postable].mediaType(json).show))

    val empty: () => Stream[Data] = () => Stream()

    val body: () => Stream[Data] = request.body.lay(empty): json =>
      () => summon[Json is Postable].stream(json)

    val httpRequest =
      Http.Request(request.method, 1.1, url.host.vouch, url.requestTarget, headers, body)

    client.request(httpRequest, url.origin)

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
  // which `call` reads to check a target type for conformance against the schema.
  object Response:
    def make(apiRequest: Api.Request): Api.Response = new Api.Response:
      def request: Api.Request = apiRequest

  trait Response:
    type Result
    type Form
    def request: Api.Request

    // Performs the request and decodes the response as `value`. First, the macro
    // checks (at compile time) that `value` conforms to the endpoint's response
    // schema. Then we send and interpret the response in *inline* code, so `value`
    // is concrete when the `Conformant` (and hence the jacinta `Decodable`)
    // instance is summoned — which is what lets `List[T]` and other collections
    // resolve their decoders.
    transparent inline def call[value]
      ( using online:     Online,
              loggable:   HttpEvent is Loggable,
              connect:    Tactic[ConnectError],
              urlError:   Tactic[UrlError],
              encoder:    CharEncoder,
              printer:    JsonPrinter,
              client:     HttpClient onto Origin["http" | "https"],
              conformant: value is Conformant )
    :   value =

      Apoplexy.check[value](this)

      conformant.read:
        Api.send(request)(using online, loggable, connect, urlError, encoder, printer)(using client)

trait Api extends Dynamic, Locatable:
  def request: Api.Request

  transparent inline def selectDynamic(field: String): Any =
    ${Apoplexy.select('this, 'field)}

  transparent inline def applyDynamic(field: String)(inline args: Any*): Any =
    ${Apoplexy.applied('this, 'field, 'args)}

  transparent inline def applyDynamicNamed(field: String)(inline args: (String, Any)*): Any =
    ${Apoplexy.appliedNamed('this, 'field, 'args)}
