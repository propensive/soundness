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
package apoplexy

import contingency.*
import distillate.*
import fulminate.*
import hieroglyph.*
import jacinta.*
import prepositional.*
import telekinesis.*
import turbulence.*
import xylophone.*

import errorDiagnostics.emptyDiagnostics
import zephyrine.{ParseError, memoize}

// Interprets an `Http.Response` as a value of `Self`, reading the body in the wire
// format `Transport` (`jacinta.Json` or `xylophone.Xml`) the spec dictates.
// `Api.Response.call[T]()` checks at compile time that `T` conforms to the response
// schema, then summons `(T is Conformant) over <Transport>` at the concrete call
// site — so only the matching format's givens are demanded, and `List[T]`-style
// collection decoders resolve.
// The decodable instances are lower priority so that the exact-type givens
// (`Http.Response`, `Unit`, raw `Json`, raw `Xml`) win — `Unit` and `Json` are
// themselves `Decodable in Json`, so they would otherwise be ambiguous.
trait LowPriorityConformant:
  // A 2xx JSON body decoded to any JSON-decodable type. Resolving `Decodable in
  // Json` here, at the concrete call site, lets jacinta pick the collection
  // decoder for `List[T]` etc.
  given jsonDecodable: [value: Decodable in Json] => Tactic[Api.Error]
  =>  (value is Conformant) over Json =
    response =>
      Conformant.successful(response)

      mitigate:
        case ParseError(_, _, _) => Api.Error(Api.Error.Reason.Malformed)
        case Json.Error(_)        => Api.Error(Api.Error.Reason.Malformed)

      . protect(response.body.stream.memoize.read[Json].as[value])

  // A 2xx XML body decoded to any XML-decodable type.
  given xmlDecodable: [value: Decodable in Xml]
  =>  ( CharDecoder, XmlSchema, Tactic[Api.Error] ) => (value is Conformant) over Xml =
    response =>
      Conformant.successful(response)

      mitigate:
        case ParseError(_, _, _) => Api.Error(Api.Error.Reason.Malformed)
        case _: Xml.Error         => Api.Error(Api.Error.Reason.Malformed)

      . protect(summon[CharDecoder].decoded(response.body.stream.memoize).read[Xml].as[value])

object Conformant extends LowPriorityConformant:
  // Raise `Api.Error` unless the response status is in the 2xx range.
  def successful(response: Http.Response)(using Tactic[Api.Error]): Unit =
    if response.status.category != Http.Status.Category.Successful
    then abort(Api.Error(Api.Error.Reason.Status(response.status.code)))

  // The escape hatch: the raw response, with no status check (any transport).
  given response: [transport] => (Http.Response is Conformant) over transport =
    response => response

  // Just check for success and discard the body — for no-content (204) endpoints
  // such as `delete`, and the default target of a bare `.call()`. Never reads the
  // body, so an empty one is fine; transport-agnostic.
  given unit: [transport] => Tactic[Api.Error] => (Unit is Conformant) over transport =
    response => successful(response)

  // The raw 2xx body as JSON.
  given json: Tactic[Api.Error] => (Json is Conformant) over Json = response =>
    successful(response)

    mitigate:
      case ParseError(_, _, _) => Api.Error(Api.Error.Reason.Malformed)

    . protect(response.body.stream.memoize.read[Json])

  // The raw 2xx body as XML. The body bytes are decoded to `Text` (via the
  // `CharDecoder`) before xylophone parses them.
  given xml: (CharDecoder, XmlSchema, Tactic[Api.Error]) => (Xml is Conformant) over Xml =
    response =>
      successful(response)

      mitigate:
        case ParseError(_, _, _) => Api.Error(Api.Error.Reason.Malformed)

      . protect(summon[CharDecoder].decoded(response.body.stream.memoize).read[Xml])

trait Conformant extends Typeclass, Transportive:
  def read(response: Http.Response): Self
