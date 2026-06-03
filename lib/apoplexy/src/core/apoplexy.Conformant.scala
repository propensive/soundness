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

import contingency.*
import distillate.*
import fulminate.*
import jacinta.*
import prepositional.*
import telekinesis.*
import turbulence.*

import errorDiagnostics.empty
import zephyrine.ParseError

// Interprets an `Http.Response` as a value of `Self`. The instances decide how a
// response body is read; `Api.Response.call[T]` performs the compile-time check
// that `T` conforms to the endpoint's response schema before summoning one.
object Conformant:
  // Raise `ApiError` unless the response status is in the 2xx range.
  def successful(response: Http.Response)(using Tactic[ApiError]): Unit =
    if response.status.category != Http.Status.Category.Successful
    then abort(ApiError(ApiError.Reason.Status(response.status.code)))

  // The escape hatch: the raw response, with no status check.
  given response: Http.Response is Conformant = response => response

  // The raw 2xx body as JSON.
  given json: Tactic[ApiError] => Json is Conformant = response =>
    successful(response)

    whereas:
      case ParseError(_, _, _) => ApiError(ApiError.Reason.Malformed)

    . mitigate(response.body.stream.read[Json])

  // A 2xx body decoded to any JSON-decodable type. The `.call[T]` inline method
  // only summons this after the conformance macro has verified `T` matches the
  // endpoint's response schema. Resolving `value is Decodable in Json` here, at
  // the concrete call site, lets jacinta pick the collection decoder for
  // `List[T]` etc. — which a macro-spliced summon over an abstract type cannot.
  given decodable: [value: Decodable in Json] => Tactic[ApiError] => value is Conformant =
    response =>
      successful(response)

      whereas:
        case ParseError(_, _, _) => ApiError(ApiError.Reason.Malformed)
        case JsonError(_)        => ApiError(ApiError.Reason.Malformed)

      . mitigate(response.body.stream.read[Json].as[value])

trait Conformant extends Typeclass:
  def read(response: Http.Response): Self
