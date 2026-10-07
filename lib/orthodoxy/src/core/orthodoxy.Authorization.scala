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
package orthodoxy

import anticipation.*
import beneficence.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.*
import jacinta.*
import prepositional.*
import rudiments.*
import telekinesis.*
import vacuous.*

import errorDiagnostics.stackTracesDiagnostics

object Authorization:
  given authorization: ("authorization" is Directive of Authorization) =
    authorization => t"Bearer ${authorization.key}"

  // The authorization an OAuth 2 token response grants (RFC 6749 §5.1): the access token, the
  // scopes (absent when the server granted exactly those requested), the lifetime as an expiry
  // instant, and a refresh token if one was issued
  def parse(json: Json)(using Tactic[OAuth.Error]): Authorization =
    mitigate:
      case Json.Error(_) => OAuth.Error(OAuth.Error.Reason.InvalidJsonResponse)

    . protect(read(json))

  private def read(json: Json)(using Tactic[Json.Error]): Authorization =
    import dynamicAccess.dynamicJson

    // The field decodings share only the resolution-scoped tactic; no aliased writer.
    val key = json.access_token.as[Text]

    val scopes: List[Text] =
      safely(json.scope.as[Text]).let(_.cut(t" ")).or(Nil)

    val expiry: Optional[Long] =
      safely(System.currentTimeMillis + json.expires_in.as[Long]*1000L)

    val refresh: Optional[Text] =
      safely(json.refresh_token.as[Text])

    Authorization(key, scopes, expiry, refresh)

// An access token and what it grants: the scopes, its expiry (an instant in milliseconds, absent
// for a token without a stated lifetime) and a refresh token where one was issued
case class Authorization
  ( key:     Text,
    scopes:  List[Text],
    expiry:  Optional[Long],
    refresh: Optional[Text] )
extends Topical, Findable:
  private[orthodoxy] def of[scope <: Scope]: Authorization of scope =
    this.asInstanceOf[Authorization of scope]

  // The token as a `Bearer` authorization, as the `Authorization` header carries it
  def bearer: Auth = Auth.Bearer(key)

  def expired: Boolean = expiry.let(System.currentTimeMillis > _).or(false)

  // Whether every one of the scopes is granted. A token whose response named no scopes is taken
  // to grant those requested, as RFC 6749 §5.1 has it, so an empty list grants everything.
  def grants(required: List[Text]): Boolean = scopes.nil || required.all(scopes.has(_))
