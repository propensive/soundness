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

import scala.collection.mutable as scm

import anticipation.*
import beneficence.*
import inimitable.*
import prepositional.*
import rudiments.*
import serpentine.*
import telekinesis.*
import urticose.*
import vacuous.*
import fulminate.*

object OAuth:
  case class State
    ( redirect: Path on Www,
      uuid:     Uuid                    = Uuid(),
      access:   Optional[Authorization] = Unset,
      refresh:  Optional[Text]          = Unset,
      expiry:   Optional[Long]          = Unset ):

    def expired: Boolean = expiry.let(System.currentTimeMillis > _).or(false)

  // OAuthError → OAuth.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case Connection(url: HttpUrl, reason: ConnectError.Reason) extends Reason(1)
      case InvalidJsonResponse                                   extends Reason(2)
      case UnexpectedHttpStatus(status: Http.Status)             extends Reason(3)
      case InsufficientPrivileges(scope: Text)                   extends Reason(4)
      case Unauthorized                                          extends Reason(5)
      case Other                                                 extends Reason(6)

    import Reason.*

    given Reason is Communicable =
      case InvalidJsonResponse           => m"Invalid JSON response"
      case UnexpectedHttpStatus(status)  => m"the provider returne an unexpected HTTP status: $status"
      case InsufficientPrivileges(scope) => m"the user has not granted access to $scope"
      case Unauthorized                  => m"authorization was not granted"
      case Other                         => m"an unexpected error occurred"

      case Connection(url, reason) =>
        m"could not connect to the OAuth provider at $url because $reason"

  case class Error(reason: OAuth.Error.Reason)(using Diagnostics)
  extends fulminate.Error(840, reason.number)(m"OAuth failed because $reason")

class OAuth() extends Findable:
  private val data: scm.HashMap[Session, OAuth.State] = scm.HashMap()

  def update(session: Session, state: OAuth.State): Unit = data(session) = state
  def apply(session: Session): Optional[OAuth.State] = data.at(session)
