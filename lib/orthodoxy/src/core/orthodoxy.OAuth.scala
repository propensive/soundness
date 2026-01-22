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
package orthodoxy

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import inimitable.*
import jacinta.*
import legerdemain.*
import prepositional.*
import rudiments.*
import scintillate.*
import serpentine.*
import spectacular.*
import symbolism.*
import telekinesis.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

import errorDiagnostics.stackTraces
import queryParameters.arbitrary
import stdioSources.virtualMachine.ansi
import jsonPrinters.indented

class OAuthStore[decodable]():
  case class State
              (redirect: Path on Www,
               uuid:     Uuid                = Uuid(),
               access:   Optional[decodable] = Unset,
               refresh:  Optional[Text]      = Unset,
               expiry:   Optional[Long]      = Unset):

    def expired: Boolean = expiry.let(System.currentTimeMillis > _).or(false)

  private val data: scm.HashMap[Session, State] = scm.HashMap()

  def update(session: Session, state: State): Unit = data(session) = state
  def apply(session: Session): Optional[State] = data.at(session)

given Http.Request => Session = Session("mysession")

case class OAuthScope(name: Text)

object OAuth:
  erased trait Context extends Topical

class OAuth[decodable]
       (init:     HttpUrl,
        exchange: HttpUrl,
        redirect: HttpUrl,
        client:   Text,
        secret:   Optional[Text] = Unset):
  private val OAuthPath: Path on Www = redirect.path

  def oauth(using Http.Request, Online, HttpEvent is Loggable)
       (lambda: OAuth.Context of this.type ?=> Http.Response)
       (using store: OAuthStore[decodable], session: Session)(using decodable is Decodable in Text)
  : Http.Response raises OAuthError =

      request.path match
        case OAuthPath =>
          mitigate:
              case error@PathError(reason, path) =>
                OAuthError(OAuthError.Reason.Other)
              case error@ConnectError(reason)    =>
                OAuthError(OAuthError.Reason.Connection(exchange, reason))
              case error@ParseError(_, _, _)     =>
                OAuthError(OAuthError.Reason.InvalidJsonResponse)
              case error@HttpError(status, _) =>
                OAuthError(OAuthError.Reason.UnexpectedHttpStatus(status))
              case error@UuidError(_)            =>
                OAuthError(OAuthError.Reason.Other)
              case error@QueryError()            =>
                OAuthError(OAuthError.Reason.Other)
              case error@JsonError(reason)       =>
                OAuthError(OAuthError.Reason.InvalidJsonResponse)

          . within:
              store(session).let: state =>
                val code: Text = request.query.code

                if store(session).let(_.uuid) != request.query.state[Uuid]
                then abort(OAuthError(OAuthError.Reason.Other))

                val query =
                  Query
                   (grant_type    = t"authorization_code",
                    code          = code,
                    redirect_uri  = redirect,
                    client_id     = client)

                val response = exchange.submit(Http.Post)(query.per(secret)(_.client_secret = _))

                val response2 = response.status match
                  case Http.Ok => response

                  case Http.Unauthorized =>
                    // FIXME: Need to handle refresh token
                    abort(OAuthError(OAuthError.Reason.Other))

                  case status =>
                    abort(OAuthError(OAuthError.Reason.Other))


                val json = response2.receive[Json]
                import dynamicJsonAccess.enabled
                val access = json.access_token.as[Text]
                val refresh = safely(json.refresh_token.as[Text])
                val scope = json.scope.as[Text].cut(t" ")
                val tokenType = json.token_type.as[Text] // assume `Bearer`

                val expiry: Optional[Long] =
                  safely(System.currentTimeMillis + json.expires_in.as[Long]*1000L)

                store(session) =
                  state.copy(access = access.decode, expiry = expiry, refresh = refresh)

                Http.Response(new Redirect(state.redirect.show, false))

            . or(lambda(using !![OAuth.Context of this.type]))

        case _ =>
          lambda(using !![OAuth.Context of this.type])

  def require(scopes: OAuthScope*)[result]
       (using store: OAuthStore[decodable], session: Session, request: Http.Request)
       (using OAuth.Context of this.type)
       (lambda: decodable ?=> Http.Response)
  : Http.Response =

      store(session).let(_.access).letGiven(lambda).or:
        val state = store.State(request.path)
        store(session) = state

        val query = Query
         (client_id     = client,
          redirect_uri  = redirect,
          access_type   = t"offline",
          scope         = scopes.map(_.name).join(t" "),
          state         = state.uuid.show,
          response_type = t"code")

        Redirect(init.query(query))
