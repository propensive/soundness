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
package orthodoxy


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
import telekinesis.*
import urticose.*
import vacuous.*
import zephyrine.*

import errorDiagnostics.stackTraces
import queryParameters.arbitrary

object Issuer:
  trait Context extends Topical
  object Context:
    def apply[topic](): Context of topic = new Context:
      type Topic = topic

class Issuer
  ( init:     HttpUrl,
    exchange: HttpUrl,
    redirect: HttpUrl,
    client:   Text,
    secret:   Optional[Text] = Unset ):
  private val OAuthPath: Path on Www = redirect.path

  def oauth(using Http.Request, Online, HttpEvent is Loggable)
    ( lambda: (Issuer.Context of this.type) ?=> Http.Response )
    ( using store: OAuth, session: Session )
  :   Http.Response raises OAuthError =

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
                  ( grant_type    = t"authorization_code",
                    code          = code,
                    redirect_uri  = redirect,
                    client_id     = client )

              val response: Optional[Http.Response] = if state.expired then Unset else
                exchange.submit(Http.Post)(query.per(secret)(_.client_secret = _))

              val json: Json = response.let(_.status) match
                case Http.Ok           => response.vouch.receive[Json]
                case Http.Unauthorized | Unset =>
                  state.refresh.let: refresh =>
                    val query = Query(grant_type = t"refresh_token", refresh_token = refresh)

                    val response =
                      exchange.submit(Http.Post)(query.per(secret)(_.client_secret = _))

                    response.status match
                      case Http.Ok => response.receive[Json]
                      case _       => abort(OAuthError(OAuthError.Reason.Unauthorized))

                  . lest(OAuthError(OAuthError.Reason.Unauthorized))

                case status            => abort(OAuthError(OAuthError.Reason.Other))

              import dynamicJsonAccess.enabled

              val access = json.access_token.as[Text]
              val refresh = safely(json.refresh_token.as[Text])
              val scopes = json.scope.as[Text].cut(t" ")
              val tokenType = json.token_type.as[Text] // assume `Bearer`

              val expiry: Optional[Long] =
                safely(System.currentTimeMillis + json.expires_in.as[Long]*1000L)

              val state2 = state.copy(access = Authorization(access, scopes, expiry, refresh))

              store(session) = state2


              Http.Response(new Redirect(state.redirect.show, false))

          . or(lambda(using Issuer.Context[this.type]()))

      case _ =>
        lambda(using Issuer.Context[this.type]())


  def require[scope <: Scope & Singleton: Precise](scopes: scope*)
    ( using store: OAuth, session: Session, request: Http.Request )
    ( using Issuer.Context of this.type )
    ( lambda: Authorization of scope ?=> Http.Response )
  :   Http.Response =

    store(session).let(_.access).let(_.of[scope]).letGiven(lambda).or:
      val state = OAuth.State(request.path)
      store(session) = state

      val query = Query
        ( client_id     = client,
          redirect_uri  = redirect,
          access_type   = t"offline",
          scope         = scopes.flatMap(_.names).to(Set).to(List).join(t" "),
          state         = state.uuid.show,
          response_type = t"code" )

      Redirect(init.query(query))
