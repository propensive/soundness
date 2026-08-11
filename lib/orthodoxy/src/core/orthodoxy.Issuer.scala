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
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import inimitable.*
import jacinta.*
import legerdemain.*
import prepositional.*
import scintillate.*
import serpentine.*
import spectacular.*
import telekinesis.*
import urticose.*
import vacuous.*
import zephyrine.*

import errorDiagnostics.stackTracesDiagnostics
import httpBackends.virtualMachine
import queryParameters.arbitraryQueryParameter

object Issuer:
  object Context:
    def apply[topic](): Context of topic = new Context:
      type Topic = topic

  trait Context extends Topical

class Issuer
  ( init:     HttpUrl,
    exchange: HttpUrl,
    redirect: HttpUrl,
    client:   Text,
    secret:   Optional[Text] = Unset ):
  private val OAuthPath: Path on Www = redirect.path

  def oauth(using Http.Request, Online, (HttpEvent is Loggable)^)
    ( lambda: (Issuer.Context of this.type) ?=> Http.Response )
    ( using store: OAuth, session: Session )
    ( using Tactic[OAuth.Error] )
  :   Http.Response =

    request.path match
      case OAuthPath =>
        mitigate:
          case error@Path.Error(reason, path) =>
            OAuth.Error(OAuth.Error.Reason.Other)

          case error@ConnectError(reason) =>
            OAuth.Error(OAuth.Error.Reason.Connection(exchange, reason))

          case error@ParseError(_, _, _) =>
            OAuth.Error(OAuth.Error.Reason.InvalidJsonResponse)

          case error@HttpError(status, _) =>
            OAuth.Error(OAuth.Error.Reason.UnexpectedHttpStatus(status))

          case error@Uuid.Error(_) =>
            OAuth.Error(OAuth.Error.Reason.Other)

          case error@Query.Error(_) =>
            OAuth.Error(OAuth.Error.Reason.Other)

          case error@JsonError(reason) =>
            OAuth.Error(OAuth.Error.Reason.InvalidJsonResponse)

        . protect:
            store(session).let: state =>
              val code: Text = request.query.code

              if store(session).let(_.uuid) != request.query.state[Uuid]
              then abort(OAuth.Error(OAuth.Error.Reason.Other))

              val query =
                Query.make
                  ( grant_type    = t"authorization_code",
                    code          = code,
                    redirect_uri  = redirect,
                    client_id     = client )

              val response: Optional[Http.Response] = if state.expired then Unset else
                exchange.submit(Http.Post)(query.per(secret)(_.client_secret = _))

              val json: Json = response match
                case response: Http.Response if response.status == Http.Ok =>
                  response.receive[Json]

                case response: Http.Response if response.status != Http.Unauthorized =>
                  abort(OAuth.Error(OAuth.Error.Reason.Other))

                // The token expired (no request was made) or was rejected: try the refresh
                // token.
                case _ =>
                  state.refresh.let: refresh =>
                    val query = Query.make(grant_type = t"refresh_token", refresh_token = refresh)

                    val response =
                      exchange.submit(Http.Post)(query.per(secret)(_.client_secret = _))

                    response.status match
                      case Http.Ok => response.receive[Json]
                      case _       => abort(OAuth.Error(OAuth.Error.Reason.Unauthorized))

                  . lest(OAuth.Error(OAuth.Error.Reason.Unauthorized))

              import dynamicJsonAccess.enabled

              // The field decodings share only the resolution-scoped tactic; no aliased
              // writer.
              val access = scala.caps.unsafe.unsafeAssumeSeparate(json.access_token.as[Text])

              val refresh =
                scala.caps.unsafe.unsafeAssumeSeparate(safely(json.refresh_token.as[Text]))

              val scopes =
                scala.caps.unsafe.unsafeAssumeSeparate(json.scope.as[Text].cut(t" "))

              val tokenType = // assume `Bearer`
                scala.caps.unsafe.unsafeAssumeSeparate(json.token_type.as[Text])

              val expiry: Optional[Long] = scala.caps.unsafe.unsafeAssumeSeparate:
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

      val query =
        Query.make
          ( client_id     = client,
            redirect_uri  = redirect,
            access_type   = t"offline",
            scope         = scopes.flatMap(_.names).distinct.to(List).join(t" "),
            state         = state.uuid.show,
            response_type = t"code" )

      Redirect(init.query(query))
