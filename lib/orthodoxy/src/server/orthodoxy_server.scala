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
import legerdemain.*
import prepositional.*
import scintillate.*
import serpentine.*
import spectacular.*
import telekinesis.*
import urticose.*
import vacuous.*

import errorDiagnostics.stackTracesDiagnostics
import httpBackends.javaNetHttp
import queryParameters.arbitraryQueryParameter

// The authorization-code flow (RFC 6749 §4.1) as scintillate middleware: `oauth` handles the
// redirect back from the issuer, exchanging the code for an authorization (or refreshing an
// expired one), and `require` sends a user without one to the issuer.
extension (issuer: Issuer)
  def oauth(using Http.Request, Online, (Http.Event is Loggable)^)
    ( lambda: (Issuer.Context of issuer.type) ?=> Http.Response )
    ( using store: Authorizations, session: Session )
    ( using Tactic[OAuth.Error] )
  :   Http.Response =

    val redirect = issuer.redirect.or:
      abort(OAuth.Error(OAuth.Error.Reason.Misconfigured(t"the flow needs a redirect URI")))

    if request.path != redirect.path then lambda(using Issuer.Context[issuer.type]())
    else
      mitigate:
        case error@Path.Error(reason, path) => OAuth.Error(OAuth.Error.Reason.Other)
        case error@Uuid.Error(_)            => OAuth.Error(OAuth.Error.Reason.Other)
        case error@Query.Error(_)           => OAuth.Error(OAuth.Error.Reason.Other)

        case error@Connect.Error(reason) =>
          OAuth.Error(OAuth.Error.Reason.Connection(issuer.exchange, reason))

      . protect:
          store(session).let: state =>
            val code: Text = request.query.code

            if state.uuid != request.query.state[Uuid]
            then abort(OAuth.Error(OAuth.Error.Reason.Other))

            // A held authorization which has expired is refreshed rather than exchanged anew
            val authorization =
              state.access match
                case access: Authorization if access.expired && access.refresh.present =>
                  issuer.refresh(access)

                case _ =>
                  issuer.exchangeCode(code)

            store(session) = state.copy(access = authorization)
            Http.Response(new Redirect(state.redirect.show, false))

          . or(lambda(using Issuer.Context[issuer.type]()))

  def require[scope <: Scope & Singleton: scala.Precise](scopes: scope*)
    ( using store: Authorizations, session: Session, request: Http.Request )
    ( using Issuer.Context of issuer.type )
    ( using Tactic[OAuth.Error] )
    ( lambda: Authorization of scope ?=> Http.Response )
  :   Http.Response =

    store(session).let(_.access).let(_.of[scope]).letGiven(lambda).or:
      val state = Authorizations.State(request.path)
      store(session) = state
      val names = scopes.flatMap(_.names).distinct.to(List)
      Redirect(issuer.authorizationUrl(names, state.uuid.show))
