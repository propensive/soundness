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
import denominative.*
import fulminate.*
import gossamer.*
import jacinta.*
import legerdemain.*
import prepositional.*
import spectacular.*
import symbolism.*
import telekinesis.*
import urticose.*
import vacuous.*
import zephyrine.*

import errorDiagnostics.stackTracesDiagnostics
import queryParameters.arbitraryQueryParameter

object Issuer:
  object Context:
    def apply[topic](): Context of topic = new Context:
      type Topic = topic

  trait Context extends Topical

// An OAuth 2 authorization server as one client sees it: the token endpoint (`exchange`) and the
// client's identity, and — for the authorization-code flow — the authorization endpoint (`init`)
// and the redirect URI registered for the client. The grants below are the client-side of RFC
// 6749: each posts to the token endpoint, with the client's credentials, and reads the
// `Authorization` it returns; the request goes through whichever `Http.Client` is in scope.
class Issuer
  ( val exchange: HttpUrl,
    val client:   Text,
    val secret:   Optional[Text]    = Unset,
    val init:     Optional[HttpUrl] = Unset,
    val redirect: Optional[HttpUrl] = Unset ):

  private def endpoint(url: Optional[HttpUrl], what: Text)(using Tactic[OAuth.Error]): HttpUrl =
    url.or(abort(OAuth.Error(OAuth.Error.Reason.Misconfigured(what))))

  // A token request: the parameters of a grant, with the client's identity (and secret, where it
  // has one) as RFC 6749 §2.3.1's `client_secret_post`. A `400` is an `invalid_grant`-class
  // refusal, a `401` an unauthenticated client; both read as `Unauthorized`.
  def grant(parameters: Query)
    ( using Online, (Http.Event is Loggable)^, Http.Client onto Origin["http" | "https"] )
    ( using Tactic[OAuth.Error] )
  :   Authorization =

    val identity = Query(List(t"client_id" -> client))
    val secrecy = secret.lay(Query()): secret => Query(List(t"client_secret" -> secret))
    val query = parameters ++ identity ++ secrecy

    val response =
      mitigate:
        case Connect.Error(reason) => OAuth.Error(OAuth.Error.Reason.Connection(exchange, reason))

      . protect(exchange.submit(Http.Post)(query))

    response.status match
      case Http.Ok =>
        mitigate:
          case Parse.Error(_, _, _)  => OAuth.Error(OAuth.Error.Reason.InvalidJsonResponse)
          case Http.Error(status, _) => OAuth.Error(OAuth.Error.Reason.UnexpectedHttpStatus(status))

        . protect(Authorization.parse(response.receive[Json]))

      case Http.Unauthorized | Http.BadRequest =>
        abort(OAuth.Error(OAuth.Error.Reason.Unauthorized))

      case status =>
        abort(OAuth.Error(OAuth.Error.Reason.UnexpectedHttpStatus(status)))

  // The client-credentials grant (RFC 6749 §4.4): the client's own authorization, for the scopes
  def clientCredentials(scopes: Text*)
    ( using Online, (Http.Event is Loggable)^, Http.Client onto Origin["http" | "https"] )
    ( using Tactic[OAuth.Error] )
  :   Authorization =

    val scope = if scopes.isEmpty then Query() else Query(List(t"scope" -> scopes.join(t" ")))
    grant(Query(List(t"grant_type" -> t"client_credentials")) ++ scope)

  // A refresh (RFC 6749 §6): a new authorization from a refresh token
  def refresh(token: Text)
    ( using Online, (Http.Event is Loggable)^, Http.Client onto Origin["http" | "https"] )
    ( using Tactic[OAuth.Error] )
  :   Authorization =

    grant(Query(List(t"grant_type" -> t"refresh_token", t"refresh_token" -> token)))

  // As `refresh(token)`, for an authorization which carries its refresh token
  def refresh(authorization: Authorization)
    ( using Online, (Http.Event is Loggable)^, Http.Client onto Origin["http" | "https"] )
    ( using Tactic[OAuth.Error] )
  :   Authorization =

    refresh(authorization.refresh.or(abort(OAuth.Error(OAuth.Error.Reason.Unauthorized))))

  // The exchange of an authorization code for an authorization (RFC 6749 §4.1.3), with the PKCE
  // verifier (RFC 7636) where the authorization request carried its challenge
  def exchangeCode(code: Text, pkce: Optional[Pkce] = Unset)
    ( using Online, (Http.Event is Loggable)^, Http.Client onto Origin["http" | "https"] )
    ( using Tactic[OAuth.Error] )
  :   Authorization =

    val redirection = endpoint(redirect, t"an authorization code needs a redirect URI")

    val verifier = pkce.lay(Query()): pkce => Query(List(t"code_verifier" -> pkce.verifier))

    val query =
      Query
        ( List
            ( t"grant_type"   -> t"authorization_code",
              t"code"         -> code,
              t"redirect_uri" -> redirection.show ) )

    grant(query ++ verifier)

  // The URL a user is sent to, to authorize the client (RFC 6749 §4.1.1): with the scopes
  // requested, a `state` to be checked against the redirect, and a PKCE challenge where one is
  // used
  def authorizationUrl(scopes: List[Text], state: Text, pkce: Optional[Pkce] = Unset)
    ( using Tactic[OAuth.Error] )
  :   HttpUrl =

    val authorization = endpoint(init, t"the authorization-code flow needs an authorization URL")
    val redirection = endpoint(redirect, t"the authorization-code flow needs a redirect URI")

    val challenge = pkce.lay(Query()): pkce =>
      Query(List(t"code_challenge" -> pkce.challenge, t"code_challenge_method" -> t"S256"))

    val scope = if scopes.nil then Query() else Query(List(t"scope" -> scopes.join(t" ")))

    val query =
      Query
        ( List
            ( t"client_id"     -> client,
              t"redirect_uri"  -> redirection.show,
              t"response_type" -> t"code",
              t"state"         -> state ) )

    authorization.query(query ++ scope ++ challenge)
