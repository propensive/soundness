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

import soundness.*

import charsets.utf8Charset
import codepages.utf8Codepage
import errorDiagnostics.stackTracesDiagnostics
import internetAccess.online
import logging.silentLogging
import strategies.throwUnsafely
import textSanitizers.skipSanitizer

// A backend which records the token request and answers with a canned response
class TokenServer(canned: () => Http.Response) extends Http.Backend:
  @scala.caps.unsafe.untrackedCaptures
  var lastUrl: Optional[Text] = Unset
  @scala.caps.unsafe.untrackedCaptures
  var lastBody: Optional[Text] = Unset

  def request
     ( url: Text, method: Http.Method, headers: List[Http.Header], body: Spring[Data]^ )
     ( using Tactic[Connect.Error] )
  :   Http.Response =
    lastUrl = url
    lastBody = body().memoize.utf8
    canned()

object Tests extends Suite(m"Orthodoxy Tests"):
  def run(): Unit =
    suite(m"PKCE"):
      test(m"the S256 challenge of RFC 7636's example verifier"):
        Pkce(t"dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk").challenge
      . assert(_ == t"E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM")

      test(m"a fresh verifier has 43 URL-safe characters"):
        Pkce().verifier.length
      . assert(_ == 43)

    suite(m"token responses"):
      val json = t"""{"access_token": "tok", "token_type": "Bearer", "expires_in": 3600,
                     "refresh_token": "ref", "scope": "read write"}""".as[Json]

      test(m"a token response is read"):
        val authorization = Authorization.parse(json)
        (authorization.key, authorization.scopes, authorization.refresh, authorization.expiry.present)
      . assert(_ == (t"tok", List(t"read", t"write"), t"ref", true))

      test(m"a response without scopes grants those requested"):
        Authorization.parse(t"""{"access_token": "tok", "token_type": "Bearer"}""".as[Json])
        . grants(List(t"anything"))
      . assert(_ == true)

      test(m"a response naming scopes grants only those"):
        Authorization.parse(json).grants(List(t"admin"))
      . assert(_ == false)

      test(m"a response without an access token is invalid"):
        capture[OAuth.Error](Authorization.parse(t"""{"token_type": "Bearer"}""".as[Json])).reason
      . assert(_ == OAuth.Error.Reason.InvalidJsonResponse)

    suite(m"grants"):
      val issuer = Issuer(url"https://issuer.example/token", t"client-1", t"s3cret")

      def ok(body: Text): Http.Response =
        Http.Response(Http.Ok, contentType = media"application/json")(body)

      test(m"the client-credentials grant posts the grant, the scopes and the client's identity"):
        val server = TokenServer(() => ok(t"""{"access_token": "tok", "token_type": "Bearer"}"""))
        given Http.Backend = server
        val authorization = issuer.clientCredentials(t"read", t"write")
        (authorization.key, server.lastUrl, server.lastBody)
      . assert: result =>
          result(0) == t"tok" && result(1) == t"https://issuer.example/token"
          && result(2) == t"grant_type=client_credentials&scope=read+write&client_id=client-1&client_secret=s3cret"

      test(m"a refresh posts the refresh token"):
        val server = TokenServer(() => ok(t"""{"access_token": "tok2", "token_type": "Bearer"}"""))
        given Http.Backend = server
        issuer.refresh(t"ref").key
      . assert(_ == t"tok2")

      test(m"a refused grant is Unauthorized"):
        given Http.Backend = TokenServer(() => Http.Response(Http.BadRequest)(t"""{"error": "invalid_grant"}"""))
        capture[OAuth.Error](issuer.refresh(t"ref")).reason
      . assert(_ == OAuth.Error.Reason.Unauthorized)

      test(m"an authorization URL carries the scopes, state and PKCE challenge"):
        val flow =
          Issuer
            ( url"https://issuer.example/token", t"client-1", Unset,
              url"https://issuer.example/authorize", url"https://app.example/callback" )

        val pkce = Pkce(t"dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk")
        flow.authorizationUrl(List(t"read"), t"xyz", pkce).show
      . assert(_ == t"https://issuer.example/authorize?client_id=client-1&redirect_uri=https%3A%2F%2Fapp.example%2Fcallback&response_type=code&state=xyz&scope=read&code_challenge=E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM&code_challenge_method=S256")

      test(m"the authorization-code flow needs an authorization endpoint"):
        capture[OAuth.Error](issuer.authorizationUrl(Nil, t"xyz")).reason match
          case OAuth.Error.Reason.Misconfigured(_) => true
          case _                                   => false
      . assert(_ == true)

    suite(m"credentials"):
      test(m"a credential is found by the scheme's name"):
        given ("api_key" is Credential to Text) = Credential(t"k-1")
        summon[("api_key" is Credential to Text)].value
      . assert(_ == t"k-1")
