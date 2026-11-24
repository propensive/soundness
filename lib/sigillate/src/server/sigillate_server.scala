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
package sigillate

import anticipation.*
import aviation.*
import contingency.*
import distillate.*
import enigmatic.*
import fulminate.*
import gastronomy.*
import gossamer.*
import telekinesis.*
import vacuous.*

// Bearer-token authentication (RFC 6750) for a scintillate server, the counterpart of
// `basicAuth`: the request's `Authorization: Bearer` header must carry a JWT that verifies
// against the key, is within its validity period by the `Clock` in scope and — where they are
// given — names the expected issuer and audience. The handler then runs with the token in
// scope, as `jwt`. A request with no token is challenged; one whose token does not check out
// is told so, with the same challenge and `error="invalid_token"`, and never why.
private def challenge(realm: Text, invalid: Boolean): Http.Response =
  val error = if invalid then t""", error="invalid_token"""" else t""
  Http.Response(Http.Unauthorized, wwwAuthenticate = t"""Bearer realm="$realm"$error""")()

// The token a `bearerAuth` handler was called with.
def jwt(using jwt: Jwt): Jwt = jwt

// Authenticates against a public key.
def bearerAuth[cipher <: Cipher]
  ( key:      PublicKey[cipher],
    realm:    Text,
    issuer:   Optional[Text] = Unset,
    audience: Optional[Text] = Unset )
  ( response: Jwt ?=> Http.Response )
  ( using connection:      Http.Connection,
          algorithm:       cipher is Jose.Algorithm,
          erased weakness: ProcessingPermit[Weakness[cipher]],
          clock:           Clock,
          tactic:          Tactic[Auth.Error],
          diagnostics:     Diagnostics )
:   Http.Response =

  connection.headers.authorization match
    case List(Auth.Bearer(token)) =>
      safely(Jwt.verify(token, key).validate(issuer, audience)).lay(challenge(realm, true)):
        jwt => response(using jwt)

    case _ =>
      challenge(realm, false)

// Authenticates against a key set, in which the token's `kid` chooses the key: the type
// argument is the cipher every key in the set is read as, `bearerAuth[Ecdsa[256]](keys, …)`.
// A token that names no key is checked against the set's only key, if it has only one.
def bearerAuth[cipher <: Cipher]
  ( keys:     Jwks,
    realm:    Text,
    issuer:   Optional[Text],
    audience: Optional[Text] )
  ( response: Jwt ?=> Http.Response )
  ( using connection:      Http.Connection,
          algorithm:       cipher is Jose.Algorithm,
          form:            cipher is Jwk.Form,
          keyAlgorithm:    cipher is KeyAlgorithm,
          erased weakness: ProcessingPermit[Weakness[cipher]],
          clock:           Clock,
          tactic:          Tactic[Auth.Error],
          diagnostics:     Diagnostics )
:   Http.Response =

  connection.headers.authorization match
    case List(Auth.Bearer(token)) =>
      val verified: Optional[Jwt] = safely:
        val chosen = token.as[Jws].header.keyId.lay(keys.sole)(keys(_))

        val key = chosen.or:
          abort(Jose.Error(Jose.Error.Reason.BadKey(t"the token names no key in the set")))

        Jwt.verify(token, key.publicKey[cipher]).validate(issuer, audience)

      verified.lay(challenge(realm, true)): jwt => response(using jwt)

    case _ =>
      challenge(realm, false)
