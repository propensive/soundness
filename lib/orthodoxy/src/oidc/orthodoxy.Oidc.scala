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

import adversaria.*
import anticipation.*
import aviation.*
import contingency.*
import corpuscular.*
import denominative.*
import distillate.*
import enigmatic.*
import fulminate.*
import gastronomy.*
import gossamer.*
import jacinta.*
import monotonous.*
import prepositional.*
import rudiments.*
import sigillate.*
import spectacular.*
import telekinesis.*
import urticose.*
import vacuous.*
import zephyrine.*

// OpenID Connect (OpenID Connect Core 1.0): OAuth 2, with the answer to "who is the user?"
// added as an ID token — a JWT, signed by the provider — beside the access token.
//
// `Oidc.discover` reads a provider's metadata from its well-known URL, and
// `Oidc.identify` verifies the ID token an `Authorization` carries: that the provider signed
// it, that it is for this client and has not expired, and that it answers the request this
// client made. What it returns is the token's claims, whose `subject` is the user.
object Oidc:
  // The parts of a provider's metadata (OpenID Connect Discovery 1.0 §3) a client acts on:
  // who the provider says it is, its two OAuth endpoints, and where it publishes its keys.
  case class Provider(issuer: Text, authorization: HttpUrl, token: HttpUrl, keys: HttpUrl):
    // The provider as an OAuth 2 `Issuer`, for a client registered with it.
    def client
      ( identifier: Text, secret: Optional[Text] = Unset, redirect: Optional[HttpUrl] = Unset )
    :   Issuer =
      Issuer(token, identifier, secret, authorization, redirect)

  private case class Metadata
    ( issuer:                                           Text,
      @name[Json](t"authorization_endpoint") authorization: Text,
      @name[Json](t"token_endpoint")         token:         Text,
      @name[Json](t"jwks_uri")               keys:          Text )

  private def invalid(detail: Text)(using Diagnostics): OAuth.Error =
    OAuth.Error(OAuth.Error.Reason.Identity(detail))

  // A provider's metadata, from `/.well-known/openid-configuration` beneath its issuer URL.
  // The document must name the issuer it was asked of (Discovery §4.3): one that names
  // another is describing some other provider's endpoints.
  def discover(issuer: Text)
    ( using Online, (Http.Event is Loggable)^, Http.Client onto Origin["http" | "https"] )
    ( using Tactic[OAuth.Error], Diagnostics )
  :   Provider =

    val base = if issuer.ends(t"/") then issuer.skip(1, Rtl) else issuer

    mitigate:
      case Url.Error(_, _, _)    => OAuth.Error(OAuth.Error.Reason.InvalidJsonResponse)
      case Parse.Error(_, _, _)  => OAuth.Error(OAuth.Error.Reason.InvalidJsonResponse)
      case Truncation.Error(_)   => OAuth.Error(OAuth.Error.Reason.InvalidJsonResponse)
      case Json.Error(_)         => OAuth.Error(OAuth.Error.Reason.InvalidJsonResponse)
      case Http.Error(status, _) => OAuth.Error(OAuth.Error.Reason.UnexpectedHttpStatus(status))

      case Connect.Error(reason) =>
        OAuth.Error(OAuth.Error.Reason.Misconfigured(t"the provider could not be reached"))

    . protect:
        val url = t"$base/.well-known/openid-configuration".as[HttpUrl]
        val metadata = url.fetch().receive[Json].as[Metadata]

        if metadata.issuer != issuer && metadata.issuer != base
        then abort(invalid(t"the metadata is for the issuer ${metadata.issuer}"))

        Provider
          ( metadata.issuer,
            metadata.authorization.as[HttpUrl],
            metadata.token.as[HttpUrl],
            metadata.keys.as[HttpUrl] )

  // `at_hash` (OpenID Connect Core §3.1.3.6): the left half of the hash of the access token,
  // under the hash of the ID token's own signature algorithm.
  private def accessTokenHash(algorithm: Text, token: Text)
    ( using Hash in Sha2[256], Hash in Sha2[384], Hash in Sha2[512] )
  :   Text =

    import alphabets.base64Url
    import hieroglyph.codepages.utf8Codepage

    val bytes: Data = token.in[Data]

    val digest: Data =
      if algorithm.ends(t"384") then bytes.digest[Sha2[384]].data
      else if algorithm.ends(t"512") || algorithm == t"EdDSA" then bytes.digest[Sha2[512]].data
      else bytes.digest[Sha2[256]].data

    digest.keep(digest.length/2).serialize[Base64]

  // Verifies the ID token of a token response (OpenID Connect Core §3.1.3.7) and gives its
  // claims. `keys` are the provider's, among which the token's `kid` chooses; the type argument
  // is the cipher they are read as, and so the one algorithm accepted. `issuer` is the
  // provider and `client` this client's identifier, which the token must be issued by and
  // for. A `nonce`, where the authorization request sent one, must come back in the token,
  // which is what ties the token to that request.
  def identify[cipher <: Cipher]
    ( authorization: Authorization,
      keys:          Jwks,
      issuer:        Text,
      client:        Text,
      nonce:         Optional[Text] = Unset )
    ( using algorithm:       cipher is Jose.Algorithm,
            form:            cipher is Jwk.Form,
            keyAlgorithm:    cipher is KeyAlgorithm,
            erased weakness: ProcessingPermit[Weakness[cipher]],
            sha256:          Hash in Sha2[256],
            sha384:          Hash in Sha2[384],
            sha512:          Hash in Sha2[512],
            clock:           Clock,
            tactic:          Tactic[OAuth.Error],
            diagnostics:     Diagnostics )
  :   Jwt =

    val token = authorization.identityToken.or:
      abort(invalid(t"the token response carried no ID token"))

    val jwt =
      mitigate:
        case error: Jose.Error => invalid(error.message.text)

      . protect:
          val chosen = token.as[Jws].header.keyId.lay(keys.sole)(keys(_))

          val key = chosen.or:
            abort(Jose.Error(Jose.Error.Reason.BadKey(t"the token names no key of the provider's")))

          Jwt.verify(token, key.publicKey[cipher]).validate(issuer = issuer, audience = client)

    // An ID token must expire and must say whom it identifies.
    if jwt.claims.expiry.absent then abort(invalid(t"it has no expiry"))
    if jwt.claims.subject.absent then abort(invalid(t"it names no subject"))

    // A token for several audiences must say which of them it was issued to.
    val party: Optional[Text] = safely(jwt.json(t"azp").as[Text])

    val several = jwt.claims.audience match
      case _ :: _ :: _ => true
      case _           => false

    if several && party.absent
    then abort(invalid(t"it is for several audiences but names no authorized party"))

    party.let: party =>
      if party != client then abort(invalid(t"it was issued to the client $party"))

    nonce.let: expected =>
      if safely(jwt.json(t"nonce").as[Text]) != expected
      then abort(invalid(t"it does not answer this client's request"))

    safely(jwt.json(t"at_hash").as[Text]).let: expected =>
      if accessTokenHash(algorithm.name, authorization.key) != expected
      then abort(invalid(t"it does not belong with the access token"))

    jwt
