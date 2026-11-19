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
import jacinta.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

import dynamicAccess.dynamicJson
import formatting.compactJsonFormatting

// A JSON Web Token (RFC 7519): a set of claims, as a JSON object, carried as the payload of a
// JWS (or a JWE). The seven registered claims are `Jwt.Claims`; any others an application
// defines are read from, and written to, the same object: `jwt.json.as[Roles]`.
//
// `Jwt.verify(text, key)` is the whole of what a server does with a bearer token: it checks
// the signature against the key (whose type fixes the algorithm), reads the claims, and checks
// that the token is within its validity period by the `Clock` in scope and, if asked, that it
// was issued by and for the expected parties. Reading a token's claims without verifying it is
// not offered.
object Jwt:
  // The registered claims (RFC 7519 §4.1). `audience` is a list because a token may name
  // several; a token that names one writes it as a plain string.
  case class Claims
    ( issuer:    Optional[Text]               = Unset,
      subject:   Optional[Text]               = Unset,
      audience:  List[Text]                   = Nil,
      expiry:    Optional[Instant over Unix]  = Unset,
      notBefore: Optional[Instant over Unix]  = Unset,
      issuedAt:  Optional[Instant over Unix]  = Unset,
      id:        Optional[Text]               = Unset )

  // A `NumericDate` (RFC 7519 §2) is seconds since the epoch, possibly with a fraction; an
  // `Instant` counts milliseconds.
  private def seconds(instant: Instant over Unix): Long = instant.long/1000L

  private def text(json: Json, name: Text)(using Tactic[Jose.Error], Diagnostics): Optional[Text] =
    mitigate:
      case Json.Error(_) => Jose.Error(Jose.Error.Reason.Claim(name))

    . protect(json(name).as[Optional[Text]])

  private def instant(json: Json, name: Text)(using Tactic[Jose.Error], Diagnostics)
  :   Optional[Instant over Unix] =

    mitigate:
      case Json.Error(_) => Jose.Error(Jose.Error.Reason.Claim(name))

    . protect:
        json(name).as[Optional[Double]].let: seconds =>
          Instant.of[Unix]((seconds*1000.0).toLong)

  // `aud` is an array of strings or, for a single audience, one string (RFC 7519 §4.1.3).
  private def audience(json: Json)(using Tactic[Jose.Error], Diagnostics): List[Text] =
    safely(json(t"aud").as[List[Text]]).or:
      mitigate:
        case Json.Error(_) => Jose.Error(Jose.Error.Reason.Claim(t"aud"))

      . protect(json(t"aud").as[Optional[Text]].lay(Nil)(List(_)))

  // A token of the registered claims alone.
  private case class Empty()

  def apply(claims: Claims): Jwt = Jwt(claims, Empty())

  // A token of the registered claims and an application's own, which must encode as a JSON
  // object. A registered claim set in `claims` replaces a member of the same name.
  def apply[custom: Encodable in Json](claims: Claims, custom: custom): Jwt =
    val json0 = custom.in[Json]
    val json1 = claims.issuer.lay(json0)(json0.updateDynamic("iss")(_))
    val json2 = claims.subject.lay(json1)(json1.updateDynamic("sub")(_))

    val json3 = claims.audience match
      case Nil          => json2
      case List(single) => json2.updateDynamic("aud")(single)
      case several      => json2.updateDynamic("aud")(several)

    val json4 = claims.expiry.lay(json3): instant =>
      json3.updateDynamic("exp")(seconds(instant))

    val json5 = claims.notBefore.lay(json4): instant =>
      json4.updateDynamic("nbf")(seconds(instant))

    val json6 = claims.issuedAt.lay(json5): instant =>
      json5.updateDynamic("iat")(seconds(instant))

    new Jwt(claims, claims.id.lay(json6)(json6.updateDynamic("jti")(_)))

  // The claims of a payload that has already been authenticated — what `jws.verify(key)` or
  // `jwe.decrypt[content](key)` returned.
  def read(payload: Data)(using Tactic[Jose.Error], Diagnostics): Jwt =
    val json = Jose.parse(payload)

    val claims =
      Claims
        ( text(json, t"iss"),
          text(json, t"sub"),
          audience(json),
          instant(json, t"exp"),
          instant(json, t"nbf"),
          instant(json, t"iat"),
          text(json, t"jti") )

    new Jwt(claims, json)

  // Verifies a signed token in the compact serialization against a public key, and checks its
  // validity period.
  def verify[cipher <: Cipher](token: Text, key: PublicKey[cipher])
    ( using algorithm:       cipher is Jose.Algorithm,
            erased weakness: ProcessingPermit[Weakness[cipher]],
            clock:           Clock,
            tactic:          Tactic[Jose.Error],
            diagnostics:     Diagnostics )
  :   Jwt =

    read(token.as[Jws].verify(key)).validate()

  // Verifies a token authenticated with a shared key.
  def verify[cipher <: Cipher](token: Text, key: SymmetricKey[cipher]^)
    ( using algorithm:       cipher is Jose.Algorithm,
            erased weakness: ProcessingPermit[Weakness[cipher]],
            clock:           Clock,
            tactic:          Tactic[Jose.Error],
            diagnostics:     Diagnostics )
  :   Jwt =

    read(token.as[Jws].verify(key)).validate()

case class Jwt(claims: Jwt.Claims, json: Json):
  // The claims as the UTF-8 bytes of their JSON: the payload of the JWS or JWE that carries
  // the token.
  def payload: Data = Jose.ascii(json.show)

  // Signs the token: a JWS whose header says it is one (`"typ":"JWT"`).
  def sign[cipher <: Cipher](key: PrivateKey[cipher]^, header: Jose.Header = Jose.Header())
    ( using algorithm: cipher is Jose.Algorithm, erased weakness: Permit[Weakness[cipher]] )
  :   Jws =
    Jws(payload, key, header.copy(mediaType = header.mediaType.or(t"JWT")))

  // Checks the claims a recipient must (RFC 7519 §7.2): that the token has not expired and
  // is not presented before its time, by the clock in scope and with `leeway` seconds allowed
  // for the difference between two machines' clocks; and, where they are given, that it names
  // the expected issuer and is addressed to the expected audience. A token with no `exp`
  // never expires, and one with no `aud` is addressed to nobody in particular, so it fails a
  // check for an audience.
  def validate(issuer: Optional[Text] = Unset, audience: Optional[Text] = Unset, leeway: Int = 0)
    ( using clock: Clock, tactic: Tactic[Jose.Error], diagnostics: Diagnostics )
  :   Jwt =

    val moment = clock().long
    val tolerance = leeway*1000L

    claims.expiry.let: expiry =>
      if moment - tolerance >= expiry.long then abort(Jose.Error(Jose.Error.Reason.Expired))

    claims.notBefore.let: start =>
      if moment + tolerance < start.long then abort(Jose.Error(Jose.Error.Reason.Premature))

    issuer.let: expected =>
      if claims.issuer != expected
      then abort(Jose.Error(Jose.Error.Reason.Issuer(claims.issuer.or(t"nobody"))))

    audience.let: expected =>
      if !claims.audience.has(expected) then abort(Jose.Error(Jose.Error.Reason.Audience))

    this
