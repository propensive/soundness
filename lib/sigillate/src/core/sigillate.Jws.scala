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

import adversaria.*
import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import enigmatic.*
import fulminate.*
import gastronomy.*
import gossamer.*
import jacinta.*
import prepositional.*
import rudiments.*
import vacuous.*

import formatting.compactJsonFormatting

// A JSON Web Signature (RFC 7515): content with a signature or MAC over it and over the header
// that says how it was made. `Jws(payload, key)` signs; `jws.verify(key)` checks the signature
// and gives the payload, and is the only way to get it — a `Jws` that has merely been read
// from text is a claim, not a fact. The compact serialization is its `Text` codec
// (`jws.in[Text]`, `text.as[Jws]`); the JSON serializations are `flattened`, `Jws.general` and
// `Jws.read`.
object Jws:
  // Signs the payload with the key, under the algorithm the key's type stands for. Any other
  // parameters come from `header`; its `alg` is overwritten.
  def apply[encodable: Encodable in Data, cipher <: Cipher]
    ( payload: encodable, key: PrivateKey[cipher]^, header: Jose.Header = Jose.Header() )
    ( using algorithm: cipher is Jose.Algorithm, erased weakness: Permit[Weakness[cipher]] )
  :   Jws =

    val protection = header.copy(algorithm = algorithm.name).encoded
    val content = Jose.encode(encodable.encode(payload))
    val input: Data = Jose.ascii(t"$protection.$content")
    val binary = summon[Data is Encodable in Data]
    val signature = key.sign(input)(using binary, algorithm.cipher, weakness)

    val wire = algorithm.wire(signature).or:
      panic(m"the provider produced a signature that is not of its own algorithm's form")

    Jws(protection, Unset, content, wire)

  // An unsecured JWS (RFC 7515 §A.5): the payload with no signature at all, under `"alg":"none"`.
  def unsecured[encodable: Encodable in Data]
    ( payload: encodable, header: Jose.Header = Jose.Header() )
  :   Jws =

    val content = Jose.encode(encodable.encode(payload))
    Jws(header.copy(algorithm = t"none").encoded, Unset, content, Data())

  // The compact serialization (RFC 7515 §7.1): the protected header, payload and signature,
  // each base64url, joined with periods. It has no place for an unprotected header, which is
  // left out.
  given encodable: Jws is Encodable in Text = jws =>
    t"${jws.protection.or(t"")}.${jws.content}.${Jose.encode(jws.signature)}"

  given decodable: (tactic: Tactic[Jose.Error], diagnostics: Diagnostics)
  =>  ( (Jws is Decodable in Text)^{tactic} ) =
    text => text.cut(t".") match
      case List(protection, content, signature) =>
        Jws(protection, Unset, content, Jose.decode(signature))

      case _ =>
        abort(Jose.Error(Jose.Error.Reason.Malformed(t"a compact JWS has three parts")))

  // The members of the JSON serializations (RFC 7515 §7.2).
  private case class Signatory
    ( @name[Json](t"protected") protection: Optional[Text],
      header:                              Optional[Jose.Header],
      signature:                           Text )

  private case class Flattened
    ( payload:                             Text,
      @name[Json](t"protected") protection: Optional[Text],
      header:                              Optional[Jose.Header],
      signature:                           Text )

  private case class General(payload: Text, signatures: List[Signatory])

  // The general JSON serialization of several signatures over one payload. Every `Jws` must
  // carry the same payload: they are the results of signing it with different keys.
  def general(signatures: List[Jws])(using Tactic[Jose.Error], Diagnostics): Json =
    val content = signatures.head.lay(t"")(_.content)

    if signatures.exists(_.content != content) then
      val detail = t"the signatures are over different payloads"
      abort(Jose.Error(Jose.Error.Reason.Malformed(detail)))

    General
      ( content,
        signatures.map: jws =>
          Signatory(jws.protection, jws.unprotected, Jose.encode(jws.signature)) )
    . in[Json]

  // Reads either JSON serialization — flattened, with one signature, or general, with
  // several — as one `Jws` for each signature.
  def read(json: Json)(using Tactic[Jose.Error], Diagnostics): List[Jws] =
    mitigate:
      case Json.Error(_) => Jose.Error(Jose.Error.Reason.Malformed(t"it is not a JWS in JSON"))

    . protect:
        if safely(json(t"signatures").as[List[Json]]).present then
          val general = json.as[General]

          general.signatures.map: signatory =>
            Jws
              ( signatory.protection,
                signatory.header,
                general.payload,
                Jose.decode(signatory.signature) )
        else
          val flattened = json.as[Flattened]

          List:
            Jws
              ( flattened.protection,
                flattened.header,
                flattened.payload,
                Jose.decode(flattened.signature) )

case class Jws
  ( protection:  Optional[Text],
    unprotected: Optional[Jose.Header],
    content:     Text,
    signature:   Data ):

  // The integrity-protected header. It is what the signer wrote, but until `verify` has
  // succeeded nothing vouches for it.
  def header(using Tactic[Jose.Error], Diagnostics): Jose.Header =
    protection.lay(Jose.Header())(Jose.Header.read(_))

  // The flattened JSON serialization (RFC 7515 §7.2.2).
  def flattened: Json =
    Jws.Flattened(content, protection, unprotected, Jose.encode(signature)).in[Json]

  // The algorithm the object names — in its protected header or, failing that, its
  // unprotected one — checked against the one the key's type stands for.
  private def vet(expected: Text)(using Tactic[Jose.Error], Diagnostics): Unit =
    val parameters = header
    parameters.vet()

    val found =
      parameters.algorithm.or(unprotected.let(_.algorithm)).or:
        abort(Jose.Error(Jose.Error.Reason.Malformed(t"the header names no algorithm")))

    if found != expected then abort(Jose.Error(Jose.Error.Reason.Mismatch(expected, found)))

  private def input: Data = Jose.ascii(t"${protection.or(t"")}.$content")

  // Checks the signature against a public key, and gives the payload if it verifies.
  def verify[cipher <: Cipher](key: PublicKey[cipher])
    ( using algorithm:       cipher is Jose.Algorithm,
            erased weakness: ProcessingPermit[Weakness[cipher]],
            tactic:          Tactic[Jose.Error],
            diagnostics:     Diagnostics )
  :   Data =

    vet(algorithm.name)

    val candidate = algorithm.signature(signature).or:
      abort(Jose.Error(Jose.Error.Reason.BadSignature))

    val binary = summon[Data is Encodable in Data]

    if !key.verify(input, candidate)(using binary, algorithm.cipher, weakness)
    then abort(Jose.Error(Jose.Error.Reason.BadSignature))

    Jose.decode(content)

  // Checks the MAC against a symmetric key, and gives the payload if it verifies.
  def verify[cipher <: Cipher](key: SymmetricKey[cipher]^)
    ( using algorithm:       cipher is Jose.Algorithm,
            erased weakness: ProcessingPermit[Weakness[cipher]],
            tactic:          Tactic[Jose.Error],
            diagnostics:     Diagnostics )
  :   Data =

    vet(algorithm.name)

    val candidate = algorithm.signature(signature).or:
      abort(Jose.Error(Jose.Error.Reason.BadSignature))

    val binary = summon[Data is Encodable in Data]

    if !key.verify(input, candidate)(using binary, algorithm.cipher, weakness)
    then abort(Jose.Error(Jose.Error.Reason.BadSignature))

    Jose.decode(content)

  // The payload of an unsecured object, which nothing vouches for. The token makes accepting
  // one explicit; an object that names any other algorithm, or carries a signature, is refused.
  def payload(unsecured: Jose.Unsecured.type)(using Tactic[Jose.Error], Diagnostics): Data =
    val parameters = header
    parameters.vet()

    if parameters.algorithm.or(t"") != t"none" || signature.length != 0
    then abort(Jose.Error(Jose.Error.Reason.Secured))

    Jose.decode(content)
