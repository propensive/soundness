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
package chatelaine

import scala.reflect.Selectable.reflectiveSelectable

import adversaria.*
import anticipation.*
import aviation.*
import breviloquence.*
import contingency.*
import corpuscular.*
import denominative.*
import distillate.*
import enigmatic.*
import fulminate.*
import gastronomy.*
import gossamer.*
import hieroglyph.*
import jacinta.*
import monotonous.*
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import zephyrine.*

import alphabets.base64Url
import codepages.utf8Codepage

// Web Authentication (W3C WebAuthn Level 2): passwordless sign-in with a key pair an
// authenticator — a security key, a phone, a laptop's secure element — makes for one site and
// never lets out. This is the relying party's side, which is a server's.
//
// There are two ceremonies, each a challenge and its answer. In *registration* the server
// sends `WebAuthn.registration` options to the browser, and `WebAuthn.register` checks what
// comes back and gives the new `Credential` to store. In *authentication* it sends
// `WebAuthn.authentication` options, and `WebAuthn.authenticate` checks the answer against the
// stored credential, whose signature counter it returns advanced. Options and answers are the
// JSON forms a page passes to `PublicKeyCredential.parseCreationOptionsFromJSON` and gets from
// `credential.toJSON()`.
//
// Every check the standard requires of a relying party is made (§7.1 and §7.2): that the
// answer is to this challenge, from this origin, for this relying party; that the user was
// present, and verified if that was asked; and that the signature is the credential's.
object WebAuthn:
  object Error:
    given communicable: Reason is Communicable =
      case Reason.Malformed(detail)     => m"it was malformed: $detail"
      case Reason.Ceremony(found)       => m"it answers a $found ceremony"
      case Reason.Challenge             => m"it does not answer the challenge that was sent"
      case Reason.Origin(found)         => m"it came from the origin $found"
      case Reason.RelyingParty          => m"it was made for a different relying party"
      case Reason.Absent                => m"the user was not present"
      case Reason.Unverified            => m"the user was not verified"
      case Reason.Algorithm(identifier) => m"the algorithm $identifier is not accepted"
      case Reason.Attestation(detail)   => m"the attestation was not acceptable: $detail"
      case Reason.Format(format)        => m"the attestation format $format is not supported"
      case Reason.Credential            => m"it is from a different credential"
      case Reason.Signature             => m"the signature did not verify"
      case Reason.Counter               => m"the signature counter did not advance"

    enum Reason(val number: Int) extends Clarification:
      case Malformed(detail: Text)     extends Reason(1)
      case Ceremony(found: Text)       extends Reason(2)
      case Challenge                   extends Reason(3)
      case Origin(found: Text)         extends Reason(4)
      case RelyingParty                extends Reason(5)
      case Absent                      extends Reason(6)
      case Unverified                  extends Reason(7)
      case Algorithm(identifier: Long) extends Reason(8)
      case Attestation(detail: Text)   extends Reason(9)
      case Format(format: Text)        extends Reason(10)
      case Credential                  extends Reason(11)
      case Signature                   extends Reason(12)
      case Counter                     extends Reason(13)

  case class Error(reason: Error.Reason)(using Diagnostics)
  extends fulminate.Error(148, 1, reason.number)
    ( m"the WebAuthn response was not accepted because $reason" )

  // What a provider must offer for the ceremonies: random challenges, SHA-256, and the
  // signature algorithms a credential or an attestation certificate may use.
  type Provision = Certificate.Verification & Cose.Key.Verification

  // The relying party: the site. Its `id` is a domain — the site's own, or a registrable
  // suffix of it — which every credential is scoped to, and `origins` are the origins its
  // pages are served from (`https://globe.example`), one of which every answer must name.
  case class RelyingParty(id: Text, name: Text, origins: List[Text])

  // The account a credential is for. `id` is the user handle: up to 64 bytes that identify
  // the account and say nothing about the person, since an authenticator stores it.
  case class User(id: Data, name: Text, displayName: Text)

  object Challenge:
    // A fresh challenge: 32 random bytes, which is twice what the standard asks for.
    def apply()(using crypto: Crypto): Challenge = Challenge(crypto.random.bytes(32))

  // The bytes a ceremony's answer must echo. A challenge is for one ceremony: it is kept on
  // the server between sending the options and checking the answer, and then discarded.
  case class Challenge(bytes: Data)

  // What an attestation established about the authenticator that made a credential.
  enum Attestation:
    // The authenticator made no statement about itself.
    case Unattested

    // The statement was signed with the credential's own key: it proves the answer is
    // well-formed, and nothing about the authenticator.
    case Self

    // The statement was signed with a key certified by this chain, the attestation
    // certificate first. `trusted` is whether the chain was validated to one of the anchors
    // given to `register`; where none were given, it was not.
    case Certified(certificates: List[Certificate], trusted: Boolean)

  // A registered credential: what the relying party stores for an account, and gives back
  // to `authenticate`. `counter` is the authenticator's signature counter as last seen, and
  // `aaguid` identifies the authenticator's model, where it says.
  case class Credential
    ( id:          Data,
      key:         Cose.Key,
      counter:     Long,
      aaguid:      Data,
      attestation: Attestation )

  // The algorithms asked for when none are named: ES256, EdDSA and RS256, in that order of
  // preference, which between them every authenticator in use supports.
  val Algorithms: List[Long] = List(-7L, -8L, -257L)

  private def encode(data: Data): Text = data.serialize[Base64]

  private def malformed(detail: Text)(using Tactic[WebAuthn.Error], Diagnostics): Nothing =
    abort(WebAuthn.Error(Error.Reason.Malformed(detail)))

  private def decode(text: Text)(using Tactic[WebAuthn.Error], Diagnostics): Data =
    mitigate:
      case Serialization.Error(_, _) =>
        WebAuthn.Error(Error.Reason.Malformed(t"a value was not base64url"))

    . protect(text.deserialize[Base64])

  // The members of the options and responses, as the standard's JSON forms name them (§5.1).
  private case class Party(id: Text, name: Text)
  private case class Account(id: Text, name: Text, displayName: Text)
  private case class Parameter(@name[Json](t"type") kind: Text, alg: Long)
  private case class Descriptor(@name[Json](t"type") kind: Text, id: Text)
  private case class Selection(userVerification: Text)

  private case class Creation
    ( @name[Json](t"rp")                 party:       Party,
      user:                                           Account,
      challenge:                                      Text,
      @name[Json](t"pubKeyCredParams")   parameters:  List[Parameter],
      @name[Json](t"excludeCredentials") exclusions:  List[Descriptor],
      authenticatorSelection:                         Selection,
      attestation:                                    Text )

  private case class Request
    ( challenge:                                    Text,
      rpId:                                         Text,
      @name[Json](t"allowCredentials") permitted:   List[Descriptor],
      userVerification:                             Text )

  private case class Attested
    ( @name[Json](t"clientDataJSON") clientData: Text, attestationObject: Text )

  private case class Asserted
    ( @name[Json](t"clientDataJSON") clientData: Text,
      authenticatorData:                         Text,
      signature:                                 Text )

  private case class Registration(id: Text, @name[Json](t"type") kind: Text, response: Attested)
  private case class Assertion(id: Text, @name[Json](t"type") kind: Text, response: Asserted)

  private case class ClientData
    ( @name[Json](t"type") kind: Text,
      challenge:                Text,
      origin:                   Text,
      crossOrigin:              Optional[Boolean] )

  private def verification(required: Boolean): Text =
    if required then t"required" else t"preferred"

  // The options for registering a credential, to be given to the page and from there to
  // `navigator.credentials.create`. `existing` are the identifiers of credentials the account
  // already has, so that one authenticator is not registered twice. `verification` asks that
  // the user be verified — by a PIN or a fingerprint — and not merely present; `attestation`
  // is the conveyance asked for, `none` unless the relying party means to examine it.
  def registration
    ( relyingParty: RelyingParty,
      user:         User,
      challenge:    Challenge,
      existing:     List[Data] = Nil,
      algorithms:   List[Long] = Algorithms,
      verification: Boolean    = false,
      attestation:  Text       = t"none" )
  :   Json =

    Creation
      ( Party(relyingParty.id, relyingParty.name),
        Account(encode(user.id), user.name, user.displayName),
        encode(challenge.bytes),
        algorithms.map(Parameter(t"public-key", _)),
        existing.map(id => Descriptor(t"public-key", encode(id))),
        Selection(WebAuthn.verification(verification)),
        attestation )
    . in[Json]

  // The options for authenticating, to be given to `navigator.credentials.get`. `permitted`
  // are the identifiers of the account's credentials; left empty, the authenticator offers
  // whichever discoverable credentials it holds for the relying party.
  def authentication
    ( relyingParty: RelyingParty,
      challenge:    Challenge,
      permitted:    List[Data] = Nil,
      verification: Boolean    = false )
  :   Json =

    Request
      ( encode(challenge.bytes),
        relyingParty.id,
        permitted.map(id => Descriptor(t"public-key", encode(id))),
        WebAuthn.verification(verification) )
    . in[Json]

  private def sha256(data: Data)(using Hash in Sha2[256]): Data = data.digest[Sha2[256]].data

  // §7.1 steps 5–9 and §7.2 steps 10–13: the client data must be for this kind of ceremony,
  // echo this challenge, and come from one of the relying party's origins.
  private def vet
    ( clientData: Data, ceremony: Text, challenge: Challenge, relyingParty: RelyingParty )
    ( using Tactic[WebAuthn.Error], Diagnostics )
  :   Unit =

    val data =
      mitigate:
        case Parse.Error(_, _, _) => WebAuthn.Error(Error.Reason.Malformed(t"the client data"))
        case Json.Error(_)        => WebAuthn.Error(Error.Reason.Malformed(t"the client data"))

      . protect(Chain(clientData).read[Json].as[ClientData])

    if data.kind != ceremony then abort(WebAuthn.Error(Error.Reason.Ceremony(data.kind)))

    if !Crypto.constantTimeEquals(decode(data.challenge), challenge.bytes)
    then abort(WebAuthn.Error(Error.Reason.Challenge))

    if !relyingParty.origins.has(data.origin) || data.crossOrigin.or(false)
    then abort(WebAuthn.Error(Error.Reason.Origin(data.origin)))

  private def be(bytes: Data, offset: Int, count: Int): Long =
    val array = Array.unsafeJvm(bytes)

    def recur(index: Int, value: Long): Long =
      if index == count then value
      else recur(index + 1, (value << 8) | (array(offset + index) & 0xffL))

    recur(0, 0L)

  // The length of the CBOR data item at `offset` (RFC 8949 §3), which is how the end of the
  // credential public key inside authenticator data is found: the key is followed, without
  // a length of its own, by any extensions. `-1` for anything that is not a well-formed,
  // definite-length item within the bytes.
  private def item(bytes: Data, offset: Int): Int =
    val array = Array.unsafeJvm(bytes)

    // The end of `count` consecutive items from `position`, or -1.
    def items(position: Int, count: Long): Int =
      if position < 0 then -1 else if count == 0L then position
      else items(end(position), count - 1L)

    def end(position: Int): Int =
      if position < 0 || position >= array.length then -1 else
        val major = (array(position) & 0xff) >>> 5
        val minor = array(position) & 0x1f

        val head =
          if minor < 24 then 1 else if minor == 24 then 2 else if minor == 25 then 3
          else if minor == 26 then 5 else if minor == 27 then 9 else -1

        if head < 0 || position + head > array.length then -1 else
          val argument: Long =
            if minor < 24 then minor.toLong else be(bytes, position + 1, head - 1)

          val next = position + head

          if argument < 0L || argument > array.length then -1
          else if major == 2 || major == 3 then
            if next + argument > array.length then -1 else next + argument.toInt
          else if major == 4 then items(next, argument)
          else if major == 5 then items(next, argument*2L)
          else if major == 6 then end(next)
          else next

    end(offset).puncture(-1).lay(-1)(_ - offset)

  // Authenticator data (§6.1): the hash of the relying party's identifier, the flags, the
  // signature counter and, when a credential is being attested, the credential itself.
  private case class Authenticator
    ( party:   Data,
      flags:   Int,
      counter: Long,
      aaguid:  Optional[Data],
      id:      Optional[Data],
      key:     Optional[Data] ):

    def present: Boolean = (flags & 0x01) != 0
    def verified: Boolean = (flags & 0x04) != 0
    def eligible: Boolean = (flags & 0x08) != 0
    def backedUp: Boolean = (flags & 0x10) != 0

  private def authenticator(bytes: Data)(using Tactic[WebAuthn.Error], Diagnostics)
  :   Authenticator =

    if bytes.length < 37 then malformed(t"the authenticator data is too short")
    val flags = Array.unsafeJvm(bytes)(32) & 0xff
    val counter = be(bytes, 33, 4)

    if (flags & 0x40) == 0 then Authenticator(bytes.keep(32), flags, counter, Unset, Unset, Unset)
    else
      if bytes.length < 55 then malformed(t"the attested credential is too short")
      val size = be(bytes, 53, 2).toInt
      if bytes.length < 55 + size then malformed(t"the credential identifier is cut short")
      val length = item(bytes, 55 + size)
      if length < 0 then malformed(t"the credential public key is not well-formed CBOR")

      Authenticator
        ( bytes.keep(32),
          flags,
          counter,
          bytes.skip(37).keep(16),
          bytes.skip(55).keep(size),
          bytes.skip(55 + size).keep(length) )

  // §7.1 steps 13–17 and §7.2 steps 15–17: the authenticator data must be for this relying
  // party, and the user present — and verified, where that was asked.
  private def vet(data: Authenticator, relyingParty: RelyingParty, verification: Boolean)
    ( using Hash in Sha2[256], Tactic[WebAuthn.Error], Diagnostics )
  :   Unit =

    if !Crypto.constantTimeEquals(data.party, sha256(relyingParty.id.in[Data]))
    then abort(WebAuthn.Error(Error.Reason.RelyingParty))

    if !data.present then abort(WebAuthn.Error(Error.Reason.Absent))
    if verification && !data.verified then abort(WebAuthn.Error(Error.Reason.Unverified))

    // A credential cannot be backed up without being eligible for it (§6.1).
    if data.backedUp && !data.eligible then malformed(t"the backup flags contradict each other")

  private def cbor[result](detail: Text)(block: Tactic[Cbor.Error] ?=> result)
    ( using Tactic[WebAuthn.Error], Diagnostics )
  :   result =

    mitigate:
      case error: Cbor.Error => WebAuthn.Error(Error.Reason.Malformed(detail))

    . protect(block)

  private def member(map: Cbor.Ast, name: Text): Optional[Cbor.Ast] =
    if !map.isMap then Unset else
      val index = map.index(name.s)
      if index < 0 then Unset else map.value(index)

  // The object identifier of the extension in which a packed attestation certificate names
  // the model of the authenticator it certifies (§8.2.1): `id-fido-gen-ce-aaguid`.
  private val Aaguid: List[Int] = List(1, 3, 6, 1, 4, 1, 45724, 1, 1, 4)

  // The packed attestation statement (§8.2): a signature over the authenticator data and
  // the hash of the client data, made either with the credential's own key or with a key
  // an attestation certificate certifies.
  private def packed
    ( statement: Cbor.Ast,
      signed:    Data,
      key:       Cose.Key,
      aaguid:    Data,
      anchors:   List[Certificate] )
    ( using clock:       Clock,
            crypto:      Provision,
            tactic:      Tactic[WebAuthn.Error],
            diagnostics: Diagnostics )
  :   Attestation =

    def refused(detail: Text): Nothing = abort(WebAuthn.Error(Error.Reason.Attestation(detail)))

    val algorithm: Long = cbor(t"the attestation statement"):
      member(statement, t"alg").lay(malformed(t"the attestation names no algorithm"))(_.long)

    val signature: Data = cbor(t"the attestation statement"):
      member(statement, t"sig").lay(malformed(t"the attestation has no signature"))(_.byteString)

    member(statement, t"x5c").lay
      ( // Self attestation: the algorithm must be the credential's own.
        if algorithm != key.algorithm then refused(t"its algorithm is not the credential's")
        else if !key.verify(signed, signature) then refused(t"its signature did not verify")
        else Attestation.Self ):

      chain =>
        val certificates: List[Certificate] = cbor(t"the attestation's certificates"):
          if !chain.isArray || chain.elements == 0 then malformed(t"the certificate chain is empty")

          List.range(0, chain.elements).map: index =>
            val document: Data = chain.element(index).byteString

            mitigate:
              case Asn1.Error(_) =>
                WebAuthn.Error(Error.Reason.Malformed(t"an attestation certificate"))

            . protect(Der(document).as[Certificate])

        val certificate = certificates.head.or(malformed(t"the certificate chain is empty"))

        val certified = certificate.publicKey.or:
          refused(t"the attestation certificate carries no key")

        if !Cose.Key(algorithm, certified).verify(signed, signature)
        then refused(t"its signature did not verify")

        // §8.2.1: an attestation certificate is not a certificate authority's, and where it
        // names the authenticator's model, that must be the model of this authenticator.
        if certificate.authority then refused(t"the attestation certificate is an authority's")

        certificate.extension(Aaguid).let: extension =>
          val named: Optional[Data] = safely(Der(extension).as[Asn1]) match
            case Asn1.OctetString(named) => named
            case _                       => Unset

          if !named.lay(false)(Crypto.constantTimeEquals(_, aaguid))
          then refused(t"the attestation certificate is for another model of authenticator")

        if anchors.nil then Attestation.Certified(certificates, false) else
          mitigate:
            case error: Certificate.Path.Error =>
              WebAuthn.Error(Error.Reason.Attestation(error.message.text))

          . protect(Certificate.validate(certificates, anchors))

          Attestation.Certified(certificates, true)

  // Checks the answer to a registration ceremony — the JSON of the `PublicKeyCredential`
  // the page got from `navigator.credentials.create` — and gives the credential to store.
  //
  // `challenge` is the one the options were made with, and `verification` and `algorithms`
  // what they asked for. `anchors` are the attestation roots the relying party trusts: with
  // none, an attestation certificate's chain is reported but not validated, which is right
  // for the many relying parties that do not restrict which authenticators they accept.
  //
  // Two things are left to the caller, because only it has the data: that the credential's
  // `id` is not already registered to another account (§7.1 step 22), and the storing.
  def register
    ( response:     Json,
      challenge:    Challenge,
      relyingParty: RelyingParty,
      verification: Boolean           = false,
      algorithms:   List[Long]        = Algorithms,
      anchors:      List[Certificate] = Nil )
    ( using clock:       Clock,
            crypto:      Provision,
            hash:        Hash in Sha2[256],
            tactic:      Tactic[WebAuthn.Error],
            diagnostics: Diagnostics )
  :   Credential =

    val registration =
      mitigate:
        case Json.Error(_) => WebAuthn.Error(Error.Reason.Malformed(t"the registration response"))

      . protect(response.as[Registration])

    val clientData = decode(registration.response.clientData)
    vet(clientData, t"webauthn.create", challenge, relyingParty)

    val document = decode(registration.response.attestationObject)

    val attestation: Cbor.Ast = cbor(t"the attestation object")(Cbor.Ast.parse(document))

    val format: Text = cbor(t"the attestation object"):
      member(attestation, t"fmt").lay(malformed(t"the attestation names no format"))(_.string.tt)

    val statement: Cbor.Ast =
      member(attestation, t"attStmt").or(malformed(t"the attestation has no statement"))

    val authenticatorData: Data = cbor(t"the attestation object"):
      member(attestation, t"authData").lay(malformed(t"there is no authenticator data")):
        _.byteString

    val data = authenticator(authenticatorData)
    vet(data, relyingParty, verification)

    val id = data.id.or(malformed(t"no credential was attested"))
    val aaguid = data.aaguid.or(malformed(t"no credential was attested"))
    val encoded = data.key.or(malformed(t"no credential was attested"))

    if id.length == 0 || id.length > 1023 then malformed(t"the credential identifier's length")

    if registration.id != encode(id) then abort(WebAuthn.Error(Error.Reason.Credential))

    val key: Cose.Key =
      mitigate:
        case error: Cbor.Error =>
          WebAuthn.Error(Error.Reason.Malformed(t"the credential public key"))

        case Cose.Error(reason) =>
          WebAuthn.Error(Error.Reason.Malformed(t"the credential public key"))

      . protect(Cose.Key.parse(encoded))

    if !algorithms.has(key.algorithm)
    then abort(WebAuthn.Error(Error.Reason.Algorithm(key.algorithm)))

    val signed: Data = Array.concat(authenticatorData, sha256(clientData))

    val established: Attestation =
      if format == t"none" then
        if statement.isMap && statement.entries == 0 then Attestation.Unattested
        else abort(WebAuthn.Error(Error.Reason.Attestation(t"a statement with the format none")))
      else if format == t"packed" then packed(statement, signed, key, aaguid, anchors)
      else abort(WebAuthn.Error(Error.Reason.Format(format)))

    Credential(id, key, data.counter, aaguid, established)

  // Checks the answer to an authentication ceremony — the JSON of the `PublicKeyCredential`
  // the page got from `navigator.credentials.get` — against the stored credential it names,
  // and gives that credential back with its signature counter advanced, to be stored again.
  //
  // The caller finds the credential by the response's `id` (`WebAuthn.identifier`), among
  // those of the account that is signing in.
  //
  // A counter that has not advanced means the credential's private key is in two places —
  // it has been cloned — and the answer is refused. An authenticator that does not count
  // reports zero every time, which is accepted while the stored counter is zero too.
  def authenticate
    ( response:     Json,
      challenge:    Challenge,
      relyingParty: RelyingParty,
      credential:   Credential,
      verification: Boolean = false )
    ( using crypto:      Provision,
            hash:        Hash in Sha2[256],
            tactic:      Tactic[WebAuthn.Error],
            diagnostics: Diagnostics )
  :   Credential =

    val assertion =
      mitigate:
        case Json.Error(_) => WebAuthn.Error(Error.Reason.Malformed(t"the assertion response"))

      . protect(response.as[Assertion])

    if !Crypto.constantTimeEquals(decode(assertion.id), credential.id)
    then abort(WebAuthn.Error(Error.Reason.Credential))

    val clientData = decode(assertion.response.clientData)
    vet(clientData, t"webauthn.get", challenge, relyingParty)

    val authenticatorData = decode(assertion.response.authenticatorData)
    val data = authenticator(authenticatorData)
    vet(data, relyingParty, verification)

    val signed: Data = Array.concat(authenticatorData, sha256(clientData))

    if !credential.key.verify(signed, decode(assertion.response.signature))
    then abort(WebAuthn.Error(Error.Reason.Signature))

    if (data.counter != 0L || credential.counter != 0L) && data.counter <= credential.counter
    then abort(WebAuthn.Error(Error.Reason.Counter))

    credential.copy(counter = data.counter)

  // The identifier of the credential a response is from, by which the caller finds the
  // stored `Credential` to pass to `authenticate`.
  def identifier(response: Json)(using Tactic[WebAuthn.Error], Diagnostics): Data =
    val id =
      mitigate:
        case Json.Error(_) => WebAuthn.Error(Error.Reason.Malformed(t"the response has no id"))

      . protect(response(t"id").as[Text])

    decode(id)
