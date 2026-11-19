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

import soundness.*

import strategies.throwUnsafely
import charsets.utf8Charset, codepages.utf8Codepage, textSanitizers.skipSanitizer
import errorDiagnostics.stackTracesDiagnostics
import providers.javaBaseProvider
import cloaks.heapCloak
import alphabets.base64Url
import formatting.compactJsonFormatting
import dysasymptotics.linearSize
import chronometries.unixChronometry

object Tests extends Suite("webauthn", m"Chatelaine tests"):
  val globe = WebAuthn.RelyingParty(t"globe.example", t"The Globe", List(t"https://globe.example"))
  val burbage = WebAuthn.User(t"burbage-1567".in[Data], t"burbage", t"Richard Burbage")

  val period = Instant(1_600_000_000_000L) ~ Instant(1_900_000_000_000L)
  // Within the validity of the tests' own certificates and of the specification's.
  given Clock = Clock.fixed(Instant.of[Unix](1_750_000_000_000L))

  // An ES256 authenticator, as almost every security key is, with an attestation key
  // certified by its maker's root.
  class SecurityKey(origin: Text = t"https://globe.example") extends Authenticator(origin):
    val key: PrivateKey[Ecdsa[256]] = PrivateKey.generate[Ecdsa[256]]()
    val attestationKey: PrivateKey[Ecdsa[256]] = PrivateKey.generate[Ecdsa[256]]()
    val makerKey: PrivateKey[Ecdsa[256]] = PrivateKey.generate[Ecdsa[256]]()
    val maker = Distinguished(commonName = t"Lord Chamberlain's Keys", organization = t"Globe")

    val model =
      Distinguished
        ( commonName         = t"Folio Key",
          organization       = t"Globe",
          organizationalUnit = t"Authenticator Attestation",
          country            = t"GB" )

    val root = Certificate.selfSigned(maker, makerKey, period, BigInt(1), authority = true)

    val attestation =
      Certificate.issued(model, attestationKey.public, maker, makerKey, period, BigInt(2))

    def algorithm: Long = -7L
    def publicKey: Data = Cose.Key(key.public).encoded.or(panic(m"an EC key has a COSE form"))
    def sign(message: Data): Data = key.sign(message).bytes
    override def attest(message: Data): Data = attestationKey.sign(message).bytes
    override def chain: List[Certificate] = List(attestation)

  // An EdDSA authenticator.
  class EdwardsKey extends Authenticator(t"https://globe.example"):
    val key: PrivateKey[Ed25519] = PrivateKey.generate[Ed25519]()
    def algorithm: Long = -8L
    def publicKey: Data = Cose.Key(key.public).encoded.or(panic(m"an OKP key has a COSE form"))
    def sign(message: Data): Data = key.sign(message).bytes

  // Registers an authenticator, giving the credential.
  def enrol(authenticator: Authenticator, conveyance: Conveyance = Conveyance.None)
  :   WebAuthn.Credential =
    val challenge = WebAuthn.Challenge()
    val options = WebAuthn.registration(globe, burbage, challenge)
    WebAuthn.register(authenticator.create(options, conveyance), challenge, globe)

  def refused(block: => Any): WebAuthn.Error.Reason = capture[WebAuthn.Error](block).reason

  // The relying party of the specification's test vectors.
  val example = WebAuthn.RelyingParty(t"example.org", t"Example", List(t"https://example.org"))
  val everything: List[Long] = List(-7L, -8L, -35L, -36L, -257L)

  def bytes(hex: Text): Data =
    import alphabets.hexLowerCase
    hex.deserialize[Hex]

  def encoded(hex: Text): Text = bytes(hex).serialize[Base64]

  def registration(vector: Vectors.Ceremony): Json =
    t"""{"id":"${encoded(vector.identifier)}","type":"public-key",
         "response":{"clientDataJSON":"${encoded(vector.clientData)}",
                     "attestationObject":"${encoded(vector.attestationObject)}"}}""".read[Json]

  def assertion(vector: Vectors.Ceremony): Json =
    t"""{"id":"${encoded(vector.identifier)}","type":"public-key",
         "response":{"clientDataJSON":"${encoded(vector.assertedClient)}",
                     "authenticatorData":"${encoded(vector.authenticatorData)}",
                     "signature":"${encoded(vector.signature)}"}}""".read[Json]

  val root: Certificate = Der(bytes(Vectors.root)).as[Certificate]

  // Both of a vector's ceremonies: the credential it registers, and that credential after
  // it has signed in.
  def reference(vector: Vectors.Ceremony, anchors: List[Certificate] = Nil)
  :   (WebAuthn.Credential, WebAuthn.Credential) =
    val challenge = WebAuthn.Challenge(bytes(vector.challenge))

    val credential =
      WebAuthn.register
        ( registration(vector), challenge, example, algorithms = everything, anchors = anchors )

    val asserted = WebAuthn.Challenge(bytes(vector.assertedChallenge))
    (credential, WebAuthn.authenticate(assertion(vector), asserted, example, credential))

  def run(): Unit =
    suite(m"Options"):
      val challenge = WebAuthn.Challenge(Array.tabulate[Byte](32)(_.toByte))
      val existing: Data = Array.fill[Byte](4)(9)

      test(m"A fresh challenge is 32 bytes, and differs from the last"):
        val one = WebAuthn.Challenge()
        val two = WebAuthn.Challenge()
        (one.bytes.length, one.bytes.serialize[Base64] != two.bytes.serialize[Base64])
      . assert(_ == (32, true))

      test(m"Registration options are the JSON a page gives to the browser"):
        WebAuthn.registration(globe, burbage, challenge, existing = List(existing)).show
      . assert(_ == t"""{"rp":{"id":"globe.example","name":"The Globe"},"user":{"id":"YnVyYmFnZS0xNTY3","name":"burbage","displayName":"Richard Burbage"},"challenge":"AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8","pubKeyCredParams":[{"type":"public-key","alg":-7},{"type":"public-key","alg":-8},{"type":"public-key","alg":-257}],"excludeCredentials":[{"type":"public-key","id":"CQkJCQ"}],"authenticatorSelection":{"userVerification":"preferred"},"attestation":"none"}""")

      test(m"Authentication options name the relying party and the permitted credentials"):
        WebAuthn.authentication(globe, challenge, List(existing), verification = true).show
      . assert(_ == t"""{"challenge":"AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8","rpId":"globe.example","allowCredentials":[{"type":"public-key","id":"CQkJCQ"}],"userVerification":"required"}""")

    suite(m"Ceremonies"):
      test(m"A security key registers, and its credential is unattested"):
        val authenticator = SecurityKey()
        val credential = enrol(authenticator)

        ( credential.id.serialize[Base64] == authenticator.credential.serialize[Base64],
          credential.key.algorithm,
          credential.counter,
          credential.attestation )
      . assert(_ == (true, -7L, 1L, WebAuthn.Attestation.Unattested))

      test(m"A registered security key signs in, and its counter advances"):
        val authenticator = SecurityKey()
        val credential = enrol(authenticator)
        val challenge = WebAuthn.Challenge()
        val options = WebAuthn.authentication(globe, challenge, List(credential.id))
        WebAuthn.authenticate(authenticator.get(options), challenge, globe, credential).counter
      . assert(_ == 2L)

      test(m"A security key signs in again and again"):
        val authenticator = SecurityKey()

        def signIn(credential: WebAuthn.Credential): WebAuthn.Credential =
          val challenge = WebAuthn.Challenge()
          val options = WebAuthn.authentication(globe, challenge)
          WebAuthn.authenticate(authenticator.get(options), challenge, globe, credential)

        signIn(signIn(signIn(enrol(authenticator)))).counter
      . assert(_ == 4L)

      test(m"The credential a response is from is found by its identifier"):
        val authenticator = SecurityKey()
        val credential = enrol(authenticator)
        val challenge = WebAuthn.Challenge()
        val response = authenticator.get(WebAuthn.authentication(globe, challenge))
        WebAuthn.identifier(response).serialize[Base64] == credential.id.serialize[Base64]
      . assert(identity)

      test(m"An EdDSA authenticator registers and signs in"):
        val authenticator = EdwardsKey()
        val credential = enrol(authenticator)
        val challenge = WebAuthn.Challenge()
        val response = authenticator.get(WebAuthn.authentication(globe, challenge))
        val updated = WebAuthn.authenticate(response, challenge, globe, credential)
        (credential.key.algorithm, updated.counter)
      . assert(_ == (-8L, 2L))

      test(m"A verified user satisfies a ceremony that requires verification"):
        val authenticator = SecurityKey()
        authenticator.verifies = true
        val challenge = WebAuthn.Challenge()
        val options = WebAuthn.registration(globe, burbage, challenge, verification = true)

        WebAuthn.register(authenticator.create(options), challenge, globe, verification = true)
        . counter
      . assert(_ == 1L)

    suite(m"Attestation"):
      test(m"Self attestation is recognised as such"):
        enrol(EdwardsKey(), Conveyance.Self).attestation
      . assert(_ == WebAuthn.Attestation.Self)

      test(m"A certified attestation is reported, untrusted where no root is given"):
        enrol(SecurityKey(), Conveyance.Certified).attestation match
          case WebAuthn.Attestation.Certified(certificates, trusted) => (certificates.size, trusted)
          case _                                                     => (0, true)
      . assert(_ == (1, false))

      test(m"A certified attestation is trusted when its chain reaches a given root"):
        val authenticator = SecurityKey()
        val challenge = WebAuthn.Challenge()
        val options = WebAuthn.registration(globe, burbage, challenge, attestation = t"direct")
        val response = authenticator.create(options, Conveyance.Certified)

        WebAuthn.register(response, challenge, globe, anchors = List(authenticator.root))
        . attestation match
            case WebAuthn.Attestation.Certified(_, trusted) => trusted
            case _                                          => false
      . assert(identity)

      test(m"A certified attestation is refused when its chain reaches no given root"):
        val authenticator = SecurityKey()
        val other = SecurityKey()
        val challenge = WebAuthn.Challenge()
        val response = authenticator.create(WebAuthn.registration(globe, burbage, challenge), Conveyance.Certified)
        refused(WebAuthn.register(response, challenge, globe, anchors = List(other.root)))
      . assert:
          case WebAuthn.Error.Reason.Attestation(detail) => detail.contains(t"trusted certificate")
          case _                                         => false

      test(m"An attestation format that is not supported is refused"):
        val authenticator = SecurityKey()
        val challenge = WebAuthn.Challenge()
        val response = authenticator.create(WebAuthn.registration(globe, burbage, challenge), Conveyance.Unknown)
        refused(WebAuthn.register(response, challenge, globe))
      . assert(_ == WebAuthn.Error.Reason.Format(t"fido-u2f"))

    suite(m"The specification's test vectors"):
      test(m"16.1: the attestation root is a certificate authority's own certificate"):
        (root.authority, root.signedBy(root), root.subject.let(_.organization))
      . assert(_ == (true, true, t"W3C"))

      test(m"16.2: an ES256 credential with no attestation registers and signs in"):
        val (credential, updated) = reference(Vectors.none)
        (credential.key.algorithm, credential.attestation, updated.counter)
      . assert(_ == (-7L, WebAuthn.Attestation.Unattested, 0L))

      test(m"16.2: the credential's identifier and model are read from the attestation"):
        val (credential, _) = reference(Vectors.none)
        import alphabets.hexLowerCase
        (credential.id.serialize[Hex], credential.aaguid.serialize[Hex])
      . assert(_ == (
          t"f91f391db4c9b2fde0ea70189cba3fb63f579ba6122b33ad94ff3ec330084be4",
          t"8446ccb9ab1db374750b2367ff6f3a1f" ))

      test(m"16.3: an ES256 credential with self attestation registers and signs in"):
        reference(Vectors.self)(0).attestation
      . assert(_ == WebAuthn.Attestation.Self)

      test(m"16.4: a cross-origin answer is refused"):
        refused(reference(Vectors.crossOrigin))
      . assert(_ == WebAuthn.Error.Reason.Origin(t"https://example.org"))

      test(m"16.6: a credential with the longest identifier registers and signs in"):
        reference(Vectors.longIdentifier)(0).id.length
      . assert(_ == 1023)

      test(m"16.7: a packed attestation of an ES256 credential is trusted from the root"):
        reference(Vectors.packedEs256, List(root))(0).attestation match
          case WebAuthn.Attestation.Certified(certificates, trusted) => (certificates.size, trusted)
          case _                                                     => (0, false)
      . assert(_ == (1, true))

      test(m"16.7: a packed attestation is reported, untrusted, when no root is given"):
        reference(Vectors.packedEs256)(0).attestation match
          case WebAuthn.Attestation.Certified(_, trusted) => trusted
          case _                                          => true
      . assert(!_)

      test(m"16.7: a packed attestation is refused against another root"):
        val other = Certificate.selfSigned
          ( Distinguished(commonName = t"Rose Root"),
            PrivateKey.generate[Ecdsa[256]](),
            period,
            BigInt(1),
            authority = true )

        refused(reference(Vectors.packedEs256, List(other)))
      . assert:
          case WebAuthn.Error.Reason.Attestation(detail) => detail.contains(t"trusted certificate")
          case _                                         => false

      test(m"16.8: a packed attestation of an ES384 credential registers and signs in"):
        reference(Vectors.packedEs384, List(root))(0).key.algorithm
      . assert(_ == -35L)

      test(m"16.9: a packed attestation of an ES512 credential registers and signs in"):
        reference(Vectors.packedEs512, List(root))(0).key.algorithm
      . assert(_ == -36L)

      test(m"16.10: a packed attestation of an RS256 credential registers and signs in"):
        reference(Vectors.packedRs256, List(root))(0).key.algorithm
      . assert(_ == -257L)

      test(m"16.11: a packed attestation of an Ed25519 credential registers and signs in"):
        reference(Vectors.packedEd25519, List(root))(0).key.algorithm
      . assert(_ == -8L)

      test(m"16.16: a FIDO U2F attestation is refused as unsupported"):
        refused(reference(Vectors.u2f))
      . assert(_ == WebAuthn.Error.Reason.Format(t"fido-u2f"))

      test(m"16.2: the reference assertion does not verify against another credential's key"):
        val (credential, _) = reference(Vectors.none)
        val (other, _) = reference(Vectors.self)
        val challenge = WebAuthn.Challenge(bytes(Vectors.none.assertedChallenge))

        refused:
          WebAuthn.authenticate
            ( assertion(Vectors.none), challenge, example, credential.copy(key = other.key) )
      . assert(_ == WebAuthn.Error.Reason.Signature)

    suite(m"Ceremonies in progress"):
      val session = Session(t"groundling-1")

      test(m"A session's challenge is kept until its answer arrives"):
        val ceremonies = Ceremonies()
        val challenge = ceremonies.begin(session)
        ceremonies.conclude(session).let(_.bytes.serialize[Base64]) == challenge.bytes.serialize[Base64]
      . assert(identity)

      test(m"A challenge is handed back only once"):
        val ceremonies = Ceremonies()
        ceremonies.begin(session)
        ceremonies.conclude(session)
        ceremonies.conclude(session)
      . assert(_ == Unset)

      test(m"A session with no ceremony in progress has no challenge"):
        Ceremonies().conclude(Session(t"groundling-2"))
      . assert(_ == Unset)

      test(m"A whole sign-in runs through the store"):
        val ceremonies = Ceremonies()
        val authenticator = SecurityKey()
        val registration = WebAuthn.registration(globe, burbage, ceremonies.begin(session))
        val created = authenticator.create(registration)
        val challenge = ceremonies.conclude(session).or(panic(m"a ceremony is in progress"))
        val credential = WebAuthn.register(created, challenge, globe)
        val options = WebAuthn.authentication(globe, ceremonies.begin(session))
        val answer = authenticator.get(options)
        val echoed = ceremonies.conclude(session).or(panic(m"a ceremony is in progress"))
        WebAuthn.authenticate(answer, echoed, globe, credential).counter
      . assert(_ == 2L)

    suite(m"Refusals"):
      test(m"A registration from another origin is refused"):
        val authenticator = SecurityKey(t"https://rose.example")
        val challenge = WebAuthn.Challenge()
        val response = authenticator.create(WebAuthn.registration(globe, burbage, challenge))
        refused(WebAuthn.register(response, challenge, globe))
      . assert(_ == WebAuthn.Error.Reason.Origin(t"https://rose.example"))

      test(m"A registration that answers another challenge is refused"):
        val authenticator = SecurityKey()
        val response = authenticator.create(WebAuthn.registration(globe, burbage, WebAuthn.Challenge()))
        refused(WebAuthn.register(response, WebAuthn.Challenge(), globe))
      . assert(_ == WebAuthn.Error.Reason.Challenge)

      test(m"A registration for another relying party is refused"):
        val authenticator = SecurityKey()
        val challenge = WebAuthn.Challenge()
        val rose = globe.copy(id = t"rose.example")
        val response = authenticator.create(WebAuthn.registration(rose, burbage, challenge))
        refused(WebAuthn.register(response, challenge, globe))
      . assert(_ == WebAuthn.Error.Reason.RelyingParty)

      test(m"A registration with an algorithm that was not asked for is refused"):
        val authenticator = SecurityKey()
        val challenge = WebAuthn.Challenge()
        val response = authenticator.create(WebAuthn.registration(globe, burbage, challenge))
        refused(WebAuthn.register(response, challenge, globe, algorithms = List(-8L)))
      . assert(_ == WebAuthn.Error.Reason.Algorithm(-7L))

      test(m"An unverified user does not satisfy a ceremony that requires verification"):
        val authenticator = SecurityKey()
        val challenge = WebAuthn.Challenge()
        val response = authenticator.create(WebAuthn.registration(globe, burbage, challenge))
        refused(WebAuthn.register(response, challenge, globe, verification = true))
      . assert(_ == WebAuthn.Error.Reason.Unverified)

      test(m"An answer made without the user present is refused"):
        val authenticator = SecurityKey()
        authenticator.presents = false
        val challenge = WebAuthn.Challenge()
        val response = authenticator.create(WebAuthn.registration(globe, burbage, challenge))
        refused(WebAuthn.register(response, challenge, globe))
      . assert(_ == WebAuthn.Error.Reason.Absent)

      test(m"An assertion offered as a registration is refused"):
        val authenticator = SecurityKey()
        val challenge = WebAuthn.Challenge()
        val assertion = authenticator.get(WebAuthn.authentication(globe, challenge))

        val response =
          t"""{"id":"${assertion(t"id").as[Text]}","type":"public-key",
               "response":{"clientDataJSON":"${assertion(t"response")(t"clientDataJSON").as[Text]}",
                           "attestationObject":"oA"}}""".read[Json]

        refused(WebAuthn.register(response, challenge, globe))
      . assert(_ == WebAuthn.Error.Reason.Ceremony(t"webauthn.get"))

      test(m"An assertion replayed against a fresh challenge is refused"):
        val authenticator = SecurityKey()
        val credential = enrol(authenticator)
        val challenge = WebAuthn.Challenge()
        val response = authenticator.get(WebAuthn.authentication(globe, challenge))
        WebAuthn.authenticate(response, challenge, globe, credential)
        refused(WebAuthn.authenticate(response, WebAuthn.Challenge(), globe, credential))
      . assert(_ == WebAuthn.Error.Reason.Challenge)

      test(m"An assertion replayed within its ceremony does not advance the counter"):
        val authenticator = SecurityKey()
        val credential = enrol(authenticator)
        val challenge = WebAuthn.Challenge()
        val response = authenticator.get(WebAuthn.authentication(globe, challenge))
        val updated = WebAuthn.authenticate(response, challenge, globe, credential)
        refused(WebAuthn.authenticate(response, challenge, globe, updated))
      . assert(_ == WebAuthn.Error.Reason.Counter)

      test(m"A cloned authenticator, whose counter has fallen behind, is refused"):
        val authenticator = SecurityKey()
        val credential = enrol(authenticator).copy(counter = 40L)
        val challenge = WebAuthn.Challenge()
        val response = authenticator.get(WebAuthn.authentication(globe, challenge))
        refused(WebAuthn.authenticate(response, challenge, globe, credential))
      . assert(_ == WebAuthn.Error.Reason.Counter)

      test(m"An assertion from another origin is refused"):
        val authenticator = SecurityKey()
        val credential = enrol(authenticator)
        authenticator.location = t"https://globe.example.evil.example"
        val challenge = WebAuthn.Challenge()
        val response = authenticator.get(WebAuthn.authentication(globe, challenge))
        refused(WebAuthn.authenticate(response, challenge, globe, credential))
      . assert(_ == WebAuthn.Error.Reason.Origin(t"https://globe.example.evil.example"))

      test(m"An assertion signed by another authenticator's key is refused"):
        val authenticator = SecurityKey()
        val impostor = SecurityKey()
        val credential = enrol(authenticator)
        val challenge = WebAuthn.Challenge()
        val response = impostor.get(WebAuthn.authentication(globe, challenge))
        refused(WebAuthn.authenticate(response, challenge, globe, credential))
      . assert(_ == WebAuthn.Error.Reason.Signature)

      test(m"An assertion from a credential other than the one named is refused"):
        val authenticator = SecurityKey()
        val credential = enrol(authenticator).copy(id = Array.fill[Byte](24)(1))
        val challenge = WebAuthn.Challenge()
        val response = authenticator.get(WebAuthn.authentication(globe, challenge))
        refused(WebAuthn.authenticate(response, challenge, globe, credential))
      . assert(_ == WebAuthn.Error.Reason.Credential)

      test(m"An assertion without verification is refused where it is required"):
        val authenticator = SecurityKey()
        val credential = enrol(authenticator)
        val challenge = WebAuthn.Challenge()
        val response = authenticator.get(WebAuthn.authentication(globe, challenge))
        refused(WebAuthn.authenticate(response, challenge, globe, credential, verification = true))
      . assert(_ == WebAuthn.Error.Reason.Unverified)

      test(m"A response that is not a credential is malformed"):
        refused(WebAuthn.register(t"""{"id":"AA"}""".read[Json], WebAuthn.Challenge(), globe))
      . assert(_ == WebAuthn.Error.Reason.Malformed(t"the registration response"))
