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

import scala.reflect.Selectable.reflectiveSelectable

import adversaria.*
import anticipation.*
import contingency.*
import corpuscular.*
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

// JSON Web Encryption (RFC 7516): content encrypted under a fresh content-encryption key,
// with that key conveyed to the recipient by one of several key-management algorithms.
// `Jwe[AesGcm[256]](payload, key)` encrypts — the type argument is the content cipher, JOSE's
// `enc`, and the key's type picks the key management, its `alg` — and `jwe.decrypt(key)`
// gives the plaintext back. The compact serialization is the `Text` codec; the JSON ones are
// `flattened`, `Jwe.general` and `Jwe.read`.
//
// A key's type stands for a *family* of key-management algorithms. For most it has one member
// (`RsaOaep[2048, Sha1]` is `RSA-OAEP`; `AesKeyWrap[128]` is `A128KW`). Where it has several —
// an `Ecdh` key agrees a key directly (`ECDH-ES`) or wraps one (`ECDH-ES+A128KW`, …), an
// `AesGcm` key is used directly (`dir`) or wraps one (`A128GCMKW`), a password wraps under any
// of the three PBES2 variants — the first is the default for encryption, and setting `alg` in
// the header passed to `Jwe` chooses another; decryption accepts any of the family, and no
// algorithm outside it.
object Jwe:
  // What key management gives the encryptor: the content-encryption key, how it is conveyed
  // (empty when the recipient derives it rather than receiving it), and the header with the
  // parameters the recipient will need.
  case class Envelope(key: Data, encryptedKey: Data, header: Jose.Header)

  private def unsupported(name: Text)(using Tactic[Jose.Error], Diagnostics): Nothing =
    abort(Jose.Error(Jose.Error.Reason.Unsupported(name)))

  private def be32(value: Int): Data =
    Array.tabulate[Byte](4)(index => (value >>> (24 - 8*index)).toByte)

  private def prefixed(data: Data): Data = Array.concat(be32(data.length), data)

  // The AES key-wrapping step several families share: `A128KW`, `A192KW` or `A256KW`, as the
  // suffix of an algorithm name, with a raw key-encryption key.
  private def wrapper(suffix: Text)(using Crypto, Tactic[Jose.Error], Diagnostics): Wrapping =
    if suffix == t"A128KW" then summon[AesKeyWrap[128]]
    else if suffix == t"A192KW" then summon[AesKeyWrap[192]]
    else if suffix == t"A256KW" then summon[AesKeyWrap[256]]
    else unsupported(suffix)

  private def wrapSize(suffix: Text): Int =
    if suffix == t"A128KW" then 16 else if suffix == t"A192KW" then 24 else 32

  object Transport:
    // A family with one algorithm, in which the content-encryption key is encrypted to the
    // recipient's public key.
    private abstract class Encrypting[cipher <: Cipher](name: Text, scheme: cipher & Encryption)
    extends Transport:
      type Self = cipher
      def algorithms: List[Text] = List(name)
      protected def decrypt(key: PrivateKey[cipher]^, wrapped: Data, size: Int): Optional[Data]

      def envelop
        ( key:        PublicKey[cipher],
          algorithm:  Text,
          header:     Jose.Header,
          encryption: Jose.Encryption )
        ( using Tactic[Jose.Error], Diagnostics )
      :   Envelope =

        val content = encryption.fresh()
        Envelope(content, scheme.encrypt(content, key.bytes, InitializationVector.zero), header)

      def open
        ( key:          PrivateKey[cipher]^,
          algorithm:    Text,
          header:       Jose.Header,
          encryptedKey: Data,
          encryption:   Jose.Encryption )
        ( using Tactic[Jose.Error], Diagnostics )
      :   Optional[Data] =

        decrypt(key, encryptedKey, encryption.size)

    // RSAES-PKCS1-v1_5: `RSA1_5`. A failure to unwrap is answered with a random key rather
    // than an error (RFC 7516 §11.5), so that what an attacker observes — the content
    // failing to authenticate — is the same whether or not the padding was valid.
    given rsa: [bits <: 2048 | 3072 | 4096: ValueOf] => (cipher: Rsa[bits], crypto: Crypto)
    =>  Rsa[bits] is Transport =
      new Encrypting[Rsa[bits]](t"RSA1_5", cipher):
        protected def decrypt(key: PrivateKey[Rsa[bits]]^, wrapped: Data, size: Int)
        :   Optional[Data] =
          safely(key.uncloak(wrapped.decrypt[Data, Rsa[bits]])).or(crypto.random.bytes(size))

    // RSAES-OAEP: `RSA-OAEP` with SHA-1, `RSA-OAEP-256` with SHA-256.
    given rsaOaep: [bits <: 2048 | 3072 | 4096: ValueOf] => (cipher: RsaOaep[bits, Sha1])
    =>  RsaOaep[bits, Sha1] is Transport =
      new Encrypting[RsaOaep[bits, Sha1]](t"RSA-OAEP", cipher):
        protected def decrypt(key: PrivateKey[RsaOaep[bits, Sha1]]^, wrapped: Data, size: Int)
        :   Optional[Data] =
          safely(key.uncloak(wrapped.decrypt[Data, RsaOaep[bits, Sha1]]))

    given rsaOaep256: [bits <: 2048 | 3072 | 4096: ValueOf]
    =>  ( cipher: RsaOaep[bits, Sha2[256]] )
    =>  RsaOaep[bits, Sha2[256]] is Transport =
      new Encrypting[RsaOaep[bits, Sha2[256]]](t"RSA-OAEP-256", cipher):
        protected def decrypt
          ( key: PrivateKey[RsaOaep[bits, Sha2[256]]]^, wrapped: Data, size: Int )
        :   Optional[Data] =
          safely(key.uncloak(wrapped.decrypt[Data, RsaOaep[bits, Sha2[256]]]))

    // ECDH-ES (RFC 7518 §4.6): an ephemeral key pair is generated for each message, its public
    // half sent in the header as `epk`, and the agreed secret run through the Concat KDF —
    // either straight into the content-encryption key (`ECDH-ES`) or into a key that wraps one
    // (`ECDH-ES+A128KW`, `+A192KW`, `+A256KW`). The agreed secret is only ever seen inside the
    // agreement's block, which zeroes it; what leaves is the derived key.
    private abstract class Agreeing[cipher <: Cipher]
      ( using crypto: Crypto, hash: Hash in Sha2[256] )
    extends Transport:
      type Self = cipher

      def algorithms: List[Text] =
        List(t"ECDH-ES", t"ECDH-ES+A128KW", t"ECDH-ES+A192KW", t"ECDH-ES+A256KW")

      // Generates an ephemeral key and lends its public half, as a JWK, and the secret agreed
      // with the peer.
      protected def sending[result](peer: PublicKey[cipher])(block: (Jwk, Data) => result): result

      // Lends the secret agreed between the recipient's key and the sender's ephemeral one.
      protected def receiving[result](key: PrivateKey[cipher]^, ephemeral: Jwk)
        ( block: Data => result )
        ( using Tactic[Jose.Error], Diagnostics )
      :   result

      // `AlgorithmID ‖ PartyUInfo ‖ PartyVInfo ‖ SuppPubInfo` (RFC 7518 §4.6.2), each
      // variable-length field prefixed with its length, and the key length in bits.
      private def derive
        ( secret: Data, algorithm: Text, header: Jose.Header, encryption: Jose.Encryption )
        ( using Tactic[Jose.Error], Diagnostics )
      :   Data =

        val direct = algorithm == t"ECDH-ES"
        val size = if direct then encryption.size else wrapSize(algorithm.skip(8))
        val identifier = Jose.ascii(if direct then encryption.name else algorithm)
        val producer = header.producer.lay(Data())(Jose.decode(_))
        val recipient = header.recipient.lay(Data())(Jose.decode(_))

        val parties: Data =
          Array.concat(prefixed(identifier), prefixed(producer), prefixed(recipient))

        KeyDerivation.concat[Sha2[256]](secret, Array.concat(parties, be32(size*8)), size)

      def envelop
        ( key:        PublicKey[cipher],
          algorithm:  Text,
          header:     Jose.Header,
          encryption: Jose.Encryption )
        ( using Tactic[Jose.Error], Diagnostics )
      :   Envelope =

        sending(key): (ephemeral, secret) =>
          val derived = derive(secret, algorithm, header, encryption)
          val updated = header.copy(ephemeralKey = ephemeral)

          if algorithm == t"ECDH-ES" then Envelope(derived, Data(), updated) else
            val content = encryption.fresh()
            Envelope(content, wrapper(algorithm.skip(8)).wrap(derived, content), updated)

      def open
        ( key:          PrivateKey[cipher]^,
          algorithm:    Text,
          header:       Jose.Header,
          encryptedKey: Data,
          encryption:   Jose.Encryption )
        ( using Tactic[Jose.Error], Diagnostics )
      :   Optional[Data] =

        val ephemeral = header.ephemeralKey.or:
          abort(Jose.Error(Jose.Error.Reason.Malformed(t"the header has no ephemeral key")))

        receiving(key, ephemeral): secret =>
          val derived = derive(secret, algorithm, header, encryption)

          if algorithm == t"ECDH-ES" then derived
          else wrapper(algorithm.skip(8)).unwrap(derived, encryptedKey)

    // The ephemeral key is held on the heap for the one agreement it exists for, whatever
    // cloak the caller's own keys use: it is generated, used and dropped inside one call.
    given ecdh: [bits <: 256 | 384 | 521: ValueOf]
    =>  ( cipher: Ecdh[bits], crypto: Crypto, hash: Hash in Sha2[256] )
    =>  Ecdh[bits] is Transport =

      new Agreeing[Ecdh[bits]]:
        protected def sending[result](peer: PublicKey[Ecdh[bits]])
          ( block: (Jwk, Data) => result )
        :   result =

          import cloaks.heapCloak
          val ephemeral = PrivateKey.generate[Ecdh[bits]]()
          val published = Jwk(ephemeral.public)
          ephemeral.agree(peer)(block(published, _))

        protected def receiving[result](key: PrivateKey[Ecdh[bits]]^, ephemeral: Jwk)
          ( block: Data => result )
          ( using Tactic[Jose.Error], Diagnostics )
        :   result =

          key.agree(ephemeral.publicKey[Ecdh[bits]])(block)

    given x25519: (cipher: X25519, crypto: Crypto, hash: Hash in Sha2[256]) => X25519 is Transport =
      new Agreeing[X25519]:
        protected def sending[result](peer: PublicKey[X25519])(block: (Jwk, Data) => result)
        :   result =

          import cloaks.heapCloak
          val ephemeral = PrivateKey.generate[X25519]()
          val published = Jwk(ephemeral.public)
          ephemeral.agree(peer)(block(published, _))

        protected def receiving[result](key: PrivateKey[X25519]^, ephemeral: Jwk)
          ( block: Data => result )
          ( using Tactic[Jose.Error], Diagnostics )
        :   result =

          key.agree(ephemeral.publicKey[X25519])(block)

  // Key management with an asymmetric key (RFC 7518 §4.2, §4.3, §4.6): the sender uses the
  // recipient's public key, the recipient the private one. `open` is `Unset` when the key does
  // not yield a content-encryption key.
  trait Transport extends Typeclass:
    type Self <: Cipher
    def algorithms: List[Text]

    def envelop
      ( key: PublicKey[Self], algorithm: Text, header: Jose.Header, encryption: Jose.Encryption )
      ( using Tactic[Jose.Error], Diagnostics )
    :   Envelope

    def open
      ( key:          PrivateKey[Self]^,
        algorithm:    Text,
        header:       Jose.Header,
        encryptedKey: Data,
        encryption:   Jose.Encryption )
      ( using Tactic[Jose.Error], Diagnostics )
    :   Optional[Data]

  object Sharing:
    // AES Key Wrap: `A128KW`, `A192KW` and `A256KW`.
    given aesKeyWrap: [bits <: 128 | 192 | 256: ValueOf] => (cipher: AesKeyWrap[bits])
    =>  AesKeyWrap[bits] is Sharing =

      new Sharing:
        type Self = AesKeyWrap[bits]
        def algorithms: List[Text] = List(t"A${valueOf[bits]}KW")

        def envelop
          ( key:        SymmetricKey[AesKeyWrap[bits]]^,
            algorithm:  Text,
            header:     Jose.Header,
            encryption: Jose.Encryption )
          ( using Tactic[Jose.Error], Diagnostics )
        :   Envelope =

          val content = encryption.fresh()
          Envelope(content, key.wrap(content), header)

        def open
          ( key:          SymmetricKey[AesKeyWrap[bits]]^,
            algorithm:    Text,
            header:       Jose.Header,
            encryptedKey: Data,
            encryption:   Jose.Encryption )
          ( using Tactic[Jose.Error], Diagnostics )
        :   Optional[Data] =

          safely(key.unwrap(encryptedKey))

    // An AES-GCM key is the content-encryption key itself (`dir`, and then the content cipher
    // must be the key's own), or wraps one with AES-GCM (`A128GCMKW`, `A192GCMKW`, `A256GCMKW`),
    // the nonce and tag of the wrapping travelling in the header as `iv` and `tag`.
    given aesGcm: [bits <: 128 | 192 | 256: ValueOf] => (cipher: AesGcm[bits], crypto: Crypto)
    =>  AesGcm[bits] is Sharing =

      new Sharing:
        type Self = AesGcm[bits]
        private val direct: Text = t"A${valueOf[bits]}GCM"
        def algorithms: List[Text] = List(t"dir", t"A${valueOf[bits]}GCMKW")

        def envelop
          ( key:        SymmetricKey[AesGcm[bits]]^,
            algorithm:  Text,
            header:     Jose.Header,
            encryption: Jose.Encryption )
          ( using Tactic[Jose.Error], Diagnostics )
        :   Envelope =

          if algorithm == t"dir" then
            if encryption.name != direct
            then abort(Jose.Error(Jose.Error.Reason.Mismatch(direct, encryption.name)))

            Envelope(key.data(Divulgence), Data(), header)
          else
            val content = encryption.fresh()
            val nonce = crypto.random.bytes(12)
            val wrapped = key.seal(nonce, Data(), content)
            val split = wrapped.length - 16

            Envelope
              ( content,
                wrapped.keep(split),
                header.copy(vector = Jose.encode(nonce), tag = Jose.encode(wrapped.skip(split))) )

        def open
          ( key:          SymmetricKey[AesGcm[bits]]^,
            algorithm:    Text,
            header:       Jose.Header,
            encryptedKey: Data,
            encryption:   Jose.Encryption )
          ( using Tactic[Jose.Error], Diagnostics )
        :   Optional[Data] =

          if algorithm == t"dir" then
            if encryption.name != direct
            then abort(Jose.Error(Jose.Error.Reason.Mismatch(direct, encryption.name)))

            key.data(Divulgence)
          else
            val nonce = Jose.decode:
              header.vector.or:
                abort(Jose.Error(Jose.Error.Reason.Malformed(t"the header has no iv parameter")))

            val tag = Jose.decode:
              header.tag.or:
                abort(Jose.Error(Jose.Error.Reason.Malformed(t"the header has no tag parameter")))

            safely(key.unseal(nonce, Data(), Array.concat(encryptedKey, tag)))

    // An AES-CBC-HMAC key can only be the content-encryption key itself: `dir`.
    given aesCbcHmac: [bits <: 256 | 384 | 512: ValueOf] => (cipher: AesCbcHmac[bits])
    =>  AesCbcHmac[bits] is Sharing =

      new Sharing:
        type Self = AesCbcHmac[bits]
        private val direct: Text = t"A${valueOf[bits]/2}CBC-HS${valueOf[bits]}"
        def algorithms: List[Text] = List(t"dir")

        private def content(key: SymmetricKey[AesCbcHmac[bits]]^, encryption: Jose.Encryption)
          ( using Tactic[Jose.Error], Diagnostics )
        :   Data =

          if encryption.name != direct
          then abort(Jose.Error(Jose.Error.Reason.Mismatch(direct, encryption.name)))

          key.data(Divulgence)

        def envelop
          ( key:        SymmetricKey[AesCbcHmac[bits]]^,
            algorithm:  Text,
            header:     Jose.Header,
            encryption: Jose.Encryption )
          ( using Tactic[Jose.Error], Diagnostics )
        :   Envelope =

          Envelope(content(key, encryption), Data(), header)

        def open
          ( key:          SymmetricKey[AesCbcHmac[bits]]^,
            algorithm:    Text,
            header:       Jose.Header,
            encryptedKey: Data,
            encryption:   Jose.Encryption )
          ( using Tactic[Jose.Error], Diagnostics )
        :   Optional[Data] =

          content(key, encryption)

  // Key management with a key both parties hold (RFC 7518 §4.4, §4.5, §4.7).
  trait Sharing extends Typeclass:
    type Self <: Cipher
    def algorithms: List[Text]

    def envelop
      ( key:        SymmetricKey[Self]^,
        algorithm:  Text,
        header:     Jose.Header,
        encryption: Jose.Encryption )
      ( using Tactic[Jose.Error], Diagnostics )
    :   Envelope

    def open
      ( key:          SymmetricKey[Self]^,
        algorithm:    Text,
        header:       Jose.Header,
        encryptedKey: Data,
        encryption:   Jose.Encryption )
      ( using Tactic[Jose.Error], Diagnostics )
    :   Optional[Data]

  // PBES2 (RFC 7518 §4.8): a key derived from a password with PBKDF2, wrapping the
  // content-encryption key. The salt and iteration count travel in the header as `p2s` and
  // `p2c`. An iteration count above `MaximumIterations` is refused when decrypting, since the
  // header is the sender's to write and each iteration is the recipient's to pay for.
  private val Passwords: List[Text] =
    List(t"PBES2-HS256+A128KW", t"PBES2-HS384+A192KW", t"PBES2-HS512+A256KW")

  val DefaultIterations: Int = 600000
  val MaximumIterations: Int = 10000000

  private def passwordKey(password: Password^, algorithm: Text, salt: Data, iterations: Int)
    ( using Crypto, Hash in Sha2[256], Hash in Sha2[384], Hash in Sha2[512] )
  :   Data =

    // RFC 7518 §4.8.1.1: the salt PBKDF2 sees is the algorithm name, a zero octet and `p2s`.
    val separator: Data = Array.fill[Byte](1)(0)
    val input: Data = Array.concat(Jose.ascii(algorithm), separator, salt)

    if algorithm == t"PBES2-HS256+A128KW"
    then KeyDerivation.pbkdf2[Sha2[256]](password, input, iterations, 16)
    else if algorithm == t"PBES2-HS384+A192KW"
    then KeyDerivation.pbkdf2[Sha2[384]](password, input, iterations, 24)
    else KeyDerivation.pbkdf2[Sha2[512]](password, input, iterations, 32)

  private def choose(family: List[Text], header: Jose.Header)
    ( using Tactic[Jose.Error], Diagnostics )
  :   Text =

    val default = family.head.or(panic(m"every family has an algorithm"))

    header.algorithm.lay(default): chosen =>
      if family.has(chosen) then chosen
      else abort(Jose.Error(Jose.Error.Reason.Mismatch(default, chosen)))

  private def assemble
    ( plaintext: Data, envelope: Envelope, algorithm: Text, encryption: Jose.Encryption )
    ( using crypto: Crypto )
  :   Jwe =

    val header = envelope.header.copy(algorithm = algorithm, encryption = encryption.name)
    val protection = header.encoded
    val vector = crypto.random.bytes(encryption.cipher.nonceSize)
    val box = encryption.cipher.seal(envelope.key, vector, Jose.ascii(protection), plaintext)
    val split = box.length - encryption.cipher.tagSize

    Jwe
      ( protection,
        Unset,
        Unset,
        envelope.encryptedKey,
        vector,
        box.keep(split),
        box.skip(split),
        Unset )

  // The first step of `Jwe[content](…)`: the content cipher is named, and what follows takes
  // the payload and the recipient's key.
  inline def apply[content <: Cipher]: Sealing[content] = new Sealing[content]()

  final class Sealing[content <: Cipher]():
    // Encrypts to a public key.
    def apply[encodable: Encodable in Data, cipher <: Cipher]
      ( payload: encodable, key: PublicKey[cipher], header: Jose.Header )
      ( using management: cipher is Transport,
              encryption: content is Jose.Encryption,
              crypto:     Crypto,
              tactic:     Tactic[Jose.Error],
              diagnostic: Diagnostics )
    :   Jwe =

      val algorithm = choose(management.algorithms, header)
      val envelope = management.envelop(key, algorithm, header, encryption)
      assemble(encodable.encode(payload), envelope, algorithm, encryption)

    def apply[encodable: Encodable in Data, cipher <: Cipher]
      ( payload: encodable, key: PublicKey[cipher] )
      ( using management: cipher is Transport,
              encryption: content is Jose.Encryption,
              crypto:     Crypto,
              tactic:     Tactic[Jose.Error],
              diagnostic: Diagnostics )
    :   Jwe =

      apply(payload, key, Jose.Header())

    // Encrypts under a shared key.
    def apply[encodable: Encodable in Data, cipher <: Cipher]
      ( payload: encodable, key: SymmetricKey[cipher]^, header: Jose.Header )
      ( using management: cipher is Sharing,
              encryption: content is Jose.Encryption,
              crypto:     Crypto,
              tactic:     Tactic[Jose.Error],
              diagnostic: Diagnostics )
    :   Jwe =

      val algorithm = choose(management.algorithms, header)
      val envelope = management.envelop(key, algorithm, header, encryption)
      assemble(encodable.encode(payload), envelope, algorithm, encryption)

    def apply[encodable: Encodable in Data, cipher <: Cipher]
      ( payload: encodable, key: SymmetricKey[cipher]^ )
      ( using management: cipher is Sharing,
              encryption: content is Jose.Encryption,
              crypto:     Crypto,
              tactic:     Tactic[Jose.Error],
              diagnostic: Diagnostics )
    :   Jwe =

      apply(payload, key, Jose.Header())

    // Encrypts under a password, with a fresh salt and — unless the header's `p2c` says
    // otherwise — `DefaultIterations` iterations.
    def apply[encodable: Encodable in Data]
      ( payload: encodable, password: Password^, header: Jose.Header )
      ( using encryption: content is Jose.Encryption,
              crypto:     Crypto,
              sha256:     Hash in Sha2[256],
              sha384:     Hash in Sha2[384],
              sha512:     Hash in Sha2[512],
              tactic:     Tactic[Jose.Error],
              diagnostic: Diagnostics )
    :   Jwe =

      val algorithm = choose(Passwords, header)
      val salt = crypto.random.bytes(16)
      val iterations = header.iterations.or(DefaultIterations)
      val key = passwordKey(password, algorithm, salt, iterations)
      val content = encryption.fresh()
      val wrapped = wrapper(algorithm.skip(12)).wrap(key, content)
      val updated = header.copy(salt = Jose.encode(salt), iterations = iterations)
      val envelope = Envelope(content, wrapped, updated)
      assemble(encodable.encode(payload), envelope, algorithm, encryption)

    def apply[encodable: Encodable in Data](payload: encodable, password: Password^)
      ( using encryption: content is Jose.Encryption,
              crypto:     Crypto,
              sha256:     Hash in Sha2[256],
              sha384:     Hash in Sha2[384],
              sha512:     Hash in Sha2[512],
              tactic:     Tactic[Jose.Error],
              diagnostic: Diagnostics )
    :   Jwe =

      apply(payload, password, Jose.Header())

  // The first step of `jwe.decrypt[content](key)`.
  final class Opening[content <: Cipher](jwe: Jwe):
    // Decrypts with a private key.
    def apply[cipher <: Cipher](key: PrivateKey[cipher]^)
      ( using management: cipher is Transport,
              encryption: content is Jose.Encryption,
              tactic:     Tactic[Jose.Error],
              diagnostic: Diagnostics )
    :   Data =

      val header = jwe.header
      val algorithm = jwe.vet(header, management.algorithms, encryption)
      val content = management.open(key, algorithm, header, jwe.encryptedKey, encryption)
      jwe.unsealed(content, encryption)

    // Decrypts with a shared key.
    def apply[cipher <: Cipher](key: SymmetricKey[cipher]^)
      ( using management: cipher is Sharing,
              encryption: content is Jose.Encryption,
              tactic:     Tactic[Jose.Error],
              diagnostic: Diagnostics )
    :   Data =

      val header = jwe.header
      val algorithm = jwe.vet(header, management.algorithms, encryption)
      val content = management.open(key, algorithm, header, jwe.encryptedKey, encryption)
      jwe.unsealed(content, encryption)

    // Decrypts with a password.
    def apply(password: Password^)
      ( using encryption: content is Jose.Encryption,
              crypto:     Crypto,
              sha256:     Hash in Sha2[256],
              sha384:     Hash in Sha2[384],
              sha512:     Hash in Sha2[512],
              tactic:     Tactic[Jose.Error],
              diagnostic: Diagnostics )
    :   Data =

      val header = jwe.header
      val algorithm = jwe.vet(header, Passwords, encryption)

      val salt = Jose.decode:
        header.salt.or:
          abort(Jose.Error(Jose.Error.Reason.Malformed(t"the header has no p2s parameter")))

      val iterations = header.iterations.or:
        abort(Jose.Error(Jose.Error.Reason.Malformed(t"the header has no p2c parameter")))

      if iterations < 1 || iterations > MaximumIterations
      then unsupported(t"PBES2 with $iterations iterations")

      val key = passwordKey(password, algorithm, salt, iterations)
      jwe.unsealed(wrapper(algorithm.skip(12)).unwrap(key, jwe.encryptedKey), encryption)

  // The compact serialization (RFC 7516 §7.1): the protected header, encrypted key,
  // initialization vector, ciphertext and tag, each base64url, joined with periods.
  given encodable: Jwe is Encodable in Text = jwe =>
    val key = Jose.encode(jwe.encryptedKey)
    val vector = Jose.encode(jwe.vector)
    val ciphertext = Jose.encode(jwe.ciphertext)
    t"${jwe.protection.or(t"")}.$key.$vector.$ciphertext.${Jose.encode(jwe.tag)}"

  given decodable: (tactic: Tactic[Jose.Error], diagnostics: Diagnostics)
  =>  ( (Jwe is Decodable in Text)^{tactic} ) =
    text => text.cut(t".") match
      case List(protection, encryptedKey, vector, ciphertext, tag) =>
        Jwe
          ( protection,
            Unset,
            Unset,
            Jose.decode(encryptedKey),
            Jose.decode(vector),
            Jose.decode(ciphertext),
            Jose.decode(tag),
            Unset )

      case _ =>
        abort(Jose.Error(Jose.Error.Reason.Malformed(t"a compact JWE has five parts")))

  // The members of the JSON serializations (RFC 7516 §7.2).
  private case class Recipient
    ( header:                                    Optional[Jose.Header],
      @name[Json](t"encrypted_key") encryptedKey: Optional[Text] )

  private case class Flattened
    ( @name[Json](t"protected")     protection:   Optional[Text],
      unprotected:                               Optional[Jose.Header],
      header:                                    Optional[Jose.Header],
      @name[Json](t"encrypted_key") encryptedKey: Optional[Text],
      @name[Json](t"iv")            vector:       Optional[Text],
      ciphertext:                                Text,
      tag:                                       Optional[Text],
      @name[Json](t"aad")           associated:   Optional[Text] )

  private case class General
    ( @name[Json](t"protected") protection: Optional[Text],
      unprotected:                         Optional[Jose.Header],
      recipients:                          List[Recipient],
      @name[Json](t"iv")        vector:     Optional[Text],
      ciphertext:                          Text,
      tag:                                 Optional[Text],
      @name[Json](t"aad")       associated: Optional[Text] )

  // The general JSON serialization of one message for several recipients. Every `Jwe` must
  // be the same message — one content-encryption key, conveyed to each recipient in turn —
  // which `Jwe.read` gives and nothing here constructs: this is for writing back what was read.
  def general(recipients: List[Jwe])(using Tactic[Jose.Error], Diagnostics): Json =
    val first = recipients.head.or:
      abort(Jose.Error(Jose.Error.Reason.Malformed(t"a JWE has at least one recipient")))

    General
      ( first.protection,
        first.unprotected,
        recipients.map: jwe =>
          Recipient(jwe.recipient, Jose.encode(jwe.encryptedKey)),
        Jose.encode(first.vector),
        Jose.encode(first.ciphertext),
        Jose.encode(first.tag),
        first.associated )
    . in[Json]

  // Reads either JSON serialization — flattened, for one recipient, or general, for
  // several — as one `Jwe` for each recipient; the content is shared between them.
  def read(json: Json)(using Tactic[Jose.Error], Diagnostics): List[Jwe] =
    def binary(text: Optional[Text]): Data = text.lay(Data())(Jose.decode(_))

    mitigate:
      case Json.Error(_) => Jose.Error(Jose.Error.Reason.Malformed(t"it is not a JWE in JSON"))

    . protect:
        if safely(json(t"recipients").as[List[Json]]).present then
          val general = json.as[General]

          general.recipients.map: recipient =>
            Jwe
              ( general.protection,
                general.unprotected,
                recipient.header,
                binary(recipient.encryptedKey),
                binary(general.vector),
                Jose.decode(general.ciphertext),
                binary(general.tag),
                general.associated )
        else
          val flattened = json.as[Flattened]

          List:
            Jwe
              ( flattened.protection,
                flattened.unprotected,
                flattened.header,
                binary(flattened.encryptedKey),
                binary(flattened.vector),
                Jose.decode(flattened.ciphertext),
                binary(flattened.tag),
                flattened.associated )

case class Jwe
  ( protection:   Optional[Text],
    unprotected:  Optional[Jose.Header],
    recipient:    Optional[Jose.Header],
    encryptedKey: Data,
    vector:       Data,
    ciphertext:   Data,
    tag:          Data,
    associated:   Optional[Text] ):

  // The header the object's parameters are read from: the protected header, then the shared
  // unprotected one, then the recipient's own (RFC 7516 §7.2.1 forbids a name in two of them,
  // so the order only matters for an object that breaks that rule). Only the first of the
  // three is authenticated.
  def header(using Tactic[Jose.Error], Diagnostics): Jose.Header =
    val integrity = protection.lay(Jose.Header())(Jose.Header.read(_))
    integrity.vet()
    integrity.merge(unprotected.or(Jose.Header())).merge(recipient.or(Jose.Header()))

  // The flattened JSON serialization (RFC 7516 §7.2.2).
  def flattened: Json =
    Jwe.Flattened
      ( protection,
        unprotected,
        recipient,
        Jose.encode(encryptedKey),
        Jose.encode(vector),
        Jose.encode(ciphertext),
        Jose.encode(tag),
        associated )
    . in[Json]

  // Decrypts the object: `jwe.decrypt[AesGcm[256]](key)`. The type argument is the content
  // cipher, which the header's `enc` must name; the key's type is the key management, which
  // its `alg` must be one of.
  def decrypt[content <: Cipher]: Jwe.Opening[content] = Jwe.Opening[content](this)

  // The additional authenticated data: the protected header as it was sent and, in the JSON
  // serializations, the `aad` member after a period (RFC 7516 §5.1 step 14).
  private def authenticated: Data =
    Jose.ascii(associated.lay(protection.or(t""))(aad => t"${protection.or(t"")}.$aad"))

  private[sigillate] def vet
    ( parameters: Jose.Header, family: List[Text], encryption: Jose.Encryption )
    ( using Tactic[Jose.Error], Diagnostics )
  :   Text =

    val expected = family.head.or(panic(m"every family has an algorithm"))

    val algorithm = parameters.algorithm.or:
      abort(Jose.Error(Jose.Error.Reason.Malformed(t"the header names no algorithm")))

    if !family.has(algorithm)
    then abort(Jose.Error(Jose.Error.Reason.Mismatch(expected, algorithm)))

    val found = parameters.encryption.or:
      abort(Jose.Error(Jose.Error.Reason.Malformed(t"the header names no content encryption")))

    if found != encryption.name
    then abort(Jose.Error(Jose.Error.Reason.Mismatch(encryption.name, found)))

    parameters.compression.let: compression =>
      abort(Jose.Error(Jose.Error.Reason.Unsupported(t"$compression compression")))

    algorithm

  // One failure for every cause — a key that did not unwrap, a key of the wrong length, a tag
  // that did not verify — so that nothing distinguishes them to whoever sent the object.
  private[sigillate] def unsealed(key: Optional[Data], encryption: Jose.Encryption)
    ( using Tactic[Jose.Error], Diagnostics )
  :   Data =

    val plaintext: Optional[Data] = key.let: key =>
      if key.length != encryption.size then Unset
      else encryption.cipher.unseal(key, vector, authenticated, Array.concat(ciphertext, tag))

    plaintext.or(abort(Jose.Error(Jose.Error.Reason.Undecryptable)))
