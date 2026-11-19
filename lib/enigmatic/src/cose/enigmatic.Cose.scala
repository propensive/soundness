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
package enigmatic

import scala.reflect.Selectable.reflectiveSelectable

import anticipation.*
import breviloquence.*
import contingency.*
import distillate.*
import enigmatic.*
import fulminate.*
import gossamer.*
import gastronomy.*
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

object Cose:
  // Serialises a COSE message to its CBOR-tagged wire form, as `cose.in[Data]`. Bounded by
  // `Cose` so that the refined `Cose in structure by cipher` a constructor returns resolves it.
  given encodable: [cose <: Cose] => cose is Encodable in Data = cose =>
    import cose.*
    val unprotectedAst: Cbor.Ast = Cose.unsealOrEmpty(unprotectedHeader)

    val envelope = cborTag match
      case Cose.Tag.Sign1 | Cose.Tag.Mac0 =>
        // Sign1/Mac0 messages carry exactly one recipient by construction; the empty
        // fallback is unreachable.
        val auth = recipients.head.let(_.authentication).or(Array.empty[Byte])
        Cbor.Ast.array(Array[Any](protectedHeader, unprotectedAst, payload, auth))

      case _ =>
        val recipList: List[Any] = recipients.map[Any]: r =>
          Cbor.Ast.array(Array[Any](r.protectedHeader, Cose.unsealOrEmpty(r.unprotectedHeader),
            r.authentication))

        val recipAst: Array[Any]^{} = recipList.to[Array]

        Cbor.Ast.array(Array[Any](protectedHeader, unprotectedAst, payload,
          Cbor.Ast.array(recipAst)))

    // Named, not `.encode`: this given's own `encode` extension is in scope here.
    Cbor.Ast.encodable.encoded(Cbor.Ast(Cbor.Tag(cborTag, envelope)))

  private def emptyMapAst: Cbor.Ast =
    Cbor.Ast.map(Array.empty[Any], Array.empty[Any])

  private[enigmatic] def emptyMap: Cbor = Cbor.ast(emptyMapAst)

  private[enigmatic] def unsealOrEmpty(cbor: Cbor): Cbor.Ast =
    val ast = Cbor.unseal(cbor)
    if ast.isMap then ast else emptyMapAst

  private[enigmatic] def toBeSigned
    ( context: String, bodyProtected: Data, externalAad: Data, payload: Data )
  :   Data =

    val sigStruct =
      Cbor.Ast.array(Array[Any](context, bodyProtected, externalAad, payload))

    CanonicalCbor.encode(sigStruct)

  // The value an integer label has in a CBOR map, as COSE's headers and keys use them.
  private[enigmatic] def labelled(map: Cbor.Ast, label: Long): Optional[Cbor.Ast] =
    def recur(index: Int): Optional[Cbor.Ast] =
      if index >= map.entries then Unset
      else if safely(map.key(index).long) == label then map.value(index)
      else recur(index + 1)

    if map.isMap then recur(0) else Unset

  // The `alg` (label 1) of a serialized protected header, where it has one.
  private[enigmatic] def algorithm(protectedHeader: Data): Optional[Long] =
    if protectedHeader.length == 0 then Unset else
      safely(Cbor.Ast.parse(protectedHeader)).let: header =>
        labelled(header, 1L).let(value => safely(value.long))

  private[enigmatic] def readByteString(ast: Cbor.Ast)(using Tactic[Cose.Error]): Data =
    if ast.isByteString then ast.asInstanceOf[Data]
    else abort(Cose.Error(Cose.Error.Reason.MalformedStructure))

  // Internal constructor that refines the phantom `Form` and `Operand` types.
  def make[scheme <: Cose.Structure, cipher <: Cipher]
    ( protectedHeader:   Data,
     unprotectedHeader: Cbor,
     payload:           Data,
     contextString:     String,
     cborTag:           Long,
     recipients:        List[Cose.Recipient] )
  :   Cose in scheme by cipher =

    new Cose(protectedHeader, unprotectedHeader, payload, contextString, cborTag, recipients):
      type Form    = scheme
      type Operand = cipher


  // User-facing constructor. The key's type selects the variant via
  // `Cose.Authenticator` (asymmetric -> Sign1, symmetric -> Mac0). Inlined so
  // `source.read[Data]` expands at the call site, picking up the caller's
  // readability and tactic instances.
  inline def apply[source, key]
    ( source: source, key: key )
    ( using auth: key is Cose.Authenticator,
            cborTactic: Tactic[Cbor.Error],
            readable: source is Readable to Data )
  :   Cose in auth.Form by auth.Operand raises Cose.Error =

    val payload: Data  = source.read[Data]
    val algId          = auth.algId
    val protectedAst   = Cbor.Ast.map(Array[Any](1L), Array[Any](algId))
    val protectedBstr  = CanonicalCbor.encode(protectedAst)
    val externalAad    = Array.empty[Byte]
    val tbs            = Cose.toBeSigned(auth.contextString, protectedBstr, externalAad, payload)
    val authentication = auth.authenticate(tbs, key)
    val recipient      = Cose.Recipient(Array.empty[Byte], Cose.emptyMap, authentication)

    Cose.make[auth.Form, auth.Operand]
      ( protectedBstr,
       Cose.emptyMap,
       payload,
       auth.contextString,
       auth.cborTag,
       List(recipient) )


  // Parse a tagged COSE envelope. The variant is determined from the CBOR
  // tag; the returned phantom types are the most-general bounds.
  // A real `using` clause rather than the `raises` sugar: a context-function result would
  // hide the `cborTactic` parameter, which the separation checker rejects.
  def parse(bytes: Data)
    ( using cborTactic: Tactic[Cbor.Error], coseTactic: Tactic[Cose.Error] )
  :   Cose =

    val ast = Cbor.Ast.parse(bytes)

    if !ast.isTag then abort(Cose.Error(Cose.Error.Reason.MalformedStructure))

    val tag       = ast.asInstanceOf[Cbor.Tag]
    val tagNumber = tag.tag

    val contextString = tagNumber match
      case Cose.Tag.Sign1 => Cose.Context.Signature1
      case Cose.Tag.Mac0  => Cose.Context.Mac0
      case Cose.Tag.Sign  => Cose.Context.Signature
      case Cose.Tag.Mac   => Cose.Context.Mac
      case other          => abort(Cose.Error(Cose.Error.Reason.UnknownTag(other)))

    val body = tag.value.asInstanceOf[Cbor.Ast]

    if !body.isArray || body.elements != 4 then
      abort(Cose.Error(Cose.Error.Reason.MalformedStructure))

    val protectedHeader = readByteString(body.element(0))
    val unprotectedAst  = body.element(1)
    if !unprotectedAst.isMap then abort(Cose.Error(Cose.Error.Reason.MalformedStructure))

    val payload = readByteString(body.element(2))

    val recipients: List[Cose.Recipient] = tagNumber match
      case Cose.Tag.Sign1 | Cose.Tag.Mac0 =>
        List(Cose.Recipient(Array.empty[Byte], emptyMap, readByteString(body.element(3))))

      case _ =>
        val recipArray = body.element(3)
        if !recipArray.isArray then abort(Cose.Error(Cose.Error.Reason.MalformedStructure))
        val builder = scala.collection.immutable.List.newBuilder[Cose.Recipient]
        var index = 0

        while index < recipArray.elements do
          val entry = recipArray.element(index)

          if !entry.isArray || entry.elements != 3 then
            abort(Cose.Error(Cose.Error.Reason.MalformedStructure))

          val rp = readByteString(entry.element(0))
          val ru = entry.element(1)
          if !ru.isMap then abort(Cose.Error(Cose.Error.Reason.MalformedStructure))
          val ra = readByteString(entry.element(2))
          builder += Cose.Recipient(rp, Cbor.ast(ru), ra)
          index += 1

        builder.result().to(List)

    new Cose
      ( protectedHeader, Cbor.ast(unprotectedAst), payload, contextString, tagNumber, recipients ):
      type Form    = Cose.Structure
      type Operand = Cipher

  // CoseAlgorithm → Cose.Algorithm
  // Maps an enigmatic.Cipher to its COSE Algorithms registry identifier
  // (RFC 9053, https://www.iana.org/assignments/cose/cose.xhtml#algorithms).
  object Algorithm:
    // HMAC 256/256 = 5, HMAC 384/384 = 6, HMAC 512/512 = 7
    given hmacSha256: HmacCipher[Sha2[256]] is Cose.Algorithm = new Cose.Algorithm:
      type Self = HmacCipher[Sha2[256]]
      def algId: Long = 5L

    given hmacSha384: HmacCipher[Sha2[384]] is Cose.Algorithm = new Cose.Algorithm:
      type Self = HmacCipher[Sha2[384]]
      def algId: Long = 6L

    given hmacSha512: HmacCipher[Sha2[512]] is Cose.Algorithm = new Cose.Algorithm:
      type Self = HmacCipher[Sha2[512]]
      def algId: Long = 7L

    private type EcdsaCrypto = Crypto { def ecdsa(digest: Text): Crypto.SignatureScheme }

    private def ecdsa[bits <: 256 | 384 | 521: ValueOf]
      ( identifier: Long, digest: Text )
      ( using crypto: EcdsaCrypto )
    :   Ecdsa[bits] is Cose.Algorithm =

      val cipher = Ecdsa.value[bits](using summon, Signature.Digest(digest), crypto)

      new Cose.Algorithm:
        type Self = Ecdsa[bits]
        def algId: Long = identifier
        override def scheme: Optional[Ecdsa[bits] & Signing] = cipher

        override def wire(signature: Data): Optional[Data] =
          Ecdsa.concatenation[bits](Signature[Ecdsa[bits]](signature))

        override def native(wire: Data): Optional[Data] =
          Ecdsa.signature[bits](wire).let(_.bytes)

    // ES256 = -7, ES384 = -35, ES512 = -36: ECDSA with the SHA-2 hash of each name, over the
    // curve conventionally paired with it.
    given es256: (crypto: EcdsaCrypto) => Ecdsa[256] is Cose.Algorithm = ecdsa[256](-7L, t"SHA256")
    given es384: (crypto: EcdsaCrypto) => Ecdsa[384] is Cose.Algorithm = ecdsa[384](-35L, t"SHA384")
    given es512: (crypto: EcdsaCrypto) => Ecdsa[521] is Cose.Algorithm = ecdsa[521](-36L, t"SHA512")

    // EdDSA = -8
    given edDsa: Ed25519 is Cose.Algorithm = new Cose.Algorithm:
      type Self = Ed25519
      def algId: Long = -8L

    // RS256 = -257: RSASSA-PKCS1-v1_5 with SHA-256, whatever `Signature.Digest` is in scope.
    given rs256: [bits <: 2048 | 3072 | 4096: ValueOf] => (crypto: Crypto)
    =>  Rsa[bits] is Cose.Algorithm =

      val cipher = Rsa.value[bits](using summon, Signature.Digest(t"SHA256"), crypto)

      new Cose.Algorithm:
        type Self = Rsa[bits]
        def algId: Long = -257L
        override def scheme: Optional[Rsa[bits] & Signing] = cipher

    // PS256 = -37: RSASSA-PSS with SHA-256.
    given ps256: [bits <: 2048 | 3072 | 4096: ValueOf]
    =>  ( crypto: Crypto { def rsaPss(digest: Text): Crypto.SignatureScheme } )
    =>  RsaPss[bits] is Cose.Algorithm =

      val cipher = RsaPss.value[bits](using summon, Signature.Digest(t"SHA256"), crypto)

      new Cose.Algorithm:
        type Self = RsaPss[bits]
        def algId: Long = -37L
        override def scheme: Optional[RsaPss[bits] & Signing] = cipher

  // `scheme` is the cipher to sign and verify with, where the algorithm fixes more than the
  // key's type does — ES256 is ECDSA *with SHA-256* — and `Unset` where the cipher in scope
  // is already the algorithm's. `wire` and `native` convert a signature between the
  // provider's form and COSE's, which differ only for ECDSA: COSE concatenates `r` and `s`
  // at the curve's width (RFC 9053 §2.1) where a provider writes them as DER.
  trait Algorithm:
    type Self
    def algId: Long
    def scheme: Optional[Self & Signing] = Unset
    def wire(signature: Data): Optional[Data] = signature
    def native(wire: Data): Optional[Data] = wire

  // CoseAuthenticator → Cose.Authenticator
  // Selects the COSE message variant from the key type:
  //   asymmetric (PrivateKey)        -> COSE_Sign1
  //   symmetric  (SymmetricKey)      -> COSE_Mac0
  // `SymmetricKey <: PrivateKey`, so resolution picks the more-specific
  // symmetric given for symmetric keys and falls back to asymmetric otherwise.
  object Authenticator:
    given asymmetric: [cipher <: Cipher & Signing]
    =>  ( algorithm: cipher & Signing, coseAlg: cipher is Cose.Algorithm )
    =>  PrivateKey[cipher] is Cose.Authenticator in Sign1 by cipher =
      new Cose.Authenticator:
        type Self    = PrivateKey[cipher]
        type Form    = Sign1
        type Operand = cipher
        def algId:         Long   = coseAlg.algId
        def contextString: String = Cose.Context.Signature1
        def cborTag:       Long   = Cose.Tag.Sign1

        def authenticate(toBeSigned: Data, key: PrivateKey[cipher]): Data =
          val scheme: cipher & Signing = coseAlg.scheme.or(algorithm)

          val signature = key.secret.uncloak: bytes =>
            scheme.sign(toBeSigned, Array.unsafeFrozen(bytes))

          coseAlg.wire(signature).or:
            panic(m"the provider produced a signature that is not of its own algorithm's form")

    given symmetric: [cipher <: Cipher & Symmetric & Signing]
    =>  ( algorithm: cipher & Signing, coseAlg: cipher is Cose.Algorithm )
    =>  SymmetricKey[cipher] is Cose.Authenticator in Mac0 by cipher =
      new Cose.Authenticator:
        type Self    = SymmetricKey[cipher]
        type Form    = Mac0
        type Operand = cipher
        def algId:         Long   = coseAlg.algId
        def contextString: String = Cose.Context.Mac0
        def cborTag:       Long   = Cose.Tag.Mac0

        def authenticate(toBeSigned: Data, key: SymmetricKey[cipher]): Data =
          key.secret.uncloak: bytes => algorithm.sign(toBeSigned, Array.unsafeFrozen(bytes))

  trait Authenticator:
    type Self
    type Form    <: Cose.Structure
    type Operand <: Cipher
    def algId:         Long
    def contextString: String
    def cborTag:       Long
    def authenticate(toBeSigned: Data, key: Self): Data

  // CoseError → Cose.Error
  object Error:
    object Reason:
      given communicable: Reason is Communicable =
        case MalformedStructure              => m"the COSE structure was not well-formed"
        case VariantMismatch(want, got)      => m"expected a $want COSE message but found a $got"
        case VerificationFailed              => m"the COSE signature or MAC did not verify"
        case CborParseError                  => m"the COSE message contained malformed CBOR"
        case DetachedPayloadRequired         => m"the COSE message has a detached payload"

        case UnknownTag(tag) =>
          m"the CBOR tag ${tag.toString} is not a COSE message tag"

        case UnsupportedAlgorithm(id) =>
          m"the COSE algorithm identifier ${id.toString} is not supported"

        case AlgorithmMismatch(want, got) =>
          m"expected COSE algorithm ${want.toString} but found ${got.toString}"

    enum Reason(val number: Int) extends Clarification:
      case MalformedStructure                              extends Reason(1)
      case UnknownTag(tag: Long)                           extends Reason(2)
      case UnsupportedAlgorithm(id: Long)                  extends Reason(3)
      case AlgorithmMismatch(expected: Long, actual: Long) extends Reason(4)
      case VariantMismatch(expected: Text, actual: Text)   extends Reason(5)
      case VerificationFailed                              extends Reason(6)
      case CborParseError                                  extends Reason(7)
      case DetachedPayloadRequired                         extends Reason(8)

  case class Error(reason: Cose.Error.Reason)(using Diagnostics)
  extends fulminate.Error(105, 6, reason.number)
    ( m"could not process the COSE message because $reason" )

  // CoseRecipient → Cose.Recipient
  case class Recipient(protectedHeader: Data, unprotectedHeader: Cbor, authentication: Data)

  // CoseVerifier → Cose.Verifier
  // Counterpart to Cose.Authenticator. Selects how the public/symmetric key is
  // used to verify a COSE signature or MAC.
  object Verifier:
    given asymmetric: [cipher <: Cipher & Signing]
    =>  ( algorithm: cipher & Signing, coseAlg: cipher is Cose.Algorithm )
    =>  PublicKey[cipher] is Cose.Verifier in Sign1 by cipher =
      new Cose.Verifier:
        type Self    = PublicKey[cipher]
        type Form    = Sign1
        type Operand = cipher
        def algId:         Long   = coseAlg.algId
        def contextString: String = Cose.Context.Signature1
        def cborTag:       Long   = Cose.Tag.Sign1

        def check(toBeSigned: Data, authentication: Data, key: PublicKey[cipher]): Boolean =
          val scheme: cipher & Signing = coseAlg.scheme.or(algorithm)

          coseAlg.native(authentication).lay(false): signature =>
            scheme.verify(toBeSigned, signature, key.bytes)

    given symmetric: [cipher <: Cipher & Symmetric & Signing]
    =>  ( algorithm: cipher & Signing, coseAlg: cipher is Cose.Algorithm )
    =>  SymmetricKey[cipher] is Cose.Verifier in Mac0 by cipher =
      new Cose.Verifier:
        type Self    = SymmetricKey[cipher]
        type Form    = Mac0
        type Operand = cipher
        def algId:         Long   = coseAlg.algId
        def contextString: String = Cose.Context.Mac0
        def cborTag:       Long   = Cose.Tag.Mac0

        def check(toBeSigned: Data, authentication: Data, key: SymmetricKey[cipher]): Boolean =
          key.secret.uncloak: bytes =>
            algorithm.verify(toBeSigned, authentication, Array.unsafeFrozen(bytes))

  trait Verifier:
    type Self
    type Form    <: Cose.Structure
    type Operand <: Cipher
    def algId:         Long
    def contextString: String
    def cborTag:       Long
    def check(toBeSigned: Data, authentication: Data, key: Self): Boolean

  // A COSE_Key (RFC 9052 §7) holding a public key: what a WebAuthn authenticator returns for
  // a new credential, and how CWT and other CBOR protocols carry keys. `algorithm` is the
  // key's `alg` (label 3) — the COSE algorithm it is to be used with, such as -7 for ES256 —
  // and `publicKey` the key itself, as the `SubjectPublicKeyInfo` every provider takes.
  //
  // The key types understood are EC2 on the three NIST prime curves, OKP for Ed25519 and
  // X25519, and RSA. A COSE_Key is written in the deterministic encoding, as CTAP2 requires.
  object Key:
    private[Cose] val Type: Long = 1L
    private[Cose] val Algorithm: Long = 3L
    private[Cose] val Curve: Long = -1L
    private[Cose] val X: Long = -2L
    private[Cose] val Y: Long = -3L
    private[Cose] val Okp: Long = 1L
    private[Cose] val Ec2: Long = 2L
    private[Cose] val RsaType: Long = 3L

    private def malformed(using Tactic[Cose.Error], Diagnostics): Nothing =
      abort(Cose.Error(Cose.Error.Reason.MalformedStructure))

    private[Cose] def unsigned(value: BigInt): Data =
      val bytes = Array.unsafeFrozen(value.toByteArray)
      if bytes.length > 1 && value.bitLength%8 == 0 then bytes.skip(1) else bytes

    // Reads a serialized COSE_Key.
    def parse(bytes: Data)(using cborTactic: Tactic[Cbor.Error], coseTactic: Tactic[Cose.Error])
      ( using Diagnostics )
    :   Cose.Key =

      val map = Cbor.Ast.parse(bytes)
      if !map.isMap then malformed

      def long(label: Long): Long = labelled(map, label).lay(malformed)(_.long)
      def data(label: Long): Data = labelled(map, label).lay(malformed)(readByteString(_))

      val algorithm = long(Algorithm)
      val keyType = long(Type)

      val publicKey: Data =
        if keyType == Ec2 then
          val curve = long(Curve)

          val bits =
            if curve == 1L then 256 else if curve == 2L then 384 else if curve == 3L then 521
            else abort(Cose.Error(Cose.Error.Reason.UnsupportedAlgorithm(algorithm)))

          val x = data(X)
          val y = data(Y)
          if x.length != (bits + 7)/8 || y.length != (bits + 7)/8 then malformed
          val prefix: Data = Array.fill[Byte](1)(4)
          Spki.ec(bits, Array.concat(prefix, x, y))

        else if keyType == Okp then
          val curve = long(Curve)

          val identifier =
            if curve == 6L then KeyAlgorithm.Ed25519 else if curve == 4L then KeyAlgorithm.X25519
            else abort(Cose.Error(Cose.Error.Reason.UnsupportedAlgorithm(algorithm)))

          Spki.wrap(Asn1.Sequence(List(Asn1.ObjectId(identifier))), data(X))

        else if keyType == RsaType then
          def number(label: Long): BigInt =
            BigInt(new java.math.BigInteger(1, Array.unsafeJvm(data(label))))

          Spki.rsa(number(-1L), number(-2L))

        else abort(Cose.Error(Cose.Error.Reason.UnsupportedAlgorithm(algorithm)))

      Cose.Key(algorithm, publicKey)

    // A public key as a COSE_Key, for the algorithm its cipher stands for.
    def apply[cipher <: Cipher](key: PublicKey[cipher])(using algorithm: cipher is Cose.Algorithm)
    :   Cose.Key =
      Cose.Key(algorithm.algId, key.bytes)

  case class Key(algorithm: Long, publicKey: Data):
    // The key as a key of a named cipher, which its type and size must agree with.
    def key[cipher <: Cipher: KeyAlgorithm](using Tactic[PublicKey.Error], Diagnostics)
    :   PublicKey[cipher] =
      PublicKey.adopt[cipher](publicKey)

    // The serialized COSE_Key, or `Unset` for a key of a type COSE_Key has no form for here.
    def encoded: Optional[Data] =
      def map(labels: List[Long], values: List[Any]): Data =
        val keys: Array[Any]^{} = labels.map[Any](identity).to[Array]
        val entries: Array[Any]^{} = values.to[Array]
        CanonicalCbor.encode(Cbor.Ast.map(keys, entries))

      safely(Der(publicKey).as[Asn1]) match
        case Asn1.Sequence(List(identifier, Asn1.BitString(bits, 0))) =>
          KeyAlgorithm.identifier(identifier).let: (arcs, parameters) =>
            if arcs == KeyAlgorithm.Ec then
              val curve: Optional[(Long, Int)] =
                if parameters == Asn1.ObjectId(KeyAlgorithm.P256) then (1L, 32)
                else if parameters == Asn1.ObjectId(KeyAlgorithm.P384) then (2L, 48)
                else if parameters == Asn1.ObjectId(KeyAlgorithm.P521) then (3L, 66)
                else Unset

              curve.let: (curve, size) =>
                if bits.length != 2*size + 1 then Unset else
                  val x: Data = bits.skip(1).keep(size)
                  val y: Data = bits.skip(1 + size)

                  map
                    ( List(Key.Type, Key.Algorithm, Key.Curve, Key.X, Key.Y),
                      List[Any](Key.Ec2, algorithm, curve, x, y) )

            else if arcs == KeyAlgorithm.Ed25519 then
              map
                ( List(Key.Type, Key.Algorithm, Key.Curve, Key.X),
                  List[Any](Key.Okp, algorithm, 6L, bits) )

            else if arcs == KeyAlgorithm.X25519 then
              map
                ( List(Key.Type, Key.Algorithm, Key.Curve, Key.X),
                  List[Any](Key.Okp, algorithm, 4L, bits) )

            else if arcs == KeyAlgorithm.Rsa then
              Spki.rsa(publicKey).let: (modulus, exponent) =>
                val n: Data = Key.unsigned(modulus)
                val e: Data = Key.unsigned(exponent)

                map
                  ( List(Key.Type, Key.Algorithm, -1L, -2L),
                    List[Any](Key.RsaType, algorithm, n, e) )

            else Unset

        case _ =>
          Unset

  // the COSE structure taxonomy, formerly nine top-level names
  trait Structure
  trait Signed extends Structure
  trait Maced  extends Structure

  trait Sign  extends Signed   // multi-signer,        CBOR tag 98
  trait Sign1 extends Signed   // single signer,       CBOR tag 18
  trait Mac   extends Maced    // multi-recipient MAC, CBOR tag 97
  trait Mac0  extends Maced    // single MAC,          CBOR tag 17

  object Tag:
    inline val Sign1 = 18L
    inline val Mac0  = 17L
    inline val Sign  = 98L
    inline val Mac   = 97L

  object Context:
    inline val Signature1 = "Signature1"
    inline val Signature  = "Signature"
    inline val Mac0       = "MAC0"
    inline val Mac        = "MAC"

class Cose
  ( val protectedHeader:   Data,
   val unprotectedHeader: Cbor,
   val payload:           Data,
   val contextString:     String,
   val cborTag:           Long,
   val recipients:        List[Cose.Recipient] ):
  type Form    <: Cose.Structure
  type Operand <: Cipher

  def verifyWith[key]
    ( key: key )
    ( using verifier: key is Cose.Verifier )
  :   Boolean raises Cose.Error =

    if verifier.contextString != contextString then
      abort(Cose.Error(Cose.Error.Reason.VariantMismatch
       ( expected = verifier.contextString.tt, actual = contextString.tt )))

    // The algorithm the message says it was made with must be the one the key's type stands
    // for: the header is checked against the key, and never chooses what is done with it.
    Cose.algorithm(protectedHeader).let: found =>
      if found != verifier.algId
      then abort(Cose.Error(Cose.Error.Reason.AlgorithmMismatch(verifier.algId, found)))

    val externalAad = Array.empty[Byte]
    val tbs = Cose.toBeSigned(contextString, protectedHeader, externalAad, payload)

    recipients.exists: recipient => verifier.check(tbs, recipient.authentication, key)
