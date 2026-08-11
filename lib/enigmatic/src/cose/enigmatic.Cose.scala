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

import anticipation.*
import breviloquence.*
import contingency.*
import enigmatic.*
import prepositional.*
import turbulence.*
import gastronomy.*
import rudiments.*
import vacuous.*
import fulminate.*

object Cose:
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
      Cbor.Ast.array(Array.of[Any](context, bodyProtected, externalAad, payload))

    CanonicalCbor.encode(sigStruct)

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
            cborTactic: Tactic[CborError],
            readable: source is Readable to Data )
  :   Cose in auth.Form by auth.Operand raises Cose.Error =

    val payload: Data  = source.read[Data]
    val algId          = auth.algId
    val protectedAst   = Cbor.Ast.map(Array.of[Any](1L), Array.of[Any](algId))
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
    ( using cborTactic: Tactic[CborError], coseTactic: Tactic[Cose.Error] )
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
      case other         => abort(Cose.Error(Cose.Error.Reason.UnknownTag(other)))

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

        List.of(builder.result())

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

  trait Algorithm:
    type Self
    def algId: Long

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
          key.secret.uncloak: bytes =>
            algorithm.sign(toBeSigned, Array.unsafeFrozen(bytes))

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
          key.secret.uncloak: bytes =>
            algorithm.sign(toBeSigned, Array.unsafeFrozen(bytes))

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
  extends fulminate.Error(596, reason.number)(m"could not process the COSE message because $reason")

  // CoseRecipient → Cose.Recipient
  case class Recipient(protectedHeader: Data, unprotectedHeader: Cbor, authentication: Data)

  // CoseVerifier → Cose.Verifier
  // Counterpart to Cose.Authenticator. Selects how the public/symmetric key is
  // used to verify a COSE signature or MAC.
  object Verifier:
    given asymmetric: [cipher <: Cipher & Signing]
    =>  ( algorithm: cipher & Signing )
    =>  PublicKey[cipher] is Cose.Verifier in Sign1 by cipher =
      new Cose.Verifier:
        type Self    = PublicKey[cipher]
        type Form    = Sign1
        type Operand = cipher
        def contextString: String = Cose.Context.Signature1
        def cborTag:       Long   = Cose.Tag.Sign1

        def check(toBeSigned: Data, authentication: Data, key: PublicKey[cipher]): Boolean =
          algorithm.verify(toBeSigned, authentication, key.bytes)

    given symmetric: [cipher <: Cipher & Symmetric & Signing]
    =>  ( algorithm: cipher & Signing )
    =>  SymmetricKey[cipher] is Cose.Verifier in Mac0 by cipher =
      new Cose.Verifier:
        type Self    = SymmetricKey[cipher]
        type Form    = Mac0
        type Operand = cipher
        def contextString: String = Cose.Context.Mac0
        def cborTag:       Long   = Cose.Tag.Mac0

        def check(toBeSigned: Data, authentication: Data, key: SymmetricKey[cipher]): Boolean =
          key.secret.uncloak: bytes =>
            algorithm.verify(toBeSigned, authentication, Array.unsafeFrozen(bytes))

  trait Verifier:
    type Self
    type Form    <: Cose.Structure
    type Operand <: Cipher
    def contextString: String
    def cborTag:       Long
    def check(toBeSigned: Data, authentication: Data, key: Self): Boolean

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

  // Serialise this COSE message to its CBOR-tagged wire form.
  def bytes: Data =
    val unprotectedAst: Cbor.Ast = Cose.unsealOrEmpty(unprotectedHeader)

    val envelope = cborTag match
      case Cose.Tag.Sign1 | Cose.Tag.Mac0 =>
        val auth = recipients.stdlib.head.authentication
        Cbor.Ast.array(Array.of[Any](protectedHeader, unprotectedAst, payload, auth))

      case _ =>
        val recipAst: Array[Any]^{} = Array.from(recipients.stdlib.map: r =>
          Cbor.Ast.array(Array.of[Any](r.protectedHeader, Cose.unsealOrEmpty(r.unprotectedHeader),
            r.authentication)))

        Cbor.Ast.array(Array.of[Any](protectedHeader, unprotectedAst, payload,
          Cbor.Ast.array(recipAst)))

    Cbor.Ast(Cbor.Tag(cborTag, envelope)).encode


  def verifyWith[key]
    ( key: key )
    ( using verifier: key is Cose.Verifier )
  :   Boolean raises Cose.Error =

    if verifier.contextString != contextString then
      abort(Cose.Error(Cose.Error.Reason.VariantMismatch
       ( expected = verifier.contextString.tt, actual = contextString.tt )))

    val externalAad = Array.empty[Byte]
    val tbs = Cose.toBeSigned(contextString, protectedHeader, externalAad, payload)

    recipients.stdlib.exists: recipient =>
      verifier.check(tbs, recipient.authentication, key)
