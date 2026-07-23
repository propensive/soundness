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
┃    Soundness, version 0.63.0.                                                                    ┃
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

object Cose:
  private def emptyMapAst: Cbor.Ast =
    Cbor.Ast.map(IArray.empty[Any], IArray.empty[Any])

  private[enigmatic] def emptyMap: Cbor = Cbor.ast(emptyMapAst)

  private[enigmatic] def unsealOrEmpty(cbor: Cbor): Cbor.Ast =
    val ast = Cbor.unseal(cbor)
    if ast.isMap then ast else emptyMapAst

  private[enigmatic] def toBeSigned
    ( context: String, bodyProtected: Data, externalAad: Data, payload: Data )
  :   Data =

    val sigStruct =
      Cbor.Ast.array(IArray[Any](context, bodyProtected, externalAad, payload))

    CanonicalCbor.encode(sigStruct)

  private[enigmatic] def readByteString(ast: Cbor.Ast)(using Tactic[CoseError]): Data =
    if ast.isByteString then ast.asInstanceOf[Data]
    else abort(CoseError(CoseError.Reason.MalformedStructure))

  // Internal constructor that refines the phantom `Form` and `Operand` types.
  def make[scheme <: CoseStructure, cipher <: Cipher]
    ( protectedHeader:   Data,
     unprotectedHeader: Cbor,
     payload:           Data,
     contextString:     String,
     cborTag:           Long,
     recipients:        List[CoseRecipient] )
  :   Cose in scheme by cipher =

    new Cose(protectedHeader, unprotectedHeader, payload, contextString, cborTag, recipients):
      type Form    = scheme
      type Operand = cipher


  // User-facing constructor. The key's type selects the variant via
  // `CoseAuthenticator` (asymmetric -> Sign1, symmetric -> Mac0). Inlined so
  // `source.read[Data]` expands at the call site, picking up the caller's
  // readability and tactic instances.
  inline def apply[source, key]
    ( source: source, key: key )
    ( using auth: key is CoseAuthenticator,
            cborTactic: Tactic[CborError],
            readable: source is Readable to Data )
  :   Cose in auth.Form by auth.Operand raises CoseError =

    val payload: Data  = source.read[Data]
    val algId          = auth.algId
    val protectedAst   = Cbor.Ast.map(IArray[Any](1L), IArray[Any](algId))
    val protectedBstr  = CanonicalCbor.encode(protectedAst)
    val externalAad    = IArray.empty[Byte]
    val tbs            = Cose.toBeSigned(auth.contextString, protectedBstr, externalAad, payload)
    val authentication = auth.authenticate(tbs, key)
    val recipient      = CoseRecipient(IArray.empty[Byte], Cose.emptyMap, authentication)

    Cose.make[auth.Form, auth.Operand]
      ( protectedBstr,
       Cose.emptyMap,
       payload,
       auth.contextString,
       auth.cborTag,
       List(recipient) )


  // Parse a tagged COSE envelope. The variant is determined from the CBOR
  // tag; the returned phantom types are the most-general bounds.
  def parse(bytes: Data)
    ( using cborTactic: Tactic[CborError] )
  :   Cose raises CoseError =

    val ast = Cbor.Ast.parse(bytes)

    if !ast.isTag then abort(CoseError(CoseError.Reason.MalformedStructure))

    val tag       = ast.asInstanceOf[Cbor.Tag]
    val tagNumber = tag.tag

    val contextString = tagNumber match
      case CoseTag.Sign1 => CoseContext.Signature1
      case CoseTag.Mac0  => CoseContext.Mac0
      case CoseTag.Sign  => CoseContext.Signature
      case CoseTag.Mac   => CoseContext.Mac
      case other         => abort(CoseError(CoseError.Reason.UnknownTag(other)))

    val body = tag.value.asInstanceOf[Cbor.Ast]

    if !body.isArray || body.elements != 4 then
      abort(CoseError(CoseError.Reason.MalformedStructure))

    val protectedHeader = readByteString(body.element(0))
    val unprotectedAst  = body.element(1)
    if !unprotectedAst.isMap then abort(CoseError(CoseError.Reason.MalformedStructure))

    val payload = readByteString(body.element(2))

    val recipients: List[CoseRecipient] = tagNumber match
      case CoseTag.Sign1 | CoseTag.Mac0 =>
        List(CoseRecipient(IArray.empty[Byte], emptyMap, readByteString(body.element(3))))

      case _ =>
        val recipArray = body.element(3)
        if !recipArray.isArray then abort(CoseError(CoseError.Reason.MalformedStructure))
        val builder = scala.collection.immutable.List.newBuilder[CoseRecipient]
        var index = 0

        while index < recipArray.elements do
          val entry = recipArray.element(index)

          if !entry.isArray || entry.elements != 3 then
            abort(CoseError(CoseError.Reason.MalformedStructure))

          val rp = readByteString(entry.element(0))
          val ru = entry.element(1)
          if !ru.isMap then abort(CoseError(CoseError.Reason.MalformedStructure))
          val ra = readByteString(entry.element(2))
          builder += CoseRecipient(rp, Cbor.ast(ru), ra)
          index += 1

        List.of(builder.result())

    new Cose
      ( protectedHeader, Cbor.ast(unprotectedAst), payload, contextString, tagNumber, recipients ):
      type Form    = CoseStructure
      type Operand = Cipher


class Cose
  ( val protectedHeader:   Data,
   val unprotectedHeader: Cbor,
   val payload:           Data,
   val contextString:     String,
   val cborTag:           Long,
   val recipients:        List[CoseRecipient] ):
  type Form    <: CoseStructure
  type Operand <: Cipher

  // Serialise this COSE message to its CBOR-tagged wire form.
  def bytes: Data =
    val unprotectedAst: Cbor.Ast = Cose.unsealOrEmpty(unprotectedHeader)

    val envelope = cborTag match
      case CoseTag.Sign1 | CoseTag.Mac0 =>
        val auth = recipients.stdlib.head.authentication
        Cbor.Ast.array(IArray[Any](protectedHeader, unprotectedAst, payload, auth))

      case _ =>
        val recipAst: IArray[Any] = IArray.from(recipients.stdlib.map: r =>
          Cbor.Ast.array(IArray[Any](r.protectedHeader, Cose.unsealOrEmpty(r.unprotectedHeader),
            r.authentication)))

        Cbor.Ast.array(IArray[Any](protectedHeader, unprotectedAst, payload,
          Cbor.Ast.array(recipAst)))

    Cbor.Ast(Cbor.Tag(cborTag, envelope)).encode


  def verifyWith[key]
    ( key: key )
    ( using verifier: key is CoseVerifier )
  :   Boolean raises CoseError =

    if verifier.contextString != contextString then
      abort(CoseError(CoseError.Reason.VariantMismatch
       ( expected = verifier.contextString.tt, actual = contextString.tt )))

    val externalAad = IArray.empty[Byte]
    val tbs = Cose.toBeSigned(contextString, protectedHeader, externalAad, payload)

    recipients.stdlib.exists: recipient =>
      verifier.check(tbs, recipient.authentication, key)
