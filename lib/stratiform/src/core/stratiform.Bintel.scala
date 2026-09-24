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
package stratiform

import java.nio.charset.StandardCharsets

import scala.language.unsafeNulls

import anticipation.*
import rudiments.*
import contingency.*
import denominative.*
import fulminate.*
import prepositional.*
import ulysses.*
import vacuous.*

object Bintel:

  // §6.1 magic number: the 4 bytes that prefix every external-schema
  // BinTEL document, followed by the document length (§6, a varint
  // counting every byte after it) that makes the document self-framing.
  // When viewed as BASE-256 text these are the four Greek letters
  // `β τ ε λ` — visually evocative of "binary TEL".
  val magic: Data =
    scala.Array[Byte](0xb2.toByte, 0xc4.toByte, 0xb5.toByte, 0xbb.toByte)
      .asInstanceOf[Array[Byte]^{}]

  // §6.2 self-contained magic number. In BASE-256 text these are the four
  // characters `β τ ε μ` — the trailing `μ` (for *monolithic*) distinguishes
  // self-contained mode from external mode's `βτελ`.
  val magicSelfContained: Data =
    scala.Array[Byte](0xb2.toByte, 0xc4.toByte, 0xb5.toByte, 0xbc.toByte)
      .asInstanceOf[Array[Byte]^{}]

  // The result of unframing one §6 document: the carried schema signature
  // bytes, the document-root body bytes, and the offset at which the
  // document's continuation (§6.3) begins.
  case class Framed(signature: Data, body: Data, continuation: Int)

  // A fully decoded BinTEL document: the carried schema signature bytes,
  // the recovered semantic-model `Tel.Element` root, and the offset at
  // which the document's continuation (§6.3) begins — one past its last
  // byte, as fixed by the declared document length. Bytes from there are
  // the caller's to interpret: a further document, content in another
  // format, or nothing at all.
  case class Document(signature: Data, root: Tel.Element, continuation: Int)

  // §11: the decoder's nesting-depth limit, matching the type-assignment
  // limit of TEL §20.2 so that every document a conforming parser accepts
  // is also decodable. Exceeding it is a resource error of the decoder's
  // configuration, not a B-code of the document.
  private val nestingLimit = 256

  // Encode a `Tel.Element` tree to its BinTEL body bytes. The element is
  // expected to be the document root (a Node with `keywordIndex = Unset`
  // and `elementType = Tels.Struct`), as produced by `Tel.Type.assign`.
  // With no codec binding, encountering a scalar with a declared encoding
  // (§21.7) is a configuration failure of the encoder: B13, and nothing
  // is emitted. The E312 mitigation is unreachable on this path — a codec
  // can only reject a value where a binding resolved one.
  def encode(element: Tel.Element, schema: Tels): Data raises Bintel.Error =
    import errorDiagnostics.emptyDiagnostics

    mitigate:
      case _: Tel.Error => Bintel.Error(Bintel.Error.Reason.CodecUnresolved)

    . protect(encodeAll(element, schema, Unset))

  // Codec-aware encoding: scalars with a declared encoding are written as
  // the bound codec's bytes (§7.1 two-form payload; framing is identical
  // to the UTF-8 form). An unresolved encoding name raises B13
  // (`Bintel.Error`); a value the codec's encoder rejects raises
  // `Tel.Error(EncodingRejected)` — the E312 condition, since the
  // document is invalid under its schema. Either way, nothing is emitted.
  def encode(element: Tel.Element, schema: Tels, codecs: Tel.Codec.Bindings)
    ( using Tactic[Tel.Error], Tactic[Bintel.Error] )
  :   Data =

    encodeAll(element, schema, Tel.Codec.Resolver(codecs))

  private def encodeAll
    ( element: Tel.Element, schema: Tels, codecs: Optional[Tel.Codec.Resolver] )
    ( using Tactic[Tel.Error], Tactic[Bintel.Error] )
  :   Data =

    Array.collect[Byte](): out =>
      encodeRoot(out, element, schema, codecs)

  // Is `signature` a syntactically-valid palimpsest? Recovers the cadence
  // byte from the XOR-fold of every byte and checks the byte length is
  // consistent with the recovered `(H, k_i, k_r)`. Returns true iff so.
  private def validSignatureLength(signature: Data): Boolean =
    val total = signature.length

    if total < 2 then false else
      var xor = 0
      var i   = 0

      while i < total do
        xor = xor ^ (signature.readable(i) & 0xff)
        i += 1

      Cadence.unpack(xor.toByte).let(_.hashCount(total - 1)).present

  // §6.1 framing. Wrap a body byte sequence with the magic number, the
  // document length (a varint counting every byte that follows it), the
  // signature length (varint), and the signature bytes. The signature
  // length MUST be a valid palimpsest length under some `(H, k_i, k_r)`,
  // recovered from the trailing cadence byte (§8.2 of bintel.md, §5.2
  // of palimpsest.md); otherwise raises `BadSignatureLength`.
  def frame(body: Data, signature: Data): Data raises Bintel.Error =
    if !validSignatureLength(signature)
    then abort(Bintel.Error(Bintel.Error.Reason.BadSignatureLength))

    Array.collect[Byte](magic.length + 20 + signature.length + body.length): out =>
      out.append(magic)
      val sigLen = Varint.encode(signature.length.toLong)
      writeVarint(out, (sigLen.length + signature.length + body.length).toLong)
      out.append(sigLen)
      out.append(signature)
      out.append(body)

  private def matchesMagic(data: Data, offset: Int, expected: Data): Boolean =
    data.length - offset >= expected.length && {
      var i  = 0
      var ok = true

      while ok && i < expected.length do
        if data.readable(offset + i) != expected.readable(i) then ok = false
        i += 1

      ok
    }

  // A varint read from `data` at `at` and confined to `[at, end)`: B02 for
  // any §4 failure, including a varint with no bytes available or one
  // that runs past `end` (§10's precedence: truncation inside a varint
  // is B02, not B09).
  private def varintAt(data: Data, at: Int, end: Int): Varint.Decoded raises Bintel.Error =
    import errorDiagnostics.emptyDiagnostics

    if at >= end then abort(Bintel.Error(Bintel.Error.Reason.VarintError))

    val decoded =
      mitigate:
        case _: Varint.Error => Bintel.Error(Bintel.Error.Reason.VarintError)

      . protect(Varint.decode(data, at))

    if decoded.next > end then abort(Bintel.Error(Bintel.Error.Reason.VarintError))
    decoded

  // §6.1/§6.2 fields 1 and 2: the magic number, which selects the mode,
  // and the declared document length. Returns whether the document is
  // self-contained, the offset of its first byte after the length field,
  // and the offset of its continuation. The declared length is checked
  // against the bytes remaining before it is acted on (§11: B09 when it
  // overruns), and never sizes a buffer.
  private def readHeader(data: Data, offset: Int): (Boolean, Int, Int) raises Bintel.Error =
    if offset < 0 || data.length - offset < magic.length
    then abort(Bintel.Error(Bintel.Error.Reason.UnexpectedEoi))

    val selfContained =
      if matchesMagic(data, offset, magic) then false
      else if matchesMagic(data, offset, magicSelfContained) then true
      else abort(Bintel.Error(Bintel.Error.Reason.BadMagic))

    val declared  = varintAt(data, offset + magic.length, data.length)
    val bodyStart = declared.next

    if declared.value > (data.length - bodyStart).toLong
    then abort(Bintel.Error(Bintel.Error.Reason.UnexpectedEoi))

    (selfContained, bodyStart, bodyStart + declared.value.toInt)

  // §6.1 field 3 / §6.2 field 3: the signature length and bytes, read
  // within `[offset, end)`. B03 if the length is not a valid palimpsest
  // length; B09 if the bytes are truncated.
  private def readSignature(data: Data, offset: Int, end: Int): (Data, Int) raises Bintel.Error =
    val sigLen   = varintAt(data, offset, end)
    val sigStart = sigLen.next

    if sigLen.value > (end - sigStart).toLong
    then abort(Bintel.Error(Bintel.Error.Reason.UnexpectedEoi))

    val sigEnd    = sigStart + sigLen.value.toInt
    val signature = data.segment(sigStart.z till sigEnd.z)

    if !validSignatureLength(signature)
    then abort(Bintel.Error(Bintel.Error.Reason.BadSignatureLength))

    (signature, sigEnd)

  // §6.1 unframing. Parse the external-schema document beginning at
  // `offset` into its signature bytes and body bytes without decoding the
  // body. Validates the magic number (B01) and the signature length
  // pattern (B03). The body's structural extent is not checked against
  // the declared length here (that is B16, raised by the decoders), and
  // the continuation is returned rather than judged (§6.3).
  def unframe(data: Data): Framed raises Bintel.Error = unframe(data, 0)

  def unframe(data: Data, offset: Int): Framed raises Bintel.Error =
    val (selfContained, bodyStart, end) = readHeader(data, offset)
    if selfContained then abort(Bintel.Error(Bintel.Error.Reason.BadMagic))
    val (signature, sigEnd) = readSignature(data, bodyStart, end)
    Framed(signature, data.segment(sigEnd.z till end.z), end)

  // §6.3 framing without decoding: read the magic number and the declared
  // document length of the document beginning at `offset`, in either mode,
  // and return the offset at which its continuation begins. Resolves no
  // schema and allocates nothing proportional to the document, so a reader
  // may count, skip, split or forward the documents of a stream at
  // constant cost each.
  def documentExtent(data: Data): Int raises Bintel.Error = documentExtent(data, 0)

  def documentExtent(data: Data, offset: Int): Int raises Bintel.Error =
    readHeader(data, offset)(2)

  // §6.1 + §7.8 — single-document decoding (§6.3): decode the external-
  // schema document beginning at `offset` under `schema`, returning the
  // signature, the semantic-model root and the continuation offset. Bytes
  // beyond the document are neither decoded nor validated, and need not
  // be BinTEL. This layer does not verify the signature against the
  // schema; `SchemaResolver` does.
  def decodeDocument(data: Data, schema: Tels): Document raises Bintel.Error =
    decodeFramed(data, 0, schema, Unset, false)

  def decodeDocument(data: Data, schema: Tels, offset: Int): Document raises Bintel.Error =
    decodeFramed(data, offset, schema, Unset, false)

  // Codec-aware single-document decoding; see `decode` for the B13/B14/B15
  // rules.
  def decodeDocument
    ( data: Data, schema: Tels, codecs: Tel.Codec.Bindings,
      checkCanonical: Boolean = false )
  :   Document raises Bintel.Error =

    decodeFramed(data, 0, schema, Tel.Codec.Resolver(codecs), checkCanonical)

  def decodeDocument
    ( data: Data, schema: Tels, codecs: Tel.Codec.Bindings, checkCanonical: Boolean,
      offset: Int )
  :   Document raises Bintel.Error =

    decodeFramed(data, offset, schema, Tel.Codec.Resolver(codecs), checkCanonical)

  // §6.3 whole-document reading: `data` is exactly one external-schema
  // document and nothing else, so a non-empty continuation is B08. That
  // is a property of this reader's contract, not of the bytes, which are
  // a well-formed stream to `decodeStream`.
  def decodeWholeDocument(data: Data, schema: Tels): Document raises Bintel.Error =
    whole(data, decodeDocument(data, schema))

  def decodeWholeDocument
    ( data: Data, schema: Tels, codecs: Tel.Codec.Bindings,
      checkCanonical: Boolean = false )
  :   Document raises Bintel.Error =

    whole(data, decodeDocument(data, schema, codecs, checkCanonical))

  private def whole(data: Data, document: Document): Document raises Bintel.Error =
    if document.continuation != data.length
    then abort(Bintel.Error(Bintel.Error.Reason.TrailingBytes))

    document

  // §6.3 stream decoding: the documents of `data` in order, by recursion
  // on the continuation. Every document is independent: an external-
  // schema document is decoded under `schema` and a self-contained one
  // under its embedded schema, so the modes may be interleaved freely. A
  // continuation beginning with neither magic number is B01 at that
  // position, and decoding stops at the first error. A reader consuming an
  // untrusted stream should bound the count and bytes it accepts (§11); a
  // stream whose external-schema documents are typed by several schemas
  // is driven by the caller with `documentExtent` and `unframe`, choosing
  // a schema per signature.
  def decodeStream(data: Data, schema: Tels): List[Document] raises Bintel.Error =
    decodeStreamAll(data, schema, Unset, false)

  def decodeStream
    ( data: Data, schema: Tels, codecs: Tel.Codec.Bindings,
      checkCanonical: Boolean = false )
  :   List[Document] raises Bintel.Error =

    decodeStreamAll(data, schema, Tel.Codec.Resolver(codecs), checkCanonical)

  private def decodeStreamAll
    ( data: Data, schema: Tels, codecs: Optional[Tel.Codec.Resolver],
      checkCanonical: Boolean )
  :   List[Document] raises Bintel.Error =

    val buffer = scala.collection.mutable.ListBuffer.empty[Document]
    var offset = 0

    while offset < data.length do
      val document =
        if matchesMagic(data, offset, magicSelfContained)
        then decodeSelfContainedAll(data, offset, codecs, checkCanonical)
        else decodeFramed(data, offset, schema, codecs, checkCanonical)

      buffer += document
      offset = document.continuation

    buffer.to(List)

  private def decodeFramed
    ( data: Data, offset: Int, schema: Tels, codecs: Optional[Tel.Codec.Resolver],
      checkCanonical: Boolean )
  :   Document raises Bintel.Error =

    val (selfContained, bodyStart, end) = readHeader(data, offset)

    // This entry point handles external-schema mode only; a self-contained
    // document is decoded by `decodeDocumentSelfContained`, or dispatched
    // to it by `decodeStream`.
    if selfContained then abort(Bintel.Error(Bintel.Error.Reason.BadMagic))

    val (signature, sigEnd) = readSignature(data, bodyStart, end)
    val body = data.segment(sigEnd.z till end.z)
    Document(signature, decodeExtent(body, schema, codecs, checkCanonical), end)

  // §6.1 field 2 / §6.2 field 2: decode a document root whose extent the
  // declared length fixed, and require the structural extent to agree
  // (B16). The declared length is what lets a reader delimit a document
  // without a schema; the agreement check is what stops a forged length
  // concealing bytes inside a document or exposing bytes of the next.
  private def decodeExtent
    ( body: Data, schema: Tels, codecs: Optional[Tel.Codec.Resolver], checkCanonical: Boolean )
  :   Tel.Element raises Bintel.Error =

    val (root, consumed) = decodeBody(body, schema, codecs, checkCanonical)

    if consumed != body.length
    then abort(Bintel.Error(Bintel.Error.Reason.DeclaredLengthMismatch))

    root

  // §6.2 self-contained framing: magic_BC, document length, signature
  // (length varint + bytes), embedded schema body (length varint + bytes),
  // document root. The signature length MUST be a valid palimpsest length;
  // otherwise raises `BadSignatureLength`.
  def frameSelfContained(signature: Data, schemaBody: Data, body: Data)
  :   Data raises Bintel.Error =

    if !validSignatureLength(signature)
    then abort(Bintel.Error(Bintel.Error.Reason.BadSignatureLength))

    val hint =
      magicSelfContained.length + 30 + signature.length + schemaBody.length + body.length

    Array.collect[Byte](hint): out =>
      val sigLen = Varint.encode(signature.length.toLong)
      val schLen = Varint.encode(schemaBody.length.toLong)
      out.append(magicSelfContained)

      writeVarint
        ( out,
          (sigLen.length + signature.length + schLen.length + schemaBody.length + body.length)
          . toLong )

      out.append(sigLen)
      out.append(signature)
      out.append(schLen)
      out.append(schemaBody)
      out.append(body)

  // §6.2 self-contained encoding of the TEL document `tel`, whose schema is given
  // as the TEL document `schemaDoc` (parseable under the tels axiom). The
  // schema's signature and bintel body are embedded so that a receiver holding
  // only the axiom can decode the result with no external schema resolution.
  def selfContained(tel: Tel, schemaDoc: Tel)
    ( using Tactic[Tel.Error], Tactic[Bintel.Error] )
  :   Data =

    val axiom      = Tels.Axiom.tels
    val schema     = Tels.Layers.compose(Tels.Reconstructor.fromTel(schemaDoc))
    val signature  = SchemaSignature.fromDocument(schemaDoc, axiom)
    val schemaBody = schemaDoc.bintel(axiom)
    frameSelfContained(signature, schemaBody, tel.bintel(schema))

  // §6.2 decoder — single-document decoding (§6.3) of the self-contained
  // document beginning at `offset`. The embedded schema body is decoded
  // under the tels axiom and used to reconstruct the composed schema (B12
  // on any failure); its signature is recomputed and verified byte-for-
  // byte against the carried signature (B11 on mismatch) before the
  // document root is decoded under the reconstructed schema. Verification
  // proves only that the body is internally consistent with its
  // signature: a receiver requiring a *trusted* schema must compare the
  // signature against those it knows (§11).
  def decodeDocumentSelfContained(data: Data): Document raises Bintel.Error =
    decodeSelfContainedAll(data, 0, Unset, false)

  def decodeDocumentSelfContained(data: Data, offset: Int): Document raises Bintel.Error =
    decodeSelfContainedAll(data, offset, Unset, false)

  // Codec-aware §6.2 decoding: the binding applies only to the *outer*
  // document root. The embedded schema body is governed by `tels`, which
  // declares no encodings, so the bootstrap never requires a codec.
  def decodeDocumentSelfContained
    ( data: Data, codecs: Tel.Codec.Bindings, checkCanonical: Boolean = false )
  :   Document raises Bintel.Error =

    decodeSelfContainedAll(data, 0, Tel.Codec.Resolver(codecs), checkCanonical)

  def decodeDocumentSelfContained
    ( data: Data, codecs: Tel.Codec.Bindings, checkCanonical: Boolean, offset: Int )
  :   Document raises Bintel.Error =

    decodeSelfContainedAll(data, offset, Tel.Codec.Resolver(codecs), checkCanonical)

  // §6.3 whole-document reading of a self-contained document: B08 on a
  // non-empty continuation.
  def decodeWholeDocumentSelfContained(data: Data): Document raises Bintel.Error =
    whole(data, decodeDocumentSelfContained(data))

  def decodeWholeDocumentSelfContained
    ( data: Data, codecs: Tel.Codec.Bindings, checkCanonical: Boolean = false )
  :   Document raises Bintel.Error =

    whole(data, decodeDocumentSelfContained(data, codecs, checkCanonical))

  private def decodeSelfContainedAll
    ( data: Data, offset: Int, codecs: Optional[Tel.Codec.Resolver], checkCanonical: Boolean )
  :   Document raises Bintel.Error =

    import errorDiagnostics.emptyDiagnostics

    val (selfContained, bodyStart, end) = readHeader(data, offset)
    if !selfContained then abort(Bintel.Error(Bintel.Error.Reason.BadMagic))

    val (signature, sigEnd) = readSignature(data, bodyStart, end)

    val schLen   = varintAt(data, sigEnd, end)
    val schStart = schLen.next

    if schLen.value > (end - schStart).toLong
    then abort(Bintel.Error(Bintel.Error.Reason.UnexpectedEoi))

    val schEnd     = schStart + schLen.value.toInt
    val schemaBody = data.segment(schStart.z till schEnd.z)
    val docBody    = data.segment(schEnd.z till end.z)

    val axiom = Tels.Axiom.tels

    // Decode + reconstruct the embedded schema and recompute its signature;
    // any structural failure here is B12.
    val (composed, recomputed) =
      mitigate:
        case _: Tel.Error    => Bintel.Error(Bintel.Error.Reason.EmbeddedSchemaUndecodable)
        case _: Bintel.Error => Bintel.Error(Bintel.Error.Reason.EmbeddedSchemaUndecodable)

      . protect:
          val schemaRoot = decode(schemaBody, axiom).asInstanceOf[Tel.Element.Node]
          val baseTels   = Tels.SemanticReconstructor.fromElement(schemaRoot)
          val sig        = SchemaSignature.fromElement(schemaRoot, axiom)
          (Tels.Layers.compose(baseTels), sig)

    if !bytesEqual(recomputed, signature)
    then abort(Bintel.Error(Bintel.Error.Reason.EmbeddedSignatureMismatch))

    Document(signature, decodeExtent(docBody, composed, codecs, checkCanonical), end)

  private def bytesEqual(a: Data, b: Data): Boolean =
    a.length == b.length && {
      var i = 0
      var equal = true

      while i < a.length && equal do
        if a.readable(i) != b.readable(i) then equal = false
        i += 1

      equal
    }

  // §9 textual encoding. The text form is one BASE-256 character per
  // byte of the underlying BinTEL document; round-trips losslessly
  // via `Base256.decode`. The text begins with `βτελ` — the four
  // BASE-256 characters for the magic bytes.
  def text(data: Data): Text = Base256.encode(data)

  // §9 textual decoding. Permissively maps each character's code-point
  // mod 256 back to a byte. Use `Base256.decodeStrict` first if the
  // input may have come from an untrusted source.
  def fromText(input: Text): Data = Base256.decode(input)

  // §7.8 decoder. Read BinTEL body bytes (no magic, no signature —
  // exactly what `encode` emits) under `schema`, recovering the
  // semantic-model `Tel.Element` tree. The schema must be the same
  // composed schema used at encode time. Any framing or schema
  // mismatch raises `Bintel.Error`.
  def decode(data: Data, schema: Tels): Tel.Element raises Bintel.Error =
    decodeAll(data, schema, Unset, false)

  // Codec-aware decoding: an encoded scalar's value bytes are passed to
  // the bound codec (B13 when the name does not resolve or no binding is
  // configured; B14 when the codec rejects the bytes). `checkCanonical`
  // additionally performs the OPTIONAL re-encode verification of §21.7:
  // B15 when `encode(decode(b)) ≠ b`.
  def decode
    ( data: Data, schema: Tels, codecs: Tel.Codec.Bindings,
      checkCanonical: Boolean = false )
  :   Tel.Element raises Bintel.Error =

    decodeAll(data, schema, Tel.Codec.Resolver(codecs), checkCanonical)

  // The whole-body reader: the body is exactly one document root, so
  // bytes remaining after it are B08.
  private def decodeAll
    ( data: Data, schema: Tels, codecs: Optional[Tel.Codec.Resolver],
      checkCanonical: Boolean )
  :   Tel.Element raises Bintel.Error =

    val (root, consumed) = decodeBody(data, schema, codecs, checkCanonical)
    if consumed != data.length then abort(Bintel.Error(Bintel.Error.Reason.TrailingBytes))
    root

  // §7.8 over body bytes: the document root and the number of bytes its
  // structure consumed.
  private def decodeBody
    ( data: Data, schema: Tels, codecs: Optional[Tel.Codec.Resolver],
      checkCanonical: Boolean )
  :   (Tel.Element, Int) raises Bintel.Error =

    val cursor = Cursor(data, 0)

    val root =
      decodeStructBody(cursor, schema.document, schema, keywordIndex = Unset, codecs,
        checkCanonical, depth = 0)

    (root, cursor.offset)

  private def decodeStructBody
    ( cursor: Cursor, struct: Tels.Struct, schema: Tels,
      keywordIndex: Optional[Int], codecs: Optional[Tel.Codec.Resolver],
      checkCanonical: Boolean, depth: Int )
  :   Tel.Element raises Bintel.Error =

    // §11 resource limits: every count in the stream is adversarial, so
    // the decoder fails rather than exhausting the stack or the heap.
    if depth > nestingLimit then abort(Bintel.Error(Bintel.Error.Reason.NestingLimitExceeded))

    val flat = flattenKeywords(struct, schema)
    val childCount = readVarint(cursor)

    // Each child consumes at least one byte (its keyword index), so a count
    // exceeding the bytes remaining is unsatisfiable and is rejected before
    // anything is allocated; storage grows only as children are read.
    if childCount > (cursor.data.length - cursor.offset).toLong
    then abort(Bintel.Error(Bintel.Error.Reason.UnexpectedEoi))

    val children = scala.collection.mutable.ArrayBuffer.empty[Tel.Element]
    var i = 0L

    while i < childCount do
      children += decodeElement(cursor, flat, schema, codecs, checkCanonical, depth)
      i += 1

    Tel.Element.Node(keywordIndex, struct, Array.from(children))

  private def decodeElement
    ( cursor: Cursor, flat: Array[(Text, Tels.Type)]^{}, schema: Tels,
      codecs: Optional[Tel.Codec.Resolver], checkCanonical: Boolean, depth: Int )
  :   Tel.Element raises Bintel.Error =

    val kidx = readVarint(cursor)
    if kidx < 0 || kidx >= flat.length then abort(Bintel.Error(Bintel.Error.Reason.BadKeywordIndex))
    val (_, memberType) = flat.readable(kidx.toInt)
    val resolved = resolveType(memberType, schema)

    resolved match
      case s: Tels.Struct =>
        decodeStructBody(cursor, s, schema, keywordIndex = kidx.toInt, codecs, checkCanonical,
          depth + 1)

      case s: Tels.Scalar =>
        // The value's byte length is read before any codec is consulted,
        // so a B13 failure below is precise, not a parse ambiguity.
        val len = readVarint(cursor)

        if cursor.offset + len > cursor.data.length
        then abort(Bintel.Error(Bintel.Error.Reason.ValueTruncated))

        val bytes = new scala.Array[Byte](len.toInt)
        var j = 0

        while j < len.toInt do
          bytes(j) = cursor.data.readable(cursor.offset + j)
          j += 1

        cursor.offset += len.toInt

        val text = s.encoding.let: name =>
          val codec = codecs.let(_(name))
          . or(abort(Bintel.Error(Bintel.Error.Reason.CodecUnresolved)))

          val frozen = bytes.asInstanceOf[Data]

          codec.decode(frozen) match
            case Tel.Codec.Decoded.Failure(_) =>
              abort(Bintel.Error(Bintel.Error.Reason.CodecDecodeFailed))

            case Tel.Codec.Decoded.Value(decoded) =>
              // OPTIONAL §21.7 canonicality verification: the bytes must
              // be the re-encoding of their decoded text (also failing
              // when the encoder rejects its own decoder's output).
              if checkCanonical then codec.encode(decoded) match
                case Tel.Codec.Encoded.Bytes(re) =>
                  if !bytesEqual(re, frozen)
                  then abort(Bintel.Error(Bintel.Error.Reason.CodecNoncanonical))

                case Tel.Codec.Encoded.Invalid(_) =>
                  abort(Bintel.Error(Bintel.Error.Reason.CodecNoncanonical))

              decoded

        . or:
            try Text(new String(bytes, StandardCharsets.UTF_8))
            catch case _: Exception => abort(Bintel.Error(Bintel.Error.Reason.BadUtf8))

        Tel.Element.Value(kidx.toInt, s, text)

      case Tels.Flag =>
        Tel.Element.Node(kidx.toInt, Tels.Flag, Array.empty)

      case _: Tels.Reference =>
        abort(Bintel.Error(Bintel.Error.Reason.ReferenceUnresolved))

  // §10: a varint that is truncated — including one with no bytes
  // available at all — wider than 64 bits, or overlong is B02.
  private def readVarint(cursor: Cursor): Long raises Bintel.Error =
    import errorDiagnostics.emptyDiagnostics

    mitigate:
      case _: Varint.Error => Bintel.Error(Bintel.Error.Reason.VarintError)

    . protect:
        val decoded = Varint.decode(cursor.data, cursor.offset)
        cursor.offset = decoded.next
        decoded.value

  // Flatten a Struct's members into a parallel keyword/type sequence
  // per §5. Fields contribute one entry; SelectRefs contribute one
  // entry per variant in the referenced SelectDefinition. Excludes
  // contribute none. A SelectRef naming no SelectDefinition — because
  // the name is unbound, or bound to a record or scalar — is B10 (§10):
  // silently contributing no slots would shift every later keyword index
  // and decode the document against the wrong members.
  private def flattenKeywords(struct: Tels.Struct, schema: Tels)
  :   Array[(Text, Tels.Type)]^{} raises Bintel.Error =

    flatten(struct, schema, strict = true)

  // The same flattening for trees `decode` produced, whose SelectRefs it
  // has already resolved; skips the unresolvable ones instead of raising.
  private def flattenKeywordsLenient(struct: Tels.Struct, schema: Tels)
  :   Array[(Text, Tels.Type)]^{} =

    import errorDiagnostics.emptyDiagnostics
    unsafely(flatten(struct, schema, strict = false))

  private def flatten(struct: Tels.Struct, schema: Tels, strict: Boolean)
  :   Array[(Text, Tels.Type)]^{} raises Bintel.Error =

    val buf = scala.collection.mutable.ArrayBuffer.empty[(Text, Tels.Type)]
    var i = 0

    while i < struct.members.length do
      struct.members.readable(i) match
        case f: Tels.Field =>
          buf += ((f.keyword, f.fieldType))

        case s: Tels.SelectRef =>
          schema.selects.seek(_.name == s.reference) match
            case selectDef: Tels.SelectDefinition =>
              var v = 0

              while v < selectDef.variants.length do
                val variant = selectDef.variants.readable(v)
                buf += ((variant.keyword, variant.variantType))
                v += 1

            case _ =>
              if strict then abort(Bintel.Error(Bintel.Error.Reason.ReferenceUnresolved))

        case _: Tels.Exclude =>
          ()

      i += 1

    Array.from(buf)

  // Reconstruct a presentation `Tel` from a schema-typed element tree — the inverse of
  // `Tel.Type.assign`. Each element's keyword comes from its parent struct's flattened
  // keyword sequence (looked up by the index BinTEL stored), so a decoded element can be
  // re-decoded to a typed value through `Tel.Decodable`.
  private def present(element: Tel.Element, schema: Tels): Tel = element match
    case Tel.Element.Node(_, struct: Tels.Struct, children) =>
      val flat = flattenKeywordsLenient(struct, schema)
      val blk = blocks(children.remap(presentCompound(_, flat, schema)))

      Tel.make(Tel.Compound("", Array.empty, Unset, blk))

    case _ =>
      Tel.empty

  private def presentCompound
    ( element: Tel.Element, flat: Array[(Text, Tels.Type)]^{}, schema: Tels )
  :   Tel.Compound =

    element match
      case Tel.Element.Value(kidx, _, text) =>
        Tel.Compound(flat.readable(kidx)._1, Array(Tel.Atom.Inline(text, 1)), Unset, Array.empty)

      case Tel.Element.Node(kidx, struct: Tels.Struct, children) =>
        val keyword   = kidx.let(flat.readable(_)._1).or(Text(""))
        val childFlat = flattenKeywordsLenient(struct, schema)

        Tel.Compound
          ( keyword,
            Array.empty,
            Unset,
            blocks(children.remap(presentCompound(_, childFlat, schema))) )

      case Tel.Element.Node(kidx, _, _) =>
        Tel.Compound(kidx.let(flat.readable(_)._1).or(Text("")), Array.empty, Unset, Array.empty)

  private def blocks(compounds: Array[Tel.Compound]^{}): Array[Tel.Block]^{} =
    if compounds.nil then Array.empty
    else Array(Tel.Block(Array.empty, Unset, compounds, 0))

  object Parsable:
    // The base of generated parsers: generated code is capture-erased, so
    // the body receives the reader as a neutral carrier, and the capability
    // is asserted here at the rim — the audited point — like the reader's
    // own accessors. (A generated override of `parse` itself would narrow
    // the trait's `BintelReader^` parameter to a pure type, which capture
    // checking rejects at the instantiation site.)
    abstract class Direct[value] extends Bintel.Parsable:
      type Self = value

      protected def parseCarrier(reader: AnyRef): value

      def parse(reader: BintelReader^): value = parseCarrier(reader.asInstanceOf[AnyRef])

  // The direct-parsing counterpart of `Bintel.read`: consumes elements
  // straight off the body bytes through a `BintelReader`, so neither the
  // `Tel.Element` tree, nor its `Tel` presentation, nor the text-format
  // decode that follows is materialized. `Parsable` is the opt-in surface:
  // `Bintel.Inlinable.parsable` generates instances whose keyword-index
  // dispatch is compiled from the value's statically-derived schema.
  // BinTEL has no per-subtree bridge back to the AST path, so shapes the
  // generator does not support stay on `Bintel.read`.
  trait Parsable extends prepositional.Typeclass:
    def parse(reader: BintelReader^): Self

  // Decode BinTEL body bytes directly to a typed value through the value's
  // `Bintel.Parsable`. Trailing bytes are rejected exactly as `decode`.
  def parse[value](data: Data)
    ( using parsable: (value is Bintel.Parsable)^ )
    ( using tactic: Tactic[Bintel.Error] )
  :   value =

    val parser = BintelParser(data)
    val result = parsable.parse(BintelReader(parser, tactic))
    if parser.offset != data.length then abort(Bintel.Error(Bintel.Error.Reason.TrailingBytes))
    result

  // Codec-aware direct parsing: leaves whose type declares a §21.7
  // encoding (the `Tel.Encoded` marker) read codec bytes through the
  // binding, with B13/B14 and, under `checkCanonical`, B15 — matching
  // the AST decoder's rules exactly.
  def parse[value](data: Data, codecs: Tel.Codec.Bindings, checkCanonical: Boolean = false)
    ( using parsable: (value is Bintel.Parsable)^ )
    ( using tactic: Tactic[Bintel.Error] )
  :   value =

    val parser = BintelParser(data, Tel.Codec.Resolver(codecs), checkCanonical)
    val result = parsable.parse(BintelReader(parser, tactic))
    if parser.offset != data.length then abort(Bintel.Error(Bintel.Error.Reason.TrailingBytes))
    result

  // Decode BinTEL body bytes to a typed value, deriving the schema from the value's type
  // — the inverse of `value.bintel`.
  def read[value: Tel.Decodable](data: Data)
    ( using value is TelSchematic over Tels.Type )
    ( using Tactic[Bintel.Error], Tactic[Tel.Error] )
  :   value =

    val schema = Tels.tels[value](Text("root"))
    present(decode(data, schema), schema).as[value]

  // As above, decoding scalars whose derived schema declares a §21.7
  // encoding through the codec binding.
  def read[value: Tel.Decodable]
    ( data: Data, codecs: Tel.Codec.Bindings, checkCanonical: Boolean = false )
    ( using value is TelSchematic over Tels.Type )
    ( using Tactic[Bintel.Error], Tactic[Tel.Error] )
  :   value =

    val schema = Tels.tels[value](Text("root"))
    present(decode(data, schema, codecs, checkCanonical), schema).as[value]

  private def resolveType(t: Tels.Type, schema: Tels): Tels.Type = t match
    case Tels.Reference(name) =>
      schema.records.seek(_.name == name).lay:
        schema.scalars.seek(_.name == name).lay(t): sc =>
          Tels.Scalar(sc.validators, sc.encoding, sc.patterns)
      . apply: rec =>
        Tels.Struct(rec.members, rec.validators)

    case other => other

  private final class Cursor(val data: Data, @scala.caps.unsafe.untrackedCaptures var offset: Int)

  private def encodeRoot
    ( out: Scribe[Byte], element: Tel.Element, schema: Tels,
      codecs: Optional[Tel.Codec.Resolver] )
    ( using Tactic[Tel.Error], Tactic[Bintel.Error] )
  :   Unit =

    element match
      case Tel.Element.Node(_, parent: Tels.Struct, children) =>
        val ordered = canonicalOrder(children, parent, schema)
        writeVarint(out, ordered.length.toLong)
        var i = 0

        while i < ordered.length do
          encodeElement(out, ordered.readable(i), schema, codecs)
          i += 1

      case Tel.Element.Node(_, _, children) =>
        writeVarint(out, children.length.toLong)
        var i = 0
        while i < children.length do { encodeElement(out, children.readable(i), schema, codecs); i += 1 }

      case _: Tel.Element.Value =>
        writeVarint(out, 1L)
        encodeElement(out, element, schema, codecs)

  private def encodeElement
    ( out: Scribe[Byte], element: Tel.Element, schema: Tels,
      codecs: Optional[Tel.Codec.Resolver] )
    ( using Tactic[Tel.Error], Tactic[Bintel.Error] )
  :   Unit =

    element match
      case node: Tel.Element.Node   => encodeNode(out, node, schema, codecs)
      case value: Tel.Element.Value => encodeValue(out, value, codecs)

  private def encodeNode
    ( out: Scribe[Byte], node: Tel.Element.Node, schema: Tels,
      codecs: Optional[Tel.Codec.Resolver] )
    ( using Tactic[Tel.Error], Tactic[Bintel.Error] )
  :   Unit =

    val kidx = node.keywordIndex.or(0).toLong
    writeVarint(out, kidx)

    node.elementType match
      case parent: Tels.Struct =>
        val ordered = canonicalOrder(node.children, parent, schema)
        writeVarint(out, ordered.length.toLong)
        var i = 0

        while i < ordered.length do
          encodeElement(out, ordered.readable(i), schema, codecs)
          i += 1

      case Tels.Flag =>
        // Flag nodes carry no children and no length.
        ()

      case _: Tels.Scalar | _: Tels.Reference =>
        // Should not appear in a well-formed Tel.Element.Node after type
        // assignment — scalars are Tel.Element.Value and references are
        // resolved during assignment. Encode no further bytes.
        ()

  // §7.1 two-form scalar payload: UTF-8 text bytes for an unencoded
  // scalar, the bound codec's bytes for an encoded one. The framing —
  // keyword index, byte length, bytes — is identical in both forms.
  private def encodeValue
    ( out: Scribe[Byte], value: Tel.Element.Value,
      codecs: Optional[Tel.Codec.Resolver] )
    ( using Tactic[Tel.Error], Tactic[Bintel.Error] )
  :   Unit =

    import errorDiagnostics.emptyDiagnostics

    writeVarint(out, value.keywordIndex.toLong)

    val bytes = value.scalarType.encoding.let: name =>
      val codec = codecs.let(_(name))
      . or(abort(Bintel.Error(Bintel.Error.Reason.CodecUnresolved)))

      codec.encode(value.text) match
        case Tel.Codec.Encoded.Bytes(data) => data.asInstanceOf[scala.Array[Byte]]

        case Tel.Codec.Encoded.Invalid(_) =>
          abort(Tel.Error(Tel.Error.Reason.EncodingRejected))

    . or(value.text.s.getBytes(StandardCharsets.UTF_8).nn)

    writeVarint(out, bytes.length.toLong)
    out.append(Array.unsafeFrozen(bytes))

  private def writeVarint(out: Scribe[Byte], value: Long): Unit =
    var n = value

    while n >= 0x80L do
      out.append(((n & 0x7fL) | 0x80L).toByte)
      n >>>= 7

    out.append(n.toByte)

  // §7.2 canonical child order: emit elements member by member, in member
  // declaration order, preserving source order within a single member.
  // A `SelectRef` member spans one flat keyword index per variant; all of
  // its variant-filling children belong to the SAME member, so they must
  // stay in source order relative to each other rather than being sorted
  // by variant index. We therefore sort (stably) by the flat index at
  // which each child's member STARTS, not by the child's own flat index.
  // For a struct of only `Field` members this is identical to sorting by
  // flat index. Atom-derived elements precede compound-derived elements
  // because type assignment inserts them first, and the stable sort keeps
  // that order within a member.
  private def canonicalOrder(children: Array[Tel.Element]^{}, parent: Tels.Struct, schema: Tels)
  :   Array[Tel.Element]^{} raises Bintel.Error =

    if children.length <= 1 then children
    else
      val memberBase = memberBaseByFlatIndex(parent, schema)

      def keyOf(e: Tel.Element): Int =
        val flat = kidxOf(e)
        if flat >= 0 && flat < memberBase.length then memberBase.readable(flat) else flat

      val arr = new scala.Array[Tel.Element](children.length)
      var i = 0

      while i < children.length do
        arr(i) = children.readable(i)
        i += 1

      // java.util.Arrays.sort with a Comparator is stable — preserves
      // source order within equal-key groups.
      java.util.Arrays.sort
        ( arr.asInstanceOf[scala.Array[AnyRef]],
         (a: AnyRef, b: AnyRef) => Integer.compare(keyOf(a.asInstanceOf[Tel.Element]),
                                                    keyOf(b.asInstanceOf[Tel.Element])) )

      arr.asInstanceOf[Array[Tel.Element]^{}]

  // Maps each flat keyword index in `parent` to the flat index at which
  // its member begins. A `Field` occupies one slot (mapping to itself); a
  // `SelectRef` occupies one slot per variant of the referenced Select,
  // all mapping to the SelectRef's starting flat index; an `Exclude`
  // occupies none. An unresolvable SelectRef is B10, as in `flattenKeywords`.
  private def memberBaseByFlatIndex(parent: Tels.Struct, schema: Tels)
  :   Array[Int]^{} raises Bintel.Error =

    val bases = scala.collection.mutable.ArrayBuffer.empty[Int]
    var flat = 0
    var i = 0

    while i < parent.members.length do
      parent.members.readable(i) match
        case _: Tels.Field =>
          bases += flat
          flat += 1

        case s: Tels.SelectRef =>
          val width = schema.selects.seek(_.name == s.reference).let(_.variants.length).or:
            abort(Bintel.Error(Bintel.Error.Reason.ReferenceUnresolved))

          var j = 0
          while j < width do { bases += flat; j += 1 }
          flat += width

        case _: Tels.Exclude => ()

      i += 1

    Array.from(bases)

  private def kidxOf(element: Tel.Element): Int = element match
    case Tel.Element.Node(idx, _, _)  => idx.or(0)
    case Tel.Element.Value(idx, _, _) => idx

  // BintelError → Bintel.Error
  object Error:

    object Reason:
      given communicable: Reason is Communicable =
        case BadSignature        => m"the schema signature does not decode against the library"
        case BadKeywordIndex     => m"a keyword index exceeds the parent's flat-keyword count"
        case ValueTruncated      => m"a Scalar value's byte length extends beyond end of input"
        case BadUtf8             => m"a Scalar value's bytes are not valid UTF-8"
        case TrailingBytes       => m"a whole-document reader found bytes after the document"
        case UnexpectedEoi       => m"the decoder requested bytes beyond end of input"

        case ReferenceUnresolved =>
          m"a Reference or SelectRef in the schema does not resolve to a definition of its kind"

        case DeclaredLengthMismatch =>
          m"the declared document length disagrees with the structural extent of the document"

        case NestingLimitExceeded =>
          m"the document nests Structs more deeply than the decoder's limit"
        case VarintError         => m"a variable-length integer in the stream is invalid"

        case BadMagic =>
          m"""
            the magic number is missing or matches neither B2 C4 B5 BB (external mode) nor B2 C4 B5 BC
            (self-contained mode)
          """

        case BadSignatureLength =>
          m"the schema signature length is not a valid palimpsest length under any (H, k_i, k_r)"

        case EmbeddedSignatureMismatch =>
          m"""
            the signature recomputed from the embedded schema body does not equal the carried
            signature
          """

        case EmbeddedSchemaUndecodable =>
          m"the embedded schema body does not decode as a valid TEL document under tels"

        case CodecUnresolved =>
          m"a scalar's declared encoding is not resolved by the codec binding"

        case CodecDecodeFailed =>
          m"an encoded scalar's value bytes are rejected by the bound codec"

        case CodecNoncanonical =>
          m"an encoded scalar's value bytes are not the re-encoding of their decoded text"

    enum Reason(val number: Int) extends Clarification:
      case BadMagic            extends Reason(1)
      case VarintError          extends Reason(2)
      case BadSignatureLength  extends Reason(3)
      case BadSignature        extends Reason(4)
      case BadKeywordIndex     extends Reason(5)
      case ValueTruncated      extends Reason(6)
      case BadUtf8             extends Reason(7)
      case TrailingBytes       extends Reason(8)
      case UnexpectedEoi       extends Reason(9)
      case ReferenceUnresolved extends Reason(10)
      case EmbeddedSignatureMismatch extends Reason(11)
      case EmbeddedSchemaUndecodable extends Reason(12)
      case CodecUnresolved           extends Reason(13)
      case CodecDecodeFailed         extends Reason(14)
      case CodecNoncanonical         extends Reason(15)
      case DeclaredLengthMismatch    extends Reason(16)
      // Not a B-code: §11 places the decoder's resource limits outside the
      // B01–B16 taxonomy, since a decoder with a larger limit would accept
      // the same bytes.
      case NestingLimitExceeded      extends Reason(17)

  case class Error(reason: Bintel.Error.Reason)(using Diagnostics)
  extends fulminate.Error(609, reason.number)(m"the BinTEL stream is invalid because $reason")

