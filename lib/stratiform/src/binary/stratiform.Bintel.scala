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

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import proscenium.compat.*

import scala.language.unsafeNulls

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import prepositional.*
import ulysses.*
import vacuous.*

object Bintel:

  // §6 magic number: the 4 bytes that prefix every BinTEL document.
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

  // The result of unframing a complete §6 file: the carried schema
  // signature bytes and the document-root body bytes.
  case class Framed(signature: Data, body: Data)

  // A fully decoded BinTEL document: the carried schema signature
  // bytes and the recovered semantic-model `Tel.Element` root.
  case class Document(signature: Data, root: Tel.Element)

  // Encode a `Tel.Element` tree to its BinTEL body bytes. The element is
  // expected to be the document root (a Node with `keywordIndex = Unset`
  // and `elementType = Tels.Struct`), as produced by `Tel.Type.assign`.
  def encode(element: Tel.Element, schema: Tels): Data =
    val out = new ByteArrayOutputStream
    encodeRoot(out, element, schema)
    out.toByteArray.asInstanceOf[Array[Byte]^{}]

  // Is `signature` a syntactically-valid palimpsest? Recovers the cadence
  // byte from the XOR-fold of every byte and checks the byte length is
  // consistent with the recovered `(H, k_i, k_r)`. Returns true iff so.
  private def validSignatureLength(signature: Data): Boolean =
    val total = signature.length

    if total < 2 then false else
      var xor = 0
      var i   = 0

      while i < total do
        xor = xor ^ (signature(i) & 0xff)
        i += 1

      Cadence.unpack(xor.toByte).let(_.hashCount(total - 1)).present

  // §6 framing. Wrap a body byte sequence with the magic number, the
  // signature length (varint), and the signature bytes. The signature
  // length MUST be a valid palimpsest length under some `(H, k_i, k_r)`,
  // recovered from the trailing cadence byte (§8.2 of bintel.md, §4.2
  // of palimpsest.md); otherwise raises `BadSignatureLength`.
  def frame(body: Data, signature: Data): Data raises Bintel.Error =
    if !validSignatureLength(signature)
    then abort(Bintel.Error(Bintel.Error.Reason.BadSignatureLength))

    val out = new ByteArrayOutputStream(magic.length + 10 + signature.length + body.length)
    out.write(magic.asInstanceOf[scala.Array[Byte]])
    val sigLen = new ByteArrayOutputStream(10)
    var n = signature.length.toLong

    while n >= 0x80L do
      sigLen.write(((n & 0x7fL) | 0x80L).toInt)
      n >>>= 7

    sigLen.write(n.toInt)
    out.write(sigLen.toByteArray)
    out.write(signature.asInstanceOf[scala.Array[Byte]])
    out.write(body.asInstanceOf[scala.Array[Byte]])
    out.toByteArray.asInstanceOf[Array[Byte]^{}]

  // §6 unframing. Parse a complete BinTEL byte sequence into its
  // signature bytes and body bytes. Validates the magic number (B01),
  // the signature length pattern (B03), and leaves trailing-byte (B08)
  // detection to the caller (typically `Bintel.decode` over the body).
  def unframe(data: Data): Framed raises Bintel.Error =
    if data.length < magic.length then abort(Bintel.Error(Bintel.Error.Reason.BadMagic))

    var i = 0

    while i < magic.length do
      if data(i) != magic(i) then abort(Bintel.Error(Bintel.Error.Reason.BadMagic))
      i += 1

    val sigLenDecoded =
      import errorDiagnostics.emptyDiagnostics

      mitigate:
        case _: Varint.Error => Bintel.Error(Bintel.Error.Reason.VarintError)

      . protect(Varint.decode(data, magic.length))

    val sigLength = sigLenDecoded.value.toInt

    val sigStart = sigLenDecoded.next
    val sigEnd   = sigStart + sigLength

    if sigEnd > data.length then abort(Bintel.Error(Bintel.Error.Reason.UnexpectedEoi))

    val sigBytes = new scala.Array[Byte](sigLength)
    System.arraycopy(data.asInstanceOf[scala.Array[Byte]], sigStart, sigBytes, 0, sigLength)
    val bodyBytes = new scala.Array[Byte](data.length - sigEnd)
    System.arraycopy(data.asInstanceOf[scala.Array[Byte]], sigEnd, bodyBytes, 0, bodyBytes.length)

    val sig = sigBytes.asInstanceOf[Array[Byte]^{}]

    if !validSignatureLength(sig) then abort(Bintel.Error(Bintel.Error.Reason.BadSignatureLength))

    Framed(sig, bodyBytes.asInstanceOf[Array[Byte]^{}])

  // §6 + §7.8 — decode a complete BinTEL document (magic + signature
  // + body) into a `Document` carrying the signature bytes and the
  // semantic-model `Tel.Element` tree under `schema`. This layer does
  // not verify the signature against the schema (§8.2 palimpsest
  // decoding is a follow-up).
  def decodeDocument(data: Data, schema: Tels): Document raises Bintel.Error =
    val framed = unframe(data)
    Document(framed.signature, decode(framed.body, schema))

  // §6.2 self-contained framing: magic_BC, signature (length varint +
  // bytes), embedded schema body (length varint + bytes), document root.
  // The signature length MUST be a valid palimpsest length; otherwise
  // raises `BadSignatureLength`.
  def frameSelfContained(signature: Data, schemaBody: Data, body: Data)
  :   Data raises Bintel.Error =

    if !validSignatureLength(signature)
    then abort(Bintel.Error(Bintel.Error.Reason.BadSignatureLength))

    val out = new ByteArrayOutputStream(
        magicSelfContained.length + 20 + signature.length + schemaBody.length + body.length)

    out.write(magicSelfContained.asInstanceOf[scala.Array[Byte]])
    writeVarint(out, signature.length.toLong)
    out.write(signature.asInstanceOf[scala.Array[Byte]])
    writeVarint(out, schemaBody.length.toLong)
    out.write(schemaBody.asInstanceOf[scala.Array[Byte]])
    out.write(body.asInstanceOf[scala.Array[Byte]])
    out.toByteArray.asInstanceOf[Array[Byte]^{}]

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

  // §6.2 decoder. Decode a complete self-contained BinTEL document. The
  // embedded schema body is decoded under the tels axiom and used to
  // reconstruct the composed schema (B12 on any failure); its signature is
  // recomputed and verified byte-for-byte against the carried signature
  // (B11 on mismatch) before the document root is decoded under the
  // reconstructed schema.
  def decodeDocumentSelfContained(data: Data): Document raises Bintel.Error =
    import errorDiagnostics.emptyDiagnostics

    if data.length < magicSelfContained.length then abort(Bintel.Error(Bintel.Error.Reason.BadMagic))

    var i = 0

    while i < magicSelfContained.length do
      if data(i) != magicSelfContained(i) then abort(Bintel.Error(Bintel.Error.Reason.BadMagic))
      i += 1

    def varint(at: Int) =
      mitigate:
        case _: Varint.Error => Bintel.Error(Bintel.Error.Reason.VarintError)

      . protect(Varint.decode(data, at))

    val sigLenD   = varint(magicSelfContained.length)
    val sigStart  = sigLenD.next
    val sigEnd    = sigStart + sigLenD.value.toInt
    if sigEnd > data.length then abort(Bintel.Error(Bintel.Error.Reason.UnexpectedEoi))
    val signature = data.slice(sigStart, sigEnd)

    if !validSignatureLength(signature)
    then abort(Bintel.Error(Bintel.Error.Reason.BadSignatureLength))

    val schLenD   = varint(sigEnd)
    val schStart  = schLenD.next
    val schEnd    = schStart + schLenD.value.toInt
    if schEnd > data.length then abort(Bintel.Error(Bintel.Error.Reason.UnexpectedEoi))
    val schemaBody = data.slice(schStart, schEnd)
    val docBody    = data.slice(schEnd, data.length)

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

    Document(signature, decode(docBody, composed))

  private def bytesEqual(a: Data, b: Data): Boolean =
    a.length == b.length && {
      var i = 0
      var equal = true

      while i < a.length && equal do
        if a(i) != b(i) then equal = false
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
    val cursor = Cursor(data, 0)
    val root = decodeStructBody(cursor, schema.document, schema, keywordIndex = Unset)
    if cursor.offset != data.length then abort(Bintel.Error(Bintel.Error.Reason.TrailingBytes))
    root

  private def decodeStructBody
    ( cursor: Cursor, struct: Tels.Struct, schema: Tels,
      keywordIndex: Optional[Int] )
  :   Tel.Element raises Bintel.Error =

    val flat = flattenKeywords(struct, schema)
    val childCount = readVarint(cursor)
    val children = new scala.Array[Tel.Element](childCount.toInt)
    var i = 0

    while i < childCount.toInt do
      children(i) = decodeElement(cursor, flat, schema)
      i += 1

    Tel.Element.Node(keywordIndex, struct, children.asInstanceOf[Array[Tel.Element]^{}])

  private def decodeElement
    ( cursor: Cursor, flat: Array[(Text, Tels.Type)]^{}, schema: Tels )
  :   Tel.Element raises Bintel.Error =

    val kidx = readVarint(cursor)
    if kidx < 0 || kidx >= flat.length then abort(Bintel.Error(Bintel.Error.Reason.BadKeywordIndex))
    val (_, memberType) = flat(kidx.toInt)
    val resolved = resolveType(memberType, schema)

    resolved match
      case s: Tels.Struct =>
        decodeStructBody(cursor, s, schema, keywordIndex = kidx.toInt)

      case s: Tels.Scalar =>
        val len = readVarint(cursor)

        if cursor.offset + len > cursor.data.length
        then abort(Bintel.Error(Bintel.Error.Reason.ValueTruncated))

        val bytes = new scala.Array[Byte](len.toInt)
        var j = 0

        while j < len.toInt do
          bytes(j) = cursor.data(cursor.offset + j)
          j += 1

        cursor.offset += len.toInt

        val text =
          try Text(new String(bytes, StandardCharsets.UTF_8))
          catch case _: Exception => abort(Bintel.Error(Bintel.Error.Reason.BadUtf8))

        Tel.Element.Value(kidx.toInt, s, text)

      case Tels.Flag =>
        Tel.Element.Node(kidx.toInt, Tels.Flag, Array.empty)

      case _: Tels.Reference =>
        abort(Bintel.Error(Bintel.Error.Reason.ReferenceUnresolved))

  private def readVarint(cursor: Cursor): Long raises Bintel.Error =
    import errorDiagnostics.emptyDiagnostics

    if cursor.offset >= cursor.data.length then abort(Bintel.Error(Bintel.Error.Reason.UnexpectedEoi))

    mitigate:
      case _: Varint.Error => Bintel.Error(Bintel.Error.Reason.VarintError)

    . protect:
        val decoded = Varint.decode(cursor.data, cursor.offset)
        cursor.offset = decoded.next
        decoded.value

  // Flatten a Struct's members into a parallel keyword/type sequence
  // per §5. Fields contribute one entry; SelectRefs contribute one
  // entry per variant in the referenced SelectDefinition. Excludes
  // contribute none.
  private def flattenKeywords(struct: Tels.Struct, schema: Tels)
  :   Array[(Text, Tels.Type)]^{} =

    val buf = scala.collection.mutable.ArrayBuffer.empty[(Text, Tels.Type)]
    var i = 0

    while i < struct.members.length do
      struct.members(i) match
        case f: Tels.Field =>
          buf += ((f.keyword, f.fieldType))

        case s: Tels.SelectRef =>
          schema.selects.find(_.name == s.reference).foreach: selectDef =>
            var v = 0

            while v < selectDef.variants.length do
              val variant = selectDef.variants(v)
              buf += ((variant.keyword, variant.variantType))
              v += 1

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
      val flat = flattenKeywords(struct, schema)
      val blk = blocks(children.map(presentCompound(_, flat, schema)))

      Tel.make(Tel.Compound("", Array.empty, Unset, blk))

    case _ =>
      Tel.empty

  private def presentCompound
    ( element: Tel.Element, flat: Array[(Text, Tels.Type)]^{}, schema: Tels )
  :   Tel.Compound =

    element match
      case Tel.Element.Value(kidx, _, text) =>
        Tel.Compound(flat(kidx)._1, Array.of(Tel.Atom.Inline(text, 1)), Unset, Array.empty)

      case Tel.Element.Node(kidx, struct: Tels.Struct, children) =>
        val keyword   = kidx.let(flat(_)._1).or(Text(""))
        val childFlat = flattenKeywords(struct, schema)

        Tel.Compound
          ( keyword,
            Array.empty,
            Unset,
            blocks(children.map(presentCompound(_, childFlat, schema))) )

      case Tel.Element.Node(kidx, _, _) =>
        Tel.Compound(kidx.let(flat(_)._1).or(Text("")), Array.empty, Unset, Array.empty)

  private def blocks(compounds: Array[Tel.Compound]^{}): Array[Tel.Block]^{} =
    if compounds.nil then Array.empty
    else Array.of(Tel.Block(Array.empty, Unset, compounds, 0))

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

  // Decode BinTEL body bytes to a typed value, deriving the schema from the value's type
  // — the inverse of `value.bintel`.
  def read[value: Tel.Decodable](data: Data)
    ( using value is TelSchematic over Tels.Type )
    ( using Tactic[Bintel.Error], Tactic[Tel.Error] )
  :   value =

    val schema = Tels.tels[value](Text("root"))
    present(decode(data, schema), schema).as[value]

  private def resolveType(t: Tels.Type, schema: Tels): Tels.Type = t match
    case Tels.Reference(name) =>
      schema.records.find(_.name == name) match
        case Some(rec) => Tels.Struct(rec.members, rec.validators)

        case None =>
          schema.scalars.find(_.name == name) match
            case Some(sc) => Tels.Scalar(sc.validators, sc.encoding)
            case None     => t

    case other => other

  private final class Cursor(val data: Data, @scala.caps.unsafe.untrackedCaptures var offset: Int)

  private def encodeRoot(out: ByteArrayOutputStream, element: Tel.Element, schema: Tels): Unit =
    element match
      case Tel.Element.Node(_, parent: Tels.Struct, children) =>
        val ordered = canonicalOrder(children, parent, schema)
        writeVarint(out, ordered.length.toLong)
        var i = 0

        while i < ordered.length do
          encodeElement(out, ordered(i), schema)
          i += 1

      case Tel.Element.Node(_, _, children) =>
        writeVarint(out, children.length.toLong)
        var i = 0
        while i < children.length do { encodeElement(out, children(i), schema); i += 1 }

      case _: Tel.Element.Value =>
        writeVarint(out, 1L)
        encodeElement(out, element, schema)

  private def encodeElement(out: ByteArrayOutputStream, element: Tel.Element, schema: Tels)
  :   Unit =

    element match
      case node: Tel.Element.Node   => encodeNode(out, node, schema)
      case value: Tel.Element.Value => encodeValue(out, value)

  private def encodeNode(out: ByteArrayOutputStream, node: Tel.Element.Node, schema: Tels): Unit =
    val kidx = node.keywordIndex.or(0).toLong
    writeVarint(out, kidx)

    node.elementType match
      case parent: Tels.Struct =>
        val ordered = canonicalOrder(node.children, parent, schema)
        writeVarint(out, ordered.length.toLong)
        var i = 0

        while i < ordered.length do
          encodeElement(out, ordered(i), schema)
          i += 1

      case Tels.Flag =>
        // Flag nodes carry no children and no length.
        ()

      case _: Tels.Scalar | _: Tels.Reference =>
        // Should not appear in a well-formed Tel.Element.Node after type
        // assignment — scalars are Tel.Element.Value and references are
        // resolved during assignment. Encode no further bytes.
        ()

  private def encodeValue(out: ByteArrayOutputStream, value: Tel.Element.Value): Unit =
    writeVarint(out, value.keywordIndex.toLong)
    val bytes = value.text.s.getBytes(StandardCharsets.UTF_8)
    writeVarint(out, bytes.length.toLong)
    out.write(bytes)

  private def writeVarint(out: ByteArrayOutputStream, value: Long): Unit =
    var n = value

    while n >= 0x80L do
      out.write(((n & 0x7fL) | 0x80L).toInt)
      n >>>= 7

    out.write(n.toInt)

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
  :   Array[Tel.Element]^{} =

    if children.length <= 1 then children
    else
      val memberBase = memberBaseByFlatIndex(parent, schema)

      def keyOf(e: Tel.Element): Int =
        val flat = kidxOf(e)
        if flat >= 0 && flat < memberBase.length then memberBase(flat) else flat

      val arr = new scala.Array[Tel.Element](children.length)
      var i = 0

      while i < children.length do
        arr(i) = children(i)
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
  // occupies none.
  private def memberBaseByFlatIndex(parent: Tels.Struct, schema: Tels): Array[Int]^{} =
    val bases = scala.collection.mutable.ArrayBuffer.empty[Int]
    var flat = 0
    var i = 0

    while i < parent.members.length do
      parent.members(i) match
        case _: Tels.Field =>
          bases += flat
          flat += 1

        case s: Tels.SelectRef =>
          val width = schema.selects.find(_.name == s.reference) match
            case Some(sd) => sd.variants.length
            case None     => 0

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
        case TrailingBytes       => m"the document root completed with input bytes remaining"
        case UnexpectedEoi       => m"the decoder requested bytes beyond end of input"
        case ReferenceUnresolved => m"a Reference type in the schema does not resolve"
        case Varint.Error         => m"a variable-length integer in the stream is invalid"

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

  case class Error(reason: Bintel.Error.Reason)(using Diagnostics)
  extends fulminate.Error(609, reason.number)(m"the BinTEL stream is invalid because $reason")

