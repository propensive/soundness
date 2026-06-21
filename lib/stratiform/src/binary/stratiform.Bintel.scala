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
┃    Soundness, version 0.54.0.                                                                    ┃
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
    Array[Byte](0xb2.toByte, 0xc4.toByte, 0xb5.toByte, 0xbb.toByte)
      .asInstanceOf[IArray[Byte]]

  // §6.2 self-contained magic number. In BASE-256 text these are the four
  // characters `β τ ε μ` — the trailing `μ` (for *monolithic*) distinguishes
  // self-contained mode from external mode's `βτελ`.
  val magicSelfContained: Data =
    Array[Byte](0xb2.toByte, 0xc4.toByte, 0xb5.toByte, 0xbc.toByte)
      .asInstanceOf[IArray[Byte]]

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
    out.toByteArray.asInstanceOf[IArray[Byte]]

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
  def frame(body: Data, signature: Data): Data raises BintelError =
    if !validSignatureLength(signature)
    then abort(BintelError(BintelError.Reason.BadSignatureLength))

    val out = new ByteArrayOutputStream(magic.length + 10 + signature.length + body.length)
    out.write(magic.asInstanceOf[Array[Byte]])
    val sigLen = new ByteArrayOutputStream(10)
    var n = signature.length.toLong

    while n >= 0x80L do
      sigLen.write(((n & 0x7fL) | 0x80L).toInt)
      n >>>= 7

    sigLen.write(n.toInt)
    out.write(sigLen.toByteArray)
    out.write(signature.asInstanceOf[Array[Byte]])
    out.write(body.asInstanceOf[Array[Byte]])
    out.toByteArray.asInstanceOf[IArray[Byte]]

  // §6 unframing. Parse a complete BinTEL byte sequence into its
  // signature bytes and body bytes. Validates the magic number (B01),
  // the signature length pattern (B03), and leaves trailing-byte (B08)
  // detection to the caller (typically `Bintel.decode` over the body).
  def unframe(data: Data): Framed raises BintelError =
    if data.length < magic.length
    then abort(BintelError(BintelError.Reason.BadMagic))

    var i = 0

    while i < magic.length do
      if data(i) != magic(i) then abort(BintelError(BintelError.Reason.BadMagic))
      i += 1

    val sigLenDecoded =
      import errorDiagnostics.emptyDiagnostics

      mitigate:
        case _: VarintError => BintelError(BintelError.Reason.VarintError)

      . protect(Varint.decode(data, magic.length))

    val sigLength = sigLenDecoded.value.toInt

    val sigStart = sigLenDecoded.next
    val sigEnd   = sigStart + sigLength

    if sigEnd > data.length then abort(BintelError(BintelError.Reason.UnexpectedEoi))

    val sigBytes = new Array[Byte](sigLength)
    System.arraycopy(data.asInstanceOf[Array[Byte]], sigStart, sigBytes, 0, sigLength)
    val bodyBytes = new Array[Byte](data.length - sigEnd)
    System.arraycopy(data.asInstanceOf[Array[Byte]], sigEnd, bodyBytes, 0, bodyBytes.length)

    val sig = sigBytes.asInstanceOf[IArray[Byte]]

    if !validSignatureLength(sig)
    then abort(BintelError(BintelError.Reason.BadSignatureLength))

    Framed(sig, bodyBytes.asInstanceOf[IArray[Byte]])

  // §6 + §7.8 — decode a complete BinTEL document (magic + signature
  // + body) into a `Document` carrying the signature bytes and the
  // semantic-model `Tel.Element` tree under `schema`. This layer does
  // not verify the signature against the schema (§8.2 palimpsest
  // decoding is a follow-up).
  def decodeDocument(data: Data, schema: Tels): Document raises BintelError =
    val framed = unframe(data)
    Document(framed.signature, decode(framed.body, schema))

  // §6.2 self-contained framing: magic_BC, signature (length varint +
  // bytes), embedded schema body (length varint + bytes), document root.
  // The signature length MUST be a valid palimpsest length; otherwise
  // raises `BadSignatureLength`.
  def frameSelfContained(signature: Data, schemaBody: Data, body: Data)
  :   Data raises BintelError =

    if !validSignatureLength(signature)
    then abort(BintelError(BintelError.Reason.BadSignatureLength))

    val out = new ByteArrayOutputStream(
        magicSelfContained.length + 20 + signature.length + schemaBody.length + body.length)

    out.write(magicSelfContained.asInstanceOf[Array[Byte]])
    writeVarint(out, signature.length.toLong)
    out.write(signature.asInstanceOf[Array[Byte]])
    writeVarint(out, schemaBody.length.toLong)
    out.write(schemaBody.asInstanceOf[Array[Byte]])
    out.write(body.asInstanceOf[Array[Byte]])
    out.toByteArray.asInstanceOf[IArray[Byte]]

  // §6.2 self-contained encoding of the TEL document `tel`, whose schema is given
  // as the TEL document `schemaDoc` (parseable under the tel-schema axiom). The
  // schema's signature and bintel body are embedded so that a receiver holding
  // only the axiom can decode the result with no external schema resolution.
  def selfContained(tel: Tel, schemaDoc: Tel): Data raises TelError raises BintelError =
    val axiom      = Tels.Axiom.tels
    val schema     = Tels.Layers.compose(Tels.Reconstructor.fromTel(schemaDoc))
    val signature  = SchemaSignature.fromDocument(schemaDoc, axiom)
    val schemaBody = schemaDoc.bintel(axiom)
    frameSelfContained(signature, schemaBody, tel.bintel(schema))

  // §6.2 decoder. Decode a complete self-contained BinTEL document. The
  // embedded schema body is decoded under the tel-schema axiom and used to
  // reconstruct the composed schema (B12 on any failure); its signature is
  // recomputed and verified byte-for-byte against the carried signature
  // (B11 on mismatch) before the document root is decoded under the
  // reconstructed schema.
  def decodeDocumentSelfContained(data: Data): Document raises BintelError =
    import errorDiagnostics.emptyDiagnostics

    if data.length < magicSelfContained.length
    then abort(BintelError(BintelError.Reason.BadMagic))

    var i = 0

    while i < magicSelfContained.length do
      if data(i) != magicSelfContained(i) then abort(BintelError(BintelError.Reason.BadMagic))
      i += 1

    def varint(at: Int) =
      mitigate:
        case _: VarintError => BintelError(BintelError.Reason.VarintError)

      . protect(Varint.decode(data, at))

    val sigLenD   = varint(magicSelfContained.length)
    val sigStart  = sigLenD.next
    val sigEnd    = sigStart + sigLenD.value.toInt
    if sigEnd > data.length then abort(BintelError(BintelError.Reason.UnexpectedEoi))
    val signature = data.slice(sigStart, sigEnd)

    if !validSignatureLength(signature)
    then abort(BintelError(BintelError.Reason.BadSignatureLength))

    val schLenD   = varint(sigEnd)
    val schStart  = schLenD.next
    val schEnd    = schStart + schLenD.value.toInt
    if schEnd > data.length then abort(BintelError(BintelError.Reason.UnexpectedEoi))
    val schemaBody = data.slice(schStart, schEnd)
    val docBody    = data.slice(schEnd, data.length)

    val axiom = Tels.Axiom.tels

    // Decode + reconstruct the embedded schema and recompute its signature;
    // any structural failure here is B12.
    val (composed, recomputed) =
      mitigate:
        case _: TelError    => BintelError(BintelError.Reason.EmbeddedSchemaUndecodable)
        case _: BintelError => BintelError(BintelError.Reason.EmbeddedSchemaUndecodable)

      . protect:
          val schemaRoot = decode(schemaBody, axiom).asInstanceOf[Tel.Element.Node]
          val baseTels   = Tels.SemanticReconstructor.fromElement(schemaRoot)
          val sig        = SchemaSignature.fromElement(schemaRoot, axiom)
          (Tels.Layers.compose(baseTels), sig)

    if !bytesEqual(recomputed, signature)
    then abort(BintelError(BintelError.Reason.EmbeddedSignatureMismatch))

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
  // mismatch raises `BintelError`.
  def decode(data: Data, schema: Tels): Tel.Element raises BintelError =
    val cursor = Cursor(data, 0)
    val root = decodeStructBody(cursor, schema.document, schema, keywordIndex = Unset)
    if cursor.offset != data.length then abort(BintelError(BintelError.Reason.TrailingBytes))
    root

  private def decodeStructBody
    ( cursor: Cursor, struct: Tels.Struct, schema: Tels,
      keywordIndex: Optional[Int] )
  :   Tel.Element raises BintelError =

    val flat = flattenKeywords(struct, schema)
    val childCount = readVarint(cursor)
    val children = new Array[Tel.Element](childCount.toInt)
    var i = 0

    while i < childCount.toInt do
      children(i) = decodeElement(cursor, flat, schema)
      i += 1

    Tel.Element.Node(keywordIndex, struct, children.asInstanceOf[IArray[Tel.Element]])

  private def decodeElement
    ( cursor: Cursor, flat: IArray[(Text, Tels.Type)], schema: Tels )
  :   Tel.Element raises BintelError =

    val kidx = readVarint(cursor)
    if kidx < 0 || kidx >= flat.length then abort(BintelError(BintelError.Reason.BadKeywordIndex))
    val (_, memberType) = flat(kidx.toInt)
    val resolved = resolveType(memberType, schema)

    resolved match
      case s: Tels.Struct =>
        decodeStructBody(cursor, s, schema, keywordIndex = kidx.toInt)

      case s: Tels.Scalar =>
        val len = readVarint(cursor)

        if cursor.offset + len > cursor.data.length
        then abort(BintelError(BintelError.Reason.ValueTruncated))

        val bytes = new Array[Byte](len.toInt)
        var j = 0

        while j < len.toInt do
          bytes(j) = cursor.data(cursor.offset + j)
          j += 1

        cursor.offset += len.toInt

        val text =
          try Text(new String(bytes, "UTF-8"))
          catch case _: Exception => abort(BintelError(BintelError.Reason.BadUtf8))

        Tel.Element.Value(kidx.toInt, s, text)

      case Tels.Flag =>
        Tel.Element.Node(kidx.toInt, Tels.Flag, IArray.empty)

      case _: Tels.Reference =>
        abort(BintelError(BintelError.Reason.ReferenceUnresolved))

  private def readVarint(cursor: Cursor): Long raises BintelError =
    import errorDiagnostics.emptyDiagnostics

    if cursor.offset >= cursor.data.length
    then abort(BintelError(BintelError.Reason.UnexpectedEoi))

    mitigate:
      case _: VarintError => BintelError(BintelError.Reason.VarintError)

    . protect:
        val decoded = Varint.decode(cursor.data, cursor.offset)
        cursor.offset = decoded.next
        decoded.value

  // Flatten a Struct's members into a parallel keyword/type sequence
  // per §5. Fields contribute one entry; SelectRefs contribute one
  // entry per variant in the referenced SelectDefinition. Excludes
  // contribute none.
  private def flattenKeywords(struct: Tels.Struct, schema: Tels)
  :   IArray[(Text, Tels.Type)] =

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

    IArray.from(buf)

  // Reconstruct a presentation `Tel` from a schema-typed element tree — the inverse of
  // `Tel.Type.assign`. Each element's keyword comes from its parent struct's flattened
  // keyword sequence (looked up by the index BinTEL stored), so a decoded element can be
  // re-decoded to a typed value through `Tel.Decodable`.
  private def present(element: Tel.Element, schema: Tels): Tel = element match
    case Tel.Element.Node(_, struct: Tels.Struct, children) =>
      val flat = flattenKeywords(struct, schema)
      val blk = blocks(children.map(presentCompound(_, flat, schema)))

      Tel.make(Tel.Compound("", IArray.empty, Unset, blk))

    case _ =>
      Tel.empty

  private def presentCompound
    ( element: Tel.Element, flat: IArray[(Text, Tels.Type)], schema: Tels )
  :   Tel.Compound =

    element match
      case Tel.Element.Value(kidx, _, text) =>
        Tel.Compound(flat(kidx)._1, IArray(Tel.Atom.Inline(text, 1)), Unset, IArray.empty)

      case Tel.Element.Node(kidx, struct: Tels.Struct, children) =>
        val keyword   = kidx.let(flat(_)._1).or(Text(""))
        val childFlat = flattenKeywords(struct, schema)

        Tel.Compound
          ( keyword,
            IArray.empty,
            Unset,
            blocks(children.map(presentCompound(_, childFlat, schema))) )

      case Tel.Element.Node(kidx, _, _) =>
        Tel.Compound(kidx.let(flat(_)._1).or(Text("")), IArray.empty, Unset, IArray.empty)

  private def blocks(compounds: IArray[Tel.Compound]): IArray[Tel.Block] =
    if compounds.nil then IArray.empty
    else IArray(Tel.Block(IArray.empty, Unset, compounds, 0))

  // Decode BinTEL body bytes to a typed value, deriving the schema from the value's type
  // — the inverse of `value.bintel`.
  def read[value: Tel.Decodable](data: Data)(using value is TelSchematic over Tels.Type)
  :   value raises BintelError raises TelError =

    val schema = Tels.tels[value](Text("root"))
    present(decode(data, schema), schema).as[value]

  private def resolveType(t: Tels.Type, schema: Tels): Tels.Type = t match
    case Tels.Reference(name) =>
      schema.records.find(_.name == name) match
        case Some(rec) => Tels.Struct(rec.members, rec.validators)

        case None =>
          schema.scalars.find(_.name == name) match
            case Some(sc) => Tels.Scalar(sc.validators)
            case None     => t

    case other => other

  private final class Cursor(val data: Data, var offset: Int)

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
    val bytes = value.text.s.getBytes("UTF-8")
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
  private def canonicalOrder(children: IArray[Tel.Element], parent: Tels.Struct, schema: Tels)
  :   IArray[Tel.Element] =

    if children.length <= 1 then children
    else
      val memberBase = memberBaseByFlatIndex(parent, schema)

      def keyOf(e: Tel.Element): Int =
        val flat = kidxOf(e)
        if flat >= 0 && flat < memberBase.length then memberBase(flat) else flat

      val arr = new Array[Tel.Element](children.length)
      var i = 0

      while i < children.length do
        arr(i) = children(i)
        i += 1

      // java.util.Arrays.sort with a Comparator is stable — preserves
      // source order within equal-key groups.
      java.util.Arrays.sort
        ( arr.asInstanceOf[Array[AnyRef]],
         (a: AnyRef, b: AnyRef) => Integer.compare(keyOf(a.asInstanceOf[Tel.Element]),
                                                    keyOf(b.asInstanceOf[Tel.Element])) )

      arr.asInstanceOf[IArray[Tel.Element]]

  // Maps each flat keyword index in `parent` to the flat index at which
  // its member begins. A `Field` occupies one slot (mapping to itself); a
  // `SelectRef` occupies one slot per variant of the referenced Select,
  // all mapping to the SelectRef's starting flat index; an `Exclude`
  // occupies none.
  private def memberBaseByFlatIndex(parent: Tels.Struct, schema: Tels): IArray[Int] =
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

    IArray.from(bases)

  private def kidxOf(element: Tel.Element): Int = element match
    case Tel.Element.Node(idx, _, _)  => idx.or(0)
    case Tel.Element.Value(idx, _, _) => idx
