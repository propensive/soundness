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
package stratiform

import scala.language.unsafeNulls

import anticipation.*
import contingency.*
import denominative.*
import gastronomy.*
import ulysses.*
import vacuous.*

// §8 of the BinTEL spec — schema-signature construction as a palimpsest
// of BLAKE3 component hashes at the BinTEL-pinned parameters
// `(H, k_i, k_r) = (32, 4, 2)` (§8.2): a 32-byte BLAKE3-256 hash, a
// 4-byte initial cadence, and a 2-byte regular cadence, so an n=1
// signature is 33 bytes and each further layer adds 2 bytes. The spec
// forbids any other parameters here, so the cadence is hard-pinned
// rather than taken contextually. It is still carried in the trailing
// byte (value `0x79`), so decoders recover it without prior agreement.

object SchemaSignature:

  // The BinTEL-pinned cadence (§8.2). `Cadence.pack` of `(s, k_i − k_r,
  // k_r − 1) = (7, 2, 1)` is `0x79`.
  given cadence: Cadence = Cadence(initial = 4, regular = 2, hashSize = 32)

  // §8.1 construction. Given a schema document parsable under `axiom`
  // (typically `Tels.Axiom.tels`), compute the full schema signature
  // as the palimpsest of:
  //
  //   - h₀ — value hash of the base schema (the document with all
  //     `layer` compounds removed), encoded against `axiom.document`.
  //   - h_i — value hash of each `layer` compound in source order,
  //     where each layer's children are encoded as a virtual root
  //     under the `Layer` Definition's keyword order.
  //
  // The resulting palimpsest length is `cadence.totalLength(n)` bytes,
  // suitable for use as the schema signature in a §6 BinTEL document
  // header or as the textual schema identifier on a TEL pragma after
  // BASE-256 encoding.
  def fromDocument(doc: Tel, axiom: Tels)
  :   Data raises BintelError raises TelError =

    fromElement(Tel.Type.assign(doc, axiom).asInstanceOf[Tel.Element.Node], axiom)

  // As `fromDocument`, but starting from an already type-assigned schema
  // root — used when recomputing the signature of an embedded schema body
  // decoded from a self-contained BinTEL document (§6.2, B11).
  def fromElement(root: Tel.Element.Node, axiom: Tels)
  :   Data raises BintelError raises TelError =

    // Resolve the flat keyword index of "layer" and the Layer
    // RecordDefinition's struct from the axiom. If either is missing
    // the axiom does not describe schemas-with-layers; we still
    // proceed by treating the whole document as the base schema.
    val layerIdx: Optional[Int] = layerKeywordIndex(axiom.document, axiom)

    val baseChildren = root.children.filter: child =>
      keywordIndexOf(child) != layerIdx

    val baseElement = Tel.Element.Node(Unset, axiom.document, baseChildren)
    val baseHash    = Blake3.hashOf(baseElement.bintel(axiom), cadence.hashSize)

    val layerChildren = root.children.filter: child =>
      keywordIndexOf(child) == layerIdx

    val layerStruct: Optional[Tels.Struct] =
      axiom.records.find(_.name == Text("Layer")) match
        case Some(rec) => Tels.Struct(rec.members, rec.validators)
        case None      => Unset

    val layerHashes: List[Data] =
      layerStruct.let: ls =>
        layerChildren.toList.map: layer =>
          val layerChildren = layer.asInstanceOf[Tel.Element.Node].children
          val layerRoot     = Tel.Element.Node(Unset, ls, layerChildren)
          Blake3.hashOf(layerRoot.bintel(axiom), cadence.hashSize)

      .or(Nil)

    encode(baseHash :: layerHashes)

  private def keywordIndexOf(element: Tel.Element): Optional[Int] = element match
    case Tel.Element.Node(idx, _, _)  => idx
    case Tel.Element.Value(idx, _, _) => idx

  // Flat-keyword-index lookup for the `layer` keyword inside the
  // given struct, walking parent.members in declaration order and
  // expanding SelectRef variants per §5.
  private def layerKeywordIndex(struct: Tels.Struct, schema: Tels): Optional[Int] =
    var idx   = 0
    var i     = 0
    var found = -1

    while i < struct.members.length && found < 0 do
      struct.members(i) match
        case f: Tels.Field =>
          if f.keyword == Text("layer") then found = idx else idx += 1

        case s: Tels.SelectRef =>
          schema.selects.find(_.name == s.reference) match
            case Some(sd) =>
              var v = 0

              while v < sd.variants.length && found < 0 do
                if sd.variants(v).keyword == Text("layer") then found = idx + v
                v += 1

              if found < 0 then idx += sd.variants.length

            case None => ()

        case _: Tels.Exclude => ()

      i += 1

    if found < 0 then Unset else found

  // Build a palimpsest from an ordered sequence of component hashes at
  // the BinTEL-pinned `cadence`. Every hash must be `cadence.hashSize`
  // (32) bytes long; an empty list, or any mis-sized hash, raises
  // `BadSignatureLength`.
  def encode(hashes: List[Data]): Data raises BintelError =
    if hashes.nil then abort(BintelError(BintelError.Reason.BadSignatureLength))

    val it = hashes.iterator
    var bad = false

    while it.hasNext && !bad do
      if it.next().length != cadence.hashSize then bad = true

    if bad then abort(BintelError(BintelError.Reason.BadSignatureLength))

    Palimpsest(hashes.toIndexedSeq).data

  // Decode a palimpsest schema signature against a library of candidate
  // component hashes. The cadence is recovered from the trailing byte
  // (§4.2 of the palimpsest spec); a byte length inconsistent with any
  // valid cadence raises `BadSignatureLength`. Failure to reconstruct
  // the ordered hash sequence raises `BadSignature`.
  def decode(signature: Data, library: List[Data]): List[Data] raises BintelError =
    val total = signature.length
    if total < 2 then abort(BintelError(BintelError.Reason.BadSignatureLength))

    var xor = 0
    var i   = 0

    while i < total do
      xor = xor ^ (signature(i) & 0xff)
      i += 1

    val cadence: Cadence = Cadence.unpack(xor.toByte).or:
      abort(BintelError(BintelError.Reason.BadSignatureLength))

    val n: Int = cadence.hashCount(total - 1).or:
      abort(BintelError(BintelError.Reason.BadSignatureLength))

    given Bibliography = Bibliography(library)

    Palimpsest(signature, n).resolve.or(abort(BintelError(BintelError.Reason.BadSignature)))
