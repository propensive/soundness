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

import scala.language.unsafeNulls
import murmuration.*
import rudiments.seek

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
    ( using Tactic[Bintel.Error], Tactic[Tel.Error] )
  :   Data =

    fromElement(Tel.Type.assign(doc, axiom).asInstanceOf[Tel.Element.Node], axiom)

  // As `fromDocument`, but starting from an already type-assigned schema
  // root — used when recomputing the signature of an embedded schema body
  // decoded from a self-contained BinTEL document (§6.2, B11).
  def fromElement(root: Tel.Element.Node, axiom: Tels)
    ( using Tactic[Bintel.Error], Tactic[Tel.Error] )
  :   Data =

    val (baseHash, layerHashes) = componentsOf(root, axiom)
    encode(baseHash :: (layerHashes: List[Data]))

  // The component hashes a schema signature is built from: the base-schema hash `h₀`, together with
  // one layer hash `h_i` per `layer` compound in source order (zip with `Tels.layers` for their
  // names). `encode(baseHash :: layerHashes)` reproduces `fromDocument`; selecting a sublist of the
  // layer hashes and re-encoding — `encode(baseHash :: chosen)` — yields the palimpsest of the base
  // composed with just those layers.
  def componentHashes(doc: Tel, axiom: Tels)
    ( using Tactic[Bintel.Error], Tactic[Tel.Error] )
  :   (Data, List[Data]) =

    componentsOf(Tel.Type.assign(doc, axiom).asInstanceOf[Tel.Element.Node], axiom)

  private def componentsOf(root: Tel.Element.Node, axiom: Tels)
    ( using Tactic[Bintel.Error], Tactic[Tel.Error] )
  :   (Data, List[Data]) =

    // Resolve the flat keyword index of "layer" and the Layer
    // RecordDefinition's struct from the axiom. If either is missing
    // the axiom does not describe schemas-with-layers; we still
    // proceed by treating the whole document as the base schema.
    val layerIdx: Optional[Int] = layerKeywordIndex(axiom.document, axiom)

    val baseChildren = root.children.filter: child => keywordIndexOf(child) != layerIdx

    val baseElement = Tel.Element.Node(Unset, axiom.document, baseChildren)
    val baseHash    = Blake3.hashOf(baseElement.bintel(axiom), cadence.hashSize)

    val layerChildren = root.children.filter: child => keywordIndexOf(child) == layerIdx

    val layerStruct: Optional[Tels.Struct] =
      axiom.records.seek(_.name == Text("Layer")).let: rec =>
        Tels.Struct(rec.members, rec.validators)

    val layerHashes: List[Data] =
      layerStruct.let: ls =>
        val hashes = layerChildren.readable.toList.to(List).map: layer =>
          val layerChildren = layer.asInstanceOf[Tel.Element.Node].children
          val layerRoot     = Tel.Element.Node(Unset, ls, layerChildren)
          Blake3.hashOf(layerRoot.bintel(axiom), cadence.hashSize)

        (hashes: List[Data])

      .or(Nil)

    (baseHash, layerHashes)

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
      struct.members.readable(i) match
        case f: Tels.Field =>
          if f.keyword == Text("layer") then found = idx else idx += 1

        case s: Tels.SelectRef =>
          schema.selects.seek(_.name == s.reference).let: sd =>
            var v = 0

            while v < sd.variants.length && found < 0 do
              if sd.variants.readable(v).keyword == Text("layer") then found = idx + v
              v += 1

            if found < 0 then idx += sd.variants.length

        case _: Tels.Exclude => ()

      i += 1

    if found < 0 then Unset else found

  // Build a palimpsest from an ordered sequence of component hashes at
  // the BinTEL-pinned `cadence`. Every hash must be `cadence.hashSize`
  // (32) bytes long; an empty list, or any mis-sized hash, raises
  // `BadSignatureLength`.
  def encode(hashes: List[Data]): Data raises Bintel.Error =
    if hashes.nil then abort(Bintel.Error(Bintel.Error.Reason.BadSignatureLength))

    val it = hashes.stdlib.iterator
    var bad = false

    while it.hasNext && !bad do if it.next().length != cadence.hashSize then bad = true

    if bad then abort(Bintel.Error(Bintel.Error.Reason.BadSignatureLength))

    Palimpsest(Sequence.from(hashes.stdlib)).data

  // The number of component hashes a palimpsest signature encodes,
  // recovered from its trailing cadence byte (§4.2 of the palimpsest
  // spec). A byte length inconsistent with any valid cadence raises
  // `BadSignatureLength`.
  def componentCount(signature: Data): Int raises Bintel.Error =
    val total = signature.length
    if total < 2 then abort(Bintel.Error(Bintel.Error.Reason.BadSignatureLength))

    var xor = 0
    var i   = 0

    while i < total do
      xor = xor ^ (signature.readable(i) & 0xff)
      i += 1

    val cadence: Cadence = Cadence.unpack(xor.toByte).or:
      abort(Bintel.Error(Bintel.Error.Reason.BadSignatureLength))

    cadence.hashCount(total - 1).or:
      abort(Bintel.Error(Bintel.Error.Reason.BadSignatureLength))

  // Decode a palimpsest schema signature against a library of candidate
  // component hashes. Failure to reconstruct the ordered hash sequence
  // raises `BadSignature`.
  def decode(signature: Data, library: List[Data]): List[Data] raises Bintel.Error =
    val n = componentCount(signature)

    given Bibliography = Bibliography(library.stdlib)

    Palimpsest(signature, n).resolve.or(abort(Bintel.Error(Bintel.Error.Reason.BadSignature)))

  // §8.1 of the TEL spec: layer selections as decomposition hints for
  // library lookup — first attempt the decode over the candidate's base
  // plus only the layers matching the selected names, then fall back to
  // its full component library, since hints are advisory.
  def decodeHinted(signature: Data, base: Data, layers: List[(Text, Data)], selection: List[Text])
  :   Optional[List[Data]] =

    val hinted = layers.stdlib.filter { (name, _) => selection.stdlib.contains(name) }.map(_(1))
    val full = layers.stdlib.map(_(1))

    safely(decode(signature, base :: hinted.to(List)))
    . or(safely(decode(signature, base :: full.to(List))))

  private def same(a: Data, b: Data): Boolean =
    a.length == b.length && {
      var i  = 0
      var ok = true

      while ok && i < a.length do
        if a.readable(i) != b.readable(i) then ok = false
        i += 1

      ok
    }

  // §8.1 of the TEL spec: when a pragma signature follows layer
  // selections it is authoritative and MUST decompose into exactly
  // `1 + n` components — the schema's base hash followed by the `n`
  // selected layers' hashes, in order — with distinct failures for a
  // wrong component count, a wrong base, and a wrong or misordered
  // layer component. `schema` supplies the declared layer names, in
  // source order, matching `componentHashes`' layer hashes.
  def verifySelection(doc: Tel, schema: Tels, axiom: Tels, selection: List[Text], signature: Data)
    ( using Tactic[Bintel.Error], Tactic[Tel.Error], Tactic[Tels.Resolution.Error] )
  :   Unit =

    import Tels.Resolution.Error.Reason

    val (base, layerHashes) = componentHashes(doc, axiom)
    val names = schema.layers.readable.toList.map(_.name)
    val byName = names.zip(layerHashes.stdlib).toMap

    val chosen = selection.stdlib.map: name =>
      byName.getOrElse(name, abort(Tels.Resolution.Error(Reason.UnknownLayer(name))))

    val expectedCount = 1 + chosen.length
    val foundCount = componentCount(signature)

    if foundCount != expectedCount
    then abort(Tels.Resolution.Error(Reason.ComponentCount(expectedCount, foundCount)))

    val expected = encode(base :: chosen.to(List))

    if !same(expected, signature) then
      // Decompose the claimed signature over the schema's full
      // component library to name the diverging component.
      val reason = safely(decode(signature, base :: layerHashes)) match
        case decoded: List[Data] =>
          val components = decoded.stdlib

          if components.isEmpty || !same(components.head, base) then Reason.BaseMismatch
          else
            val tail = components.tail.toIndexedSeq
            val wanted = chosen.toIndexedSeq
            val names = selection.stdlib.toIndexedSeq
            var idx = 0
            var layerReason: Optional[Tels.Resolution.Error.Reason] = Unset

            while layerReason.absent && idx < tail.length && idx < wanted.length do
              if !same(tail(idx), wanted(idx)) then layerReason = Reason.LayerMismatch(names(idx))
              idx += 1

            layerReason.or(Reason.Unverified(
              Text("the signature does not match the base and selected layers")))

        case _ =>
          Reason.Unverified(
            Text("the signature does not decompose over the schema's component hashes"))

      abort(Tels.Resolution.Error(reason))
