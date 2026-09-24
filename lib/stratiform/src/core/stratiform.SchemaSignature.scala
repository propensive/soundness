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
import rudiments.{bind, each, seek, segment, to}

import anticipation.*
import contingency.*
import denominative.*
import denominative.dysasymptotics.linearSize
import fulminate.*
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

    // `Data` is a *frozen* byte array (`Array[Byte]^{}`), and a generic combinator's
    // predicate parameter cannot carry that freeze — `hashes.all(…)` fails capture
    // checking — so the size scan runs over the stdlib view. `forall` short-circuits
    // at the first mis-sized hash, as the iterator loop it replaces did.
    if !hashes.stdlib.forall(_.length == cadence.hashSize)
    then abort(Bintel.Error(Bintel.Error.Reason.BadSignatureLength))

    Palimpsest(hashes.to[Sequence]).data

  // The number of component hashes a palimpsest signature encodes,
  // recovered from its trailing cadence byte (§5.2 of the palimpsest
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

    given Bibliography = Bibliography(library)

    Palimpsest(signature, n).resolve.or(abort(Bintel.Error(Bintel.Error.Reason.BadSignature)))

  // §8.1 of the TEL spec: layer selections as decomposition hints for
  // library lookup — first attempt the decode over the candidate's base
  // plus only the layers matching the selected names, then fall back to
  // its full component library, since hints are advisory.
  def decodeHinted(signature: Data, base: Data, layers: List[(Text, Data)], selection: List[Text])
  :   Optional[List[Data]] =

    val hinted: List[Data] = layers.filter { (name, _) => selection.has(name) }.map(_(1))
    val full: List[Data] = layers.map(_(1))

    safely(decode(signature, base :: hinted)).or(safely(decode(signature, base :: full)))


  // The value hash (§3) of a rendered schema element: the BLAKE3-256 digest of its
  // document-root encoding under `axiom`, as `componentsOf` hashes a parsed document's base and
  // layers. A `Tels.Renderer` element hashes identically to the document it renders.
  def hash(element: Tel.Element, axiom: Tels = Tels.Axiom.tels): Data raises Bintel.Error =
    Blake3.hashOf(element.bintel(axiom), cadence.hashSize)

  // The component hashes of a schema value — the base (the schema without its layers) and
  // each layer in declaration order — rendered through `Tels.Renderer`.
  def componentHashes(schema: Tels, axiom: Tels)
    ( using Tactic[Bintel.Error], Tactic[Tels.Renderer.Error] )
  :   (Data, List[Data]) =

    val base = hash(Tels.Renderer.element(schema.copy(layers = Array.empty), axiom), axiom)

    val layers = schema.layers.readable.toList.to(List).map: layer =>
      hash(Tels.Renderer.layer(layer, axiom), axiom)

    (base, layers)

  // The value hash of an atom (§20.3, BinTEL §8.1): its virtual single-atom layer, or for the
  // head, the definition-less schema.
  def atomHash(atom: Tels.Atoms.Atom, axiom: Tels = Tels.Axiom.tels)
    ( using Tactic[Bintel.Error], Tactic[Tels.Renderer.Error] )
  :   Data =

    hash(atom.element(axiom), axiom)

  object Lineage:
    // A layer or an atom of a lineage: its hash, its name (a layer's; an atom has none), and
    // the layer that composes it.
    case class Component(hash: Data, name: Optional[Text], layer: Tels.Layer):
      def text: Text = Base256.encode(hash)

    def apply(schema: Tels, axiom: Tels = Tels.Axiom.tels)
      ( using Tactic[Bintel.Error], Tactic[Tels.Renderer.Error] )
    :   Lineage =

      val base = hash(Tels.Renderer.element(schema.copy(layers = Array.empty), axiom), axiom)

      val layers = schema.layers.readable.toList.to(List).map: layer =>
        Component(hash(Tels.Renderer.layer(layer, axiom), axiom), layer.name, layer)

      // Atoms are deduplicated by hash: an atom that occurs in two groups, or twice in one, is
      // one component of the lineage.
      val seen = scala.collection.mutable.HashSet.empty[Text]
      val buffer = scala.collection.mutable.ListBuffer.empty[Component]

      Tels.Atoms.decompose(schema).each: group =>
        group.atoms.each:
          case Tels.Atoms.Atom.Head(_, _) => ()

          case atom @ Tels.Atoms.Atom.Part(_, layer) =>
            val hash = atomHash(atom, axiom)
            if seen.add(Base256.encode(hash)) then buffer += Component(hash, Unset, layer)

      Lineage(schema, base, layers, buffer.toList.to(List))

    // A lineage from a schema document, as parsed.
    def of(document: Tel, axiom: Tels = Tels.Axiom.tels)
      ( using Tactic[Tel.Error], Tactic[Bintel.Error], Tactic[Tels.Renderer.Error] )
    :   Lineage =

      apply(Tels.Reconstructor.fromTel(document), axiom)

  // A base schema with every component of its lineage (§8.2, decoding step 3): its layers, and
  // the atoms of the base and of each layer, each with its hash. A signature naming the base is
  // decoded against exactly this set; a reader's or writer's library is a list of lineages.
  case class Lineage
    ( schema: Tels, base: Data, layers: List[Lineage.Component], atoms: List[Lineage.Component] ):

    // Every component but the base: the layers, then the atoms.
    def components: List[Lineage.Component] = List(layers, atoms).bind(identity)

    // The candidate hashes a signature naming this base is decoded against.
    def candidates: List[Data] = base :: components.map(_.hash)

    // The composition of the base with the named layers, in declaration order, as a signature.
    def signature(selection: List[Text])
      ( using Tactic[Bintel.Error], Tactic[Tels.Resolution.Error] )
    :   Data =

      val chosen = selection.map: name =>
        layers.seek(_.name == name).or:
          abort(Tels.Resolution.Error(Tels.Resolution.Error.Reason.UnknownLayer(name)))

        . hash

      encode(base :: chosen)

    // The component with exactly this hash.
    def component(hash: Data): Optional[Lineage.Component] =
      components.seek: component => same(component.hash, hash)

    // The components whose hash begins with `prefix`: none, one (denoted), or several (which an
    // acceptance treats as none, §8.4).
    def matching(prefix: Data): List[Lineage.Component] =
      components.filter(_.hash.readable.startsWith(prefix.readable))

    // The shortest prefix of the named layer's hash, at least `minimum` bytes long, that no other
    // component of this lineage shares — the form an acceptance names a further component by.
    def prefix(name: Text, minimum: Int = 4): Data raises Tels.Resolution.Error =
      val layer = layers.seek(_.name == name).or:
        abort(Tels.Resolution.Error(Tels.Resolution.Error.Reason.UnknownLayer(name)))

      // Lengthens the prefix one byte at a time until it denotes the layer alone; the full
      // hash always does.
      @scala.annotation.tailrec
      def shortest(length: Int): Data =
        val candidate = layer.hash.segment(0.z till length.z)

        if length >= layer.hash.length || matching(candidate).size <= 1 then candidate
        else shortest(length + 1)

      val start =
        if minimum < 1 then 1
        else if minimum > layer.hash.length then layer.hash.length
        else minimum

      shortest(start)

    // The composed schema a decoded hash sequence names: the base, then each component's layer
    // in order. The first hash must be this base; an unknown hash, or a sequence that does not
    // compose validly, is a resolution failure.
    def compose(hashes: List[Data]): Tels raises Tels.Resolution.Error =
      import Tels.Resolution.Error.Reason
      import errorDiagnostics.emptyDiagnostics

      hashes match
        case first :: rest =>
          if !same(first, base) then abort(Tels.Resolution.Error(Reason.BaseMismatch))

          val layers0 = rest.map: hash =>
            component(hash).or:
              abort(Tels.Resolution.Error(Reason.Unresolved(Tels.Resolution.Step.Library,
                  Base256.encode(hash))))

            . layer

          mitigate:
            case error: Tel.Error => Tels.Resolution.Error(Reason.NotSchema(error.message.text))

          . protect(Tels.Layers.composeComponents(schema, layers0))

        case _ =>
          abort(Tels.Resolution.Error(Reason.ComponentCount(1, 0)))

  object Library:
    val empty: Library = Library(List())
    case class Decoded(lineage: Lineage, hashes: List[Data])

  // A library of lineages, decoding signatures in two phases as §8.2 recommends: the base by the
  // first four bytes of the body, which are its hash prefix uncontested, then the further
  // components by the palimpsest search over that base's lineage alone, never over the whole
  // library.
  case class Library(lineages: List[Lineage]):
    def byBase(prefix: Data): List[Lineage] =
      lineages.filter(_.base.readable.startsWith(prefix.readable))

    // The lineage and ordered component hashes a signature names, if some lineage of the
    // library serves it.
    def decode(signature: Data): Optional[Library.Decoded] =
      if signature.length < 5 then Unset else
        val prefix = signature.segment(0.z till 4.z)

        def hashes(lineage: Lineage): Optional[List[Data]] =
          safely(SchemaSignature.decode(signature, lineage.candidates))

        byBase(prefix).seek(hashes(_).present).let: lineage =>
          hashes(lineage).let(Library.Decoded(lineage, _))

    // The composed schema a signature names.
    def resolve(signature: Data): Optional[Tels] raises Tels.Resolution.Error =
      decode(signature).let: decoded => decoded.lineage.compose(decoded.hashes)

  private[stratiform] def same(a: Data, b: Data): Boolean =
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

    // Frozen `Data` elements do not survive the generic `zip`/`to[Map]` pair (the
    // rebuilt element type loses its `^{}`), so the name-to-hash index is built on
    // the stdlib view.
    val byName = names.zip(layerHashes.stdlib).toMap

    val chosen: List[Data] = selection.map: name =>
      byName.getOrElse(name, abort(Tels.Resolution.Error(Reason.UnknownLayer(name))))

    // The selection is a linked list, so counting it is a walk; `linearSize` says so.
    val expectedCount = 1 + chosen.size
    val foundCount = componentCount(signature)

    if foundCount != expectedCount
    then abort(Tels.Resolution.Error(Reason.ComponentCount(expectedCount, foundCount)))

    val expected = encode(base :: chosen)

    if !same(expected, signature) then
      // Decompose the claimed signature over the schema's full
      // component library to name the diverging component.
      val reason = safely(decode(signature, base :: layerHashes)) match
        case decoded: List[Data] =>
          // Frozen `Data` elements lose their `^{}` when they pass through a generic
          // combinator's lambda parameter, so the component comparison stays on
          // indexed stdlib views, where the element type is preserved verbatim.
          val components = decoded.stdlib

          if components.isEmpty || !same(components.head, base) then Reason.BaseMismatch
          else
            val tail = components.tail.toIndexedSeq
            val wanted = chosen.stdlib.toIndexedSeq
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
