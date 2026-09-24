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

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import charEncoders.utf8Encoder
import denominative.dysasymptotics.linearSize

// Rendering schema values back to their typed documents (`Tels.Renderer`), the canonical
// decomposition into atoms (§20.3 of the TEL specification, `Tels.Atoms`), and lineages and
// libraries (BinTEL §8.2, decoding step 3): a schema's base, layers and atoms with their hashes,
// against which a signature is decoded in two phases. The specification's worked acceptance
// request names the layered contact schema's components, which ties the rendered hashes to the
// reference implementation's.
object LineageTests extends Suite(m"Stratiform lineage tests"):

  private def resource(name: String): Data =
    val stream = getClass.getResourceAsStream(s"/stratiform/corpus/$name").nn
    try Array.unsafeFrozen(stream.readAllBytes().nn) finally stream.close()

  private def resourceText(name: String): Text =
    Text(String(Array.unsafeJvm(resource(name)), "UTF-8").trim.nn)

  private def hex(data: Data): String =
    data.readable.toSeq.map { byte => f"${byte & 0xff}%02x" }.mkString

  private val axiom: Tels = Tels.Axiom.tels

  private lazy val contactDoc: Tel = resource("contact-layered-schema.tel").read[Tel]
  private lazy val contact: Tels = Tels.Reconstructor.fromTel(contactDoc)

  private val layeredSource: Text = Text("""|tel 1.0
    |
    |name layered
    |
    |document
    |  field name String
    |
    |layer
    |  name alpha
    |  overlay
    |    field email String optional
    |
    |layer
    |  name beta
    |  overlay
    |    field email String optional
    |
    |layer
    |  name gamma
    |  overlay
    |    field email String
    |""".stripMargin)

  private lazy val layered: Tels = Tels.Reconstructor.fromTel(layeredSource.read[Tel])

  private def layerNamed(schema: Tels, name: Text): Tels.Layer =
    schema.layers.seek(_.name == name).or(panic(m"no layer $name"))

  private def layersOf(atoms: List[Tels.Atoms.Atom]): List[Tels.Layer] = atoms.map:
    case Tels.Atoms.Atom.Part(_, layer) => layer

    case Tels.Atoms.Atom.Head(_, _) =>
      Tels.Layer(t"", Tels.Struct(Array.empty, Array.empty), Array.empty, Array.empty, Array.empty)

  private def keywords(schema: Tels): List[Text] =
    proscenium.List.from(schema.document.members.readable.toList).map:
      case field: Tels.Field      => field.keyword
      case select: Tels.SelectRef => select.reference
      case _: Tels.Exclude        => t""

  case class Inner(count: Int)
  case class Outer(name: Text, inner: Inner)
  case class Flat(name: Text, count: Int, note: Optional[Text])
  case class Mapped(prefs: Map[Text, Int])

  def run(): Unit =
    suite(m"Rendering a schema value"):
      test(m"the tels axiom renders to the pinned tels value hash"):
        hex(SchemaSignature.hash(Tels.Renderer.element(Tels.Axiom.tels)))
      . assert(_ == "d440b01e327c62c41ac641047f2c4d8df3cbe94abb24db33f189226b7b8b7ad3")

      test(m"the acceptance axiom renders to the pinned acceptance value hash"):
        hex(SchemaSignature.hash(Tels.Renderer.element(Tels.Axiom.acceptance)))
      . assert(_ == "ed8e981cd770b7d75067e05dfd87d8b5c37e97763872febf513c125ab1a8b245")

      test(m"a reconstructed layered schema renders to the bytes of its document"):
        val fromDocument = Tel.Type.assign(contactDoc, axiom).bintel(axiom)
        val rendered = Tels.Renderer.element(contact).bintel(axiom)
        hex(rendered) == hex(fromDocument)
      . assert(identity)

      test(m"the component hashes of a rendered schema are those of its document"):
        val (base, layers) = SchemaSignature.componentHashes(contactDoc, axiom)
        val (base2, layers2) = SchemaSignature.componentHashes(contact, axiom)
        hex(base) == hex(base2) && layers.map(hex) == layers2.map(hex)
      . assert(identity)

      test(m"a derived schema with an inline struct cannot be rendered"):
        capture[Tels.Renderer.Error](Tels.Renderer.element(Tels.tels[Mapped](t"mapped"))).reason
      . assert(_ == Tels.Renderer.Error.Reason.InlineStruct(t"prefs"))

      test(m"a nested product renders as a named record"):
        Tels.Renderer.element(Tels.tels[Outer](t"outer")).children.length
      . assert(_ == 3)

      test(m"a derived flat schema renders and hashes"):
        SchemaSignature.hash(Tels.Renderer.element(Tels.tels[Flat](t"flat"))).length
      . assert(_ == 32)

      test(m"a derived flat schema renders as its hand-written equivalent"):
        val source = t"tel 1.0\n\nname flat\n\ndocument\n  field name String\n  field count String\n  field note String optional\n"
        val handWritten = Tel.Type.assign(source.read[Tel], axiom).valueHash(axiom)
        hex(SchemaSignature.hash(Tels.Renderer.element(Tels.tels[Flat](t"flat")))) == hex(handWritten.data)
      . assert(identity)

    suite(m"Atoms"):
      test(m"the base group begins with the head atom"):
        Tels.Atoms.base(contact).atoms.prim match
          case Tels.Atoms.Atom.Head(name, sigil) => (name, sigil.absent)
          case _                                 => (t"", false)
      . assert(_ == (t"contact", true))

      test(m"the base of the contact schema has three atoms"):
        Tels.Atoms.base(contact).atoms.size
      . assert(_ == 3)

      test(m"with-address decomposes into four atoms"):
        Tels.Atoms.layer(layerNamed(contact, t"with-address")).atoms.size
      . assert(_ == 4)

      test(m"a select's variants are one atom and each exclude another"):
        ( Tels.Atoms.layer(layerNamed(contact, t"with-status")).atoms.size,
          Tels.Atoms.layer(layerNamed(contact, t"read-only-status")).atoms.size )
      . assert(_ == (2, 1))

      test(m"the head atom hashes as the definition-less schema document"):
        val source = t"tel 1.0\n\nname contact\n\ndocument\n"
        val expected = Tel.Type.assign(source.read[Tel], axiom).valueHash(axiom)
        hex(SchemaSignature.atomHash(Tels.Atoms.Atom.Head(t"contact", Unset))) == hex(expected.data)
      . assert(identity)

      test(m"an atom's hash does not depend on the layer containing it"):
        val alpha = Tels.Atoms.layer(layerNamed(layered, t"alpha")).atoms.map(SchemaSignature.atomHash(_)).map(hex)
        val beta  = Tels.Atoms.layer(layerNamed(layered, t"beta")).atoms.map(SchemaSignature.atomHash(_)).map(hex)
        val gamma = Tels.Atoms.layer(layerNamed(layered, t"gamma")).atoms.map(SchemaSignature.atomHash(_)).map(hex)
        (alpha == beta, alpha == gamma)
      . assert(_ == (true, false))

      test(m"composing a layer and composing its atoms give the same schema"):
        val byLayer = Tels.Layers.compose(contact, List(t"with-address"))
        val atoms = layersOf(Tels.Atoms.layer(layerNamed(contact, t"with-address")).atoms)
        val byAtoms = Tels.Layers.composeComponents(contact, atoms)
        Tels.Reconstructor.equivalent(byLayer, byAtoms)
      . assert(identity)

      test(m"composing every atom of every layer gives the fully composed schema"):
        val expansion = Tels.Atoms.expansion(Tels.Atoms.decompose(contact))
        val parts = expansion.filter:
          case Tels.Atoms.Atom.Head(_, _) => false
          case _                          => true

        val byAtoms = Tels.Layers.composeComponents(contact, layersOf(parts))
        Tels.Reconstructor.equivalent(Tels.Layers.compose(contact), byAtoms)
      . assert(identity)

    suite(m"Lineages and libraries"):
      lazy val lineage: SchemaSignature.Lineage = SchemaSignature.Lineage.of(contactDoc)
      lazy val other: SchemaSignature.Lineage = SchemaSignature.Lineage(layered)
      lazy val request: Tel.Acceptance = resourceText("acceptance-request.tel").as[Tel.Acceptance]
      lazy val first: Tel.Acceptance.Alternative = request.alternatives.prim.or(panic(m"no alternative"))

      test(m"the lineage names the six layers in declaration order"):
        lineage.layers.map(_.name)
      . assert(_ == List(t"with-address", t"extended-address", t"with-phone", t"with-status",
          t"with-business", t"read-only-status"))

      test(m"the lineage's atoms are distinct by hash"):
        val hashes = lineage.atoms.map(_.text)
        hashes.size == hashes.distinct.size
      . assert(identity)

      test(m"the base with with-address signs as the demo request's first alternative"):
        Base256.encode(lineage.signature(List(t"with-address")))
      . assert(_ == first.schema.text)

      test(m"the base alone signs as the demo request's second alternative"):
        request.alternatives match
          case _ :: second :: _ => Base256.encode(lineage.signature(List())) == second.schema.text
          case _                => false
      . assert(identity)

      test(m"the four-byte prefixes of with-phone and with-status are the request's components"):
        proscenium.List(t"with-phone", t"with-status").map { name => Base256.encode(lineage.prefix(name)) }
      . assert(_ == first.components.map(_.text))

      test(m"a prefix denotes exactly one component"):
        lineage.matching(lineage.prefix(t"with-phone")).size
      . assert(_ == 1)

      test(m"a full hash is its own prefix"):
        lineage.prefix(t"with-phone", 32).length
      . assert(_ == 32)

      test(m"an unknown layer name is a resolution error"):
        capture[Tels.Resolution.Error](lineage.signature(List(t"nope"))).reason
      . assert(_ == Tels.Resolution.Error.Reason.UnknownLayer(t"nope"))

      test(m"a library decodes the request's signature to the base and with-address"):
        SchemaSignature.Library(List(other, lineage)).decode(first.schema.data).let: decoded =>
          (decoded.lineage.schema.name, decoded.hashes.map(hex))
      . assert(_ == (t"contact", List(hex(lineage.base), lineage.layers.map(_.hash).prim.let(hex).or(""))))

      test(m"a library composes a decoded signature"):
        SchemaSignature.Library(List(lineage)).resolve(first.schema.data).let(keywords(_))
      . assert(_ == List(t"name", t"email", t"address"))

      test(m"a signature naming atoms composes to the same schema as the layer"):
        val atomHashes = Tels.Atoms.layer(layerNamed(contact, t"with-address")).atoms.map(SchemaSignature.atomHash(_))
        val signature = SchemaSignature.encode(lineage.base :: atomHashes)
        val expected = Tels.Layers.compose(contact, List(t"with-address"))
        SchemaSignature.Library(List(lineage)).resolve(signature).let(Tels.Reconstructor.equivalent(_, expected))
      . assert(_ == true)

      test(m"a signature naming some atoms of a layer composes to a smaller schema"):
        val atoms = Tels.Atoms.layer(layerNamed(contact, t"with-address")).atoms
        val recordOnly = atoms.filter:
          case Tels.Atoms.Atom.Part(Tels.Atoms.Path.Record(_), _) => true
          case _                                                   => false

        val signature = SchemaSignature.encode(lineage.base :: recordOnly.map(SchemaSignature.atomHash(_)))
        SchemaSignature.Library(List(lineage)).resolve(signature).let(schema => (keywords(schema), schema.records.length))
      . assert(_ == (List(t"name", t"email"), 1))

      test(m"a signature naming atoms whose references are unmet does not compose"):
        val atoms = Tels.Atoms.layer(layerNamed(contact, t"with-address")).atoms
        val overlayOnly = atoms.filter:
          case Tels.Atoms.Atom.Part(Tels.Atoms.Path.Body, _) => true
          case _                                              => false

        val signature = SchemaSignature.encode(lineage.base :: overlayOnly.map(SchemaSignature.atomHash(_)))
        capture[Tels.Resolution.Error](SchemaSignature.Library(List(lineage)).resolve(signature)).reason match
          case Tels.Resolution.Error.Reason.NotSchema(_) => true
          case _                                         => false
      . assert(identity)

      test(m"a signature no lineage serves decodes to nothing"):
        SchemaSignature.Library(List(other)).decode(first.schema.data).present
      . assert(_ == false)

      test(m"a two-lineage library picks the base by its prefix"):
        val signature = other.signature(List(t"alpha"))
        SchemaSignature.Library(List(lineage, other)).decode(signature).let(_.lineage.schema.name)
      . assert(_ == t"layered")

      test(m"an unknown component in a decoded sequence is a resolution error"):
        val bogus = List(lineage.base, other.base)
        capture[Tels.Resolution.Error](lineage.compose(bogus)).reason match
          case Tels.Resolution.Error.Reason.Unresolved(step, _) => step
          case _                                                => Tels.Resolution.Step.Lira
      . assert(_ == Tels.Resolution.Step.Library)
