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
import codepages.utf8Codepage
import denominative.dysasymptotics.linearSize

// Acceptances from Scala types (`Tel.Acceptance[(A, B)]`): the derived schema's shape —
// nested products as named records, optional members as atoms, `@assign` groups and `@retract`ed
// cases as layers — the
// alternatives it yields, a writer serving a typed value, and a reader decoding what arrives into
// the union of its formats.
object withPhone extends Tel.Layer("with-phone")
object lean extends Tel.Layer("lean")
object quiet extends Tel.Layer("quiet")

object TypedAcceptanceTests extends Suite(m"Stratiform typed acceptance tests"):
  // A protocol whose messages are a sum, some of which layers retract: `lean` excludes `ping` and
  // `data`, and `quiet` excludes `data` too.
  enum Message derives CanEqual:
    @retract(lean) case Ping
    @retract(lean) @retract(quiet) case Data(payload: Text)
    case Stop

  sealed trait Shape
  @retract(lean) case class Square(side: Int) extends Shape
  @retract(lean) case object Dot extends Shape
  case class Circle(radius: Int) extends Shape

  case class Envelope(id: Text, message: Message)

  case class Address(street: Text, city: Text)
  case class ContactV1(name: Text, email: Optional[Text])

  case class ContactV2
    ( name:    Text,
      email:   Optional[Text],
      address: Optional[Address],
      @assign(withPhone) phone: Optional[Text] )

  case class Invoice(number: Text, total: Int)
  case class Node(label: Text, child: Optional[Node])

  private def keywords(struct: Tels.Struct): List[Text] =
    proscenium.List.from(struct.members.readable.toList).map:
      case field: Tels.Field      => field.keyword
      case select: Tels.SelectRef => select.reference
      case _: Tels.Exclude        => t""

  private def retractions(schema: Tels): List[(Text, Text, List[Text])] =
    proscenium.List.from(schema.layers.readable.toList).bind: layer =>
      proscenium.List.from(layer.selects.readable.toList).map: select =>
        (layer.name, select.name, proscenium.List.from(select.excludes.readable.toList))

  private def variants(schema: Tels, name: Text): List[Text] =
    proscenium.List.from(schema.selects.readable.toList).bind: select =>
      if select.name != name then Nil
      else proscenium.List.from(select.variants.readable.toList).map(_.keyword)

  // The names of the lineage's layers whose hashes a served document carries.
  private def servedLayers(lineage: SchemaSignature.Lineage, served: Tel.Acceptance.Served)
  :   List[Text] =

    lineage.layers.filter { layer => served.hashes.exists(hex(_) == hex(layer.hash)) }
    . map(_.layer.name)

  private def hex(data: Data): String =
    data.readable.toSeq.map { byte => f"${byte & 0xff}%02x" }.mkString

  def run(): Unit =
    suite(m"Derived schema shape"):
      lazy val schema = Tels.tels[ContactV2](t"contact")

      test(m"a nested product is a named record referenced by the field"):
        (schema.records.readable.toList.map(_.name), keywords(schema.document))
      . assert(_ == (scala.List(t"Address"), List(t"name", t"email", t"address")))

      test(m"a layered field forms a layer of that name, refining the root"):
        proscenium.List.from(schema.layers.readable.toList).map: layer =>
          (layer.name, keywords(layer.overlay))
      . assert(_ == List((t"with-phone", List(t"phone"))))

      test(m"a required field has implicit polarity"):
        schema.document.members.readable.toList.map:
          case field: Tels.Field => field.required
          case _                 => Tels.Polarity.Tight
      . assert(_ == scala.List(Tels.Polarity.Implicit, Tels.Polarity.Loose, Tels.Polarity.Loose))

      test(m"a recursive type registers its own record"):
        Tels.tels[Node](t"node").records.readable.toList.map(_.name)
      . assert(_ == scala.List(t"Node"))

      test(m"a derived schema renders and hashes as its hand-written equivalent"):
        val source = Text("""|tel 1.0
          |
          |name contact
          |
          |record Address
          |  field street String
          |  field city String
          |
          |document
          |  field name String
          |  field email String optional
          |  field address Address optional
          |
          |layer
          |  name with-phone
          |  overlay
          |    field phone String optional
          |""".stripMargin)

        val (base, layers) = SchemaSignature.componentHashes(source.read[Tel], Tels.Axiom.tels)
        val (base2, layers2) = SchemaSignature.componentHashes(schema, Tels.Axiom.tels)
        hex(base) == hex(base2) && layers.map(hex) == layers2.map(hex)
      . assert(identity)

    suite(m"Layers named by objects"):
      test(m"a layer's name is read from its type"):
        scala.compiletime.constValue[Tel.Name[withPhone.type]]
      . assert(_ == "with-phone")

      test(m"retracted enum cases form layers which exclude them"):
        retractions(Tels.tels[Message](t"messages"))
      . assert(_ == List((t"lean", t"Message", List(t"ping", t"data")),
                         (t"quiet", t"Message", List(t"data"))))

      test(m"retracted cases stay variants of the base select"):
        variants(Tels.tels[Message](t"messages"), t"Message")
      . assert(_ == List(t"ping", t"data", t"stop"))

      test(m"case classes and case objects of a sealed trait can be retracted"):
        retractions(Tels.tels[Shape](t"shapes"))
      . assert(_ == List((t"lean", t"Shape", List(t"square", t"dot"))))

      test(m"a nested sum's retractions are layers of the enclosing schema"):
        retractions(Tels.tels[Envelope](t"envelope"))
      . assert(_ == List((t"lean", t"Message", List(t"ping", t"data")),
                         (t"quiet", t"Message", List(t"data"))))

      test(m"composing a retracting layer removes its cases"):
        variants(Tels.Layers.compose(Tels.tels[Envelope](t"envelope"), List(t"lean")), t"Message")
      . assert(_ == List(t"stop"))

      test(m"a retracting layer hashes as its hand-written equivalent"):
        val source = Text("""|tel 1.0
          |
          |name shapes
          |
          |record Square
          |  field side String
          |
          |record Dot
          |
          |record Circle
          |  field radius String
          |
          |select Shape
          |  variant square Square
          |  variant dot Dot
          |  variant circle Circle
          |
          |document
          |  select Shape
          |
          |layer
          |  name lean
          |  select Shape
          |    exclude square
          |    exclude dot
          |""".stripMargin)

        val (base, layers) = SchemaSignature.componentHashes(source.read[Tel], Tels.Axiom.tels)

        val (base2, layers2) =
          SchemaSignature.componentHashes(Tels.tels[Shape](t"shapes"), Tels.Axiom.tels)

        (hex(base) == hex(base2), layers.map(hex) == layers2.map(hex))
      . assert(_ == (true, true))

      test(m"a layer named by a val is rejected"):
        demilitarize:
          val alias: withPhone.type = withPhone
          case class Contact(@assign(alias) phone: Text)
          Tels.tels[Contact](t"contact")
        . map(_.message)
      . assert(_.exists(_.contains("singleton object")))

      test(m"a layer whose name is not a literal is rejected"):
        demilitarize:
          val label: String = "unnamed"
          object unnamed extends Tel.Layer(label)
          case class Contact(@assign(unnamed) phone: Text)
          Tels.tels[Contact](t"contact")
        . map(_.message)
      . assert(_.exists(_.contains("string literal")))

      test(m"a layer retracting every case of a sum is rejected"):
        demilitarize:
          enum Gone:
            @retract(lean) case First
            @retract(lean) case Second

          Tels.tels[Gone](t"gone")
        . map(_.message)
      . assert(_.exists(_.contains("retracts every case")))

    suite(m"Lineages of derived schemas"):
      lazy val v2 = Tel.Acceptance.lineage[ContactV2](t"contact-v2")
      lazy val v1 = Tel.Acceptance.lineage[ContactV1](t"contact-v1")

      test(m"the base is the required members alone"):
        keywords(v2.schema.document)
      . assert(_ == List(t"name"))

      test(m"optional members and the records only they reach are the extra atoms"):
        // the two fields of `Address`, then `email` and `address`
        v2.extras.size
      . assert(_ == 4)

      test(m"the base's own atoms and the layer's are components, but not offered"):
        (v2.atoms.size, v2.offered.size)
      . assert(_ == (6, 5))

      test(m"the layer is a component"):
        v2.layers.map(_.name)
      . assert(_ == List(t"with-phone"))

      test(m"the base carries the schema name, so differently named lineages differ"):
        hex(v1.base) == hex(v2.base)
      . assert(_ == false)

      test(m"the same types under the same schema name share a base"):
        hex(Tel.Acceptance.lineage[ContactV1](t"contact").base)
        == hex(Tel.Acceptance.lineage[ContactV2](t"contact").base)
      . assert(identity)

    suite(m"Typed acceptances"):
      lazy val acceptance = Tel.Acceptance[(ContactV2, Invoice)]()

      test(m"one alternative per format, in order"):
        acceptance.acceptance.alternatives.map(_.components.size)
      . assert(_ == List(5, 0))

      test(m"each alternative requires the base alone"):
        acceptance.acceptance.alternatives.map(_.schema.count)
      . assert(_ == List(1, 1))

      test(m"`otherwise` appends a less preferred format"):
        Tel.Acceptance[(ContactV2, Invoice)]().otherwise[ContactV1].acceptance.alternatives.size
      . assert(_ == 3)

      test(m"the acceptance round-trips through its text form"):
        acceptance.acceptance.encode.as[Tel.Acceptance] == acceptance.acceptance
      . assert(identity)

    suite(m"Serving and reading typed values"):
      lazy val acceptance = Tel.Acceptance[(ContactV2, Invoice)]()
      lazy val bea = ContactV2(t"Bea", t"bea@example.com", Address(t"High", t"Town"), t"123")

      test(m"a value of the first format is served with every component"):
        bea.fulfil(acceptance.acceptance).let { served => (served.alternative.schema.count, served.hashes.size) }
      . assert(_ == (1, 6))

      test(m"and read back as that format, with every member"):
        bea.fulfil(acceptance.acceptance).let: served =>
          acceptance.read(served.document) match
            case contact: ContactV2 => contact == bea
            case _: Invoice         => false
      . assert(_ == true)

      test(m"a value of the second format is read as the second format"):
        Invoice(t"42", 7).fulfil(acceptance.acceptance).let: served =>
          acceptance.read(served.document) match
            case invoice: Invoice => invoice.number
            case _: ContactV2     => t""
      . assert(_ == t"42")

      test(m"a value with members unset is served and read without them"):
        val plain = ContactV2(t"Bea", Unset, Unset, Unset)
        plain.fulfil(acceptance.acceptance).let: served =>
          acceptance.read(served.document) match
            case contact: ContactV2 => contact == plain
            case _: Invoice         => false
      . assert(_ == true)

      // The older writer cannot serve the newer base (the names differ), so it serves its own
      // alternative; the reader, whose first alternative's schema the document's is a subtype
      // of structurally, takes its most preferred reading and decodes the newer format.
      test(m"a writer of an older format is read as the most preferred compatible format"):
        val versions = Tel.Acceptance[(ContactV2, ContactV1)]()

        ContactV1(t"Bea", t"bea@example.com").fulfil(versions.acceptance).let: served =>
          (served.hashes.size, versions.read(served.document) match
            case contact: ContactV2 => contact.email
            case _: ContactV1       => Unset)
      . assert(_ == (2, t"bea@example.com"))

      test(m"retracting layers are components of a sum's lineage"):
        Tel.Acceptance.lineage[Envelope](t"envelope").layers.map(_.name)
      . assert(_ == List(t"lean", t"quiet"))

      test(m"a case no layer retracts is served under the retracting layer"):
        val shapes = Tel.Acceptance[Tuple1[Shape]]()
        val circle: Shape = Circle(3)
        circle.fulfil(shapes.acceptance).let: served =>
          (servedLayers(shapes.lineages.stdlib.head, served), shapes.read(served.document) == circle)
      . assert(_ == (List(t"lean"), true))

      test(m"a retracted case is served without the layer retracting it"):
        val shapes = Tel.Acceptance[Tuple1[Shape]]()
        val square: Shape = Square(4)
        square.fulfil(shapes.acceptance).let: served =>
          (servedLayers(shapes.lineages.stdlib.head, served), shapes.read(served.document) == square)
      . assert(_ == (Nil, true))

      test(m"a case retracted from one of two layers is served under the other"):
        val messages = Tel.Acceptance[Tuple1[Message]]()
        val ping: Message = Message.Ping
        ping.fulfil(messages.acceptance).let: served =>
          (servedLayers(messages.lineages.stdlib.head, served), messages.read(served.document) == ping)
      . assert(_ == (List(t"quiet"), true))

      test(m"two layers retracting the same case are served together"):
        val messages = Tel.Acceptance[Tuple1[Message]]()
        val stop: Message = Message.Stop
        stop.fulfil(messages.acceptance).let: served =>
          (servedLayers(messages.lineages.stdlib.head, served), messages.read(served.document) == stop)
      . assert(_ == (List(t"lean", t"quiet"), true))

      test(m"a case retracted from both layers is served under neither"):
        val messages = Tel.Acceptance[Tuple1[Message]]()
        val data: Message = Message.Data(t"payload")
        data.fulfil(messages.acceptance).let: served =>
          (servedLayers(messages.lineages.stdlib.head, served), messages.read(served.document) == data)
      . assert(_ == (Nil, true))

      test(m"a document of no accepted format is unread"):
        val other = Tel.Acceptance[Tuple1[Node]]()
        Node(t"root", Unset).fulfil(other.acceptance).let: served =>
          capture[Tel.Acceptance.Error](acceptance.read(served.document)).reason
      . assert(_ == Tel.Acceptance.Error.Reason.Unread)
