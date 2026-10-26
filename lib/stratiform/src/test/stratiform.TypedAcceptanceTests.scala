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

object TypedAcceptanceTests extends Suite("tel-typed-acceptance", m"Stratiform typed acceptance tests"):
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

  // Two sums whose cases share a simple name (#2319): each `Code` derives to a record of its own.
  enum Phrase derives CanEqual:
    case Code(language: Text, tokens: List[Text])
    case Plain(text: Text)

  enum Element derives CanEqual:
    case Code(language: Text, lines: List[Text], start: Int)
    case Para(content: List[Phrase])

  case class Sheet(elements: List[Element]) derives CanEqual

  // Two products whose simple names clash, under one root.
  object Alpha:
    case class Item(label: Text) derives CanEqual

  object Beta:
    case class Item(count: Int) derives CanEqual

  case class Both(alpha: Alpha.Item, beta: Beta.Item) derives CanEqual

  // One type reached by two paths.
  case class Pair(left: Address, right: Address)

  // A type whose own name is the one the clashing `Code`s qualify to.
  case class PhraseCode(code: Text)
  case class Clash(phrase: Phrase, element: Element, named: PhraseCode)

  // Sums at member positions (#2323): as a field, in a list, a set and an option, and recursively.
  enum Figurine derives CanEqual:
    case Circle(radius: Int)
    case Square(side: Int)

  case class Drawing
    ( shape:   Figurine,
      shapes:  List[Figurine],
      spare:   Optional[Figurine],
      palette: Set[Figurine] )
  derives CanEqual

  enum Outline derives CanEqual:
    case Leaf(size: Int)
    case Branch(children: List[Outline])

  case class Plan(outline: Outline) derives CanEqual

  // The type of the field `keyword` in a struct
  private def fieldType(struct: Tels.Struct, keyword: Text): Optional[Tels.Type] =
    struct.members.to[proscenium.List].bind:
      case field: Tels.Field if field.keyword == keyword => List(field.fieldType)
      case _                                             => Nil

    . prim

  // The members of the record `name` in a schema
  private def recordMembers(schema: Tels, name: Text): Optional[Tels.Struct] =
    schema.records.seek(_.name == name).let: record =>
      Tels.Struct(record.members, Array.empty)

  // Whether a struct holds exactly one member, a `SelectRef` to the select `name`
  private def selectsOnly(name: Text)(struct: Tels.Struct): Boolean =
    struct.members.to[proscenium.List] match
      case List(select: Tels.SelectRef) => select.reference == name
      case _                            => false

  private def keywords(struct: Tels.Struct): List[Text] =
    struct.members.to[List].map:
      case field: Tels.Field      => field.keyword
      case select: Tels.SelectRef => select.reference
      case _: Tels.Exclude        => t""

  private def retractions(schema: Tels): List[(Text, Text, List[Text])] =
    schema.layers.to[List].bind: layer =>
      layer.selects.to[List].map: select =>
        (layer.name, select.name, select.excludes.to[List])

  private def variants(schema: Tels, name: Text): List[Text] =
    schema.selects.to[List].bind: select =>
      if select.name != name then Nil
      else select.variants.to[List].map(_.keyword)

  // The names of the lineage's layers whose hashes a served document carries.
  private def servedLayers(lineage: SchemaSignature.Lineage, served: Tel.Acceptance.Served)
  :   List[Text] =

    lineage.layers.filter { layer => served.hashes.exists(hex(_) == hex(layer.hash)) }
    . map(_.layer.name)

  private def hex(data: Data): String =
    data.stdlib.map { byte => f"${byte & 0xff}%02x" }.mkString

  def run(): Unit =
    suite(m"Derived schema shape"):
      lazy val schema = Tels.tels[ContactV2](t"contact")

      test(m"a nested product is a named record referenced by the field"):
        (schema.records.to[List].map(_.name), keywords(schema.document))
      . assert(_ == (List(t"Address"), List(t"name", t"email", t"address")))

      test(m"a layered field forms a layer of that name, refining the root"):
        schema.layers.to[List].map: layer =>
          (layer.name, keywords(layer.overlay))
      . assert(_ == List((t"with-phone", List(t"phone"))))

      test(m"a required field has implicit polarity"):
        schema.document.members.to[List].map:
          case field: Tels.Field => field.required
          case _                 => Tels.Polarity.Tight
      . assert(_ == List(Tels.Polarity.Implicit, Tels.Polarity.Loose, Tels.Polarity.Loose))

      test(m"a recursive type registers its own record"):
        Tels.tels[Node](t"node").records.to[List].map(_.name)
      . assert(_ == List(t"Node"))

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

    suite(m"Definition names of derived schemas"):
      lazy val sheet = Tels.tels[Sheet](t"sheet")

      test(m"two types sharing a simple name are both kept, qualified by their enclosing types"):
        sheet.records.to[List].map(_.name).filter(!_.s.endsWith("Value"))
      . assert(_ == List(t"ElementCode", t"Para", t"PhraseCode", t"Plain"))

      test(m"each sum a field holds has a value record"):
        val names = sheet.records.to[List].map(_.name)
        (names.has(t"ElementValue"), names.has(t"PhraseValue"), names.count(_.s.endsWith("Value")))
      . assert(_ == (true, true, 2))

      test(m"a variant of a qualified record references it by its qualified name"):
        sheet.selects.to[List].bind(_.variants.to[List]).map(_.variantType)
      . assert(_ == List(Tels.Reference(t"ElementCode"), Tels.Reference(t"Para"),
                               Tels.Reference(t"PhraseCode"), Tels.Reference(t"Plain")))

      test(m"two products sharing a simple name are qualified by their enclosing objects"):
        Tels.tels[Both](t"both").records.to[List].map(_.name)
      . assert(_ == List(t"AlphaItem", t"BetaItem"))

      test(m"a value of clashing types round-trips through BinTEL"):
        Bintel.read[Both](Both(Alpha.Item(t"one"), Beta.Item(2)).bintel)
      . assert(_ == Both(Alpha.Item(t"one"), Beta.Item(2)))

      test(m"a type reached by several paths yields one definition"):
        Tels.tels[Pair](t"pair").records.to[List].map(_.name)
      . assert(_ == List(t"Address"))

      test(m"a type with no clashes is named as before"):
        val schema = Tels.tels[Envelope](t"envelope")
        val records = schema.records.to[List].map(_.name)
        val selects = schema.selects.to[List].map(_.name)
        (records.filter(!_.s.endsWith("Value")), records.filter(_.s.endsWith("Value")), selects)
      . assert(_ == (List(t"Ping", t"Data", t"Stop"), List(t"MessageValue"), List(t"Message")))

      test(m"a clash the qualification cannot resolve is E210"):
        capture[Tel.Error](Tels.tels[Clash](t"clash")).reason
      . assert(_ == Tel.Error.Reason.DuplicateDefinition)

    suite(m"Sums at member positions"):
      lazy val drawing = Tels.tels[Drawing](t"drawing")

      val value =
        Drawing
          ( Figurine.Circle(1),
            List(Figurine.Square(2), Figurine.Circle(3)),
            Figurine.Square(4),
            Set(Figurine.Circle(5)) )

      test(m"a sum-typed field references the sum's value record"):
        List(t"shape", t"shapes", t"spare", t"palette").map(fieldType(drawing.document, _))
      . assert(_ == List.fill(4)(Tels.Reference(t"FigurineValue")))

      test(m"a sum's value record holds one SelectRef to its select"):
        recordMembers(drawing, t"FigurineValue").let(selectsOnly(t"Figurine")).or(false)
      . assert(identity)

      test(m"a sum's select keeps the sum's name"):
        variants(drawing, t"Figurine")
      . assert(_ == List(t"circle", t"square"))

      test(m"a schema with sum-typed fields passes the composed checks"):
        Tels.Validation.checkComposed(Tels.Layers.compose(drawing)).name
      . assert(_ == t"drawing")

      test(m"a record with sum-typed fields round-trips through BinTEL"):
        Bintel.read[Drawing](value.bintel)
      . assert(_ == value)

      test(m"an absent optional sum round-trips through BinTEL"):
        val sparse = value.copy(spare = Unset)
        Bintel.read[Drawing](sparse.bintel)
      . assert(_ == value.copy(spare = Unset))

      test(m"a recursive sum round-trips through BinTEL"):
        val plan = Plan(Outline.Branch(List(Outline.Leaf(1), Outline.Branch(List(Outline.Leaf(2))))))
        Bintel.read[Plan](plan.bintel)
      . assert(_ == Plan(Outline.Branch(List(Outline.Leaf(1), Outline.Branch(List(Outline.Leaf(2)))))))

      test(m"a recursive sum's own list of itself references the sum's value record"):
        recordMembers(Tels.tels[Plan](t"plan"), t"Branch").let(fieldType(_, t"children"))
      . assert(_ == Tels.Reference(t"OutlineValue"))

      test(m"a sum at the root is a SelectRef to its select, with no value record"):
        val schema = Tels.tels[Figurine](t"figurine")
        (keywords(schema.document), recordMembers(schema, t"FigurineValue"))
      . assert(_ == (List(t"Figurine"), Unset))

      test(m"a value of sums whose cases share a name round-trips through BinTEL"):
        val sheet = Sheet(List(Element.Para(List(Phrase.Code(t"scala", List(t"x")))),
                               Element.Code(t"scala", List(t"val x = 1"), 3)))

        Bintel.read[Sheet](sheet.bintel)
      . assert(_ == Sheet(List(Element.Para(List(Phrase.Code(t"scala", List(t"x")))),
                               Element.Code(t"scala", List(t"val x = 1"), 3))))

      test(m"a hand-written Reference to a select is still E217"):
        val field = Tels.Field(Tels.Polarity.Implicit, Tels.Polarity.Implicit, t"shape",
                               Tels.Reference(t"FigurineValue"), Unset)

        val schema =
          Tels.Layers.compose(drawing).copy(document = Tels.Struct(Array(field), Array.empty))

        val field2 = field.copy(fieldType = Tels.Reference(t"Figurine"))
        val schema2 = schema.copy(document = Tels.Struct(Array(field2), Array.empty))

        capture[Tel.Error](Tels.Validation.checkComposed(schema2)).reason
      . assert(_ == Tel.Error.Reason.ReferenceKindMismatch)

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
