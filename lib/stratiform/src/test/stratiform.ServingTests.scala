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
import charEncoders.utf8Encoder
import denominative.dysasymptotics.linearSize

// The writer and reader obligations of BinTEL §8.4 (`Tel.Acceptance.serve`, `served` and
// `receive`): a writer holding a value under some composition serves the first alternative it
// can, with the richest permitted composition, and a reader takes the first alternative whose
// schema is a supertype of what arrived, projecting the document to it.
object ServingTests extends Suite(m"Stratiform acceptance serving tests"):

  private def resource(name: String): Data =
    val stream = getClass.getResourceAsStream(s"/stratiform/corpus/$name").nn
    try Array.unsafeFrozen(stream.readAllBytes().nn) finally stream.close()

  private def resourceText(name: String): Text =
    Text(String(Array.unsafeJvm(resource(name)), "UTF-8").trim.nn)

  private lazy val contactDoc: Tel = resource("contact-layered-schema.tel").read[Tel]
  private lazy val contact: SchemaSignature.Lineage = SchemaSignature.Lineage.of(contactDoc)
  private lazy val library: SchemaSignature.Library = SchemaSignature.Library(List(contact))
  private lazy val request: Tel.Acceptance = resourceText("acceptance-request.tel").as[Tel.Acceptance]

  private lazy val first: Tel.Acceptance.Alternative =
    request.alternatives.prim.or(panic(m"no alternative"))

  private val peopleSource: Text = Text("""|tel 1.0
    |
    |name people
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
    |  name strict
    |  overlay
    |    field email String
    |""".stripMargin)

  private lazy val people: SchemaSignature.Lineage =
    SchemaSignature.Lineage(Tels.Reconstructor.fromTel(peopleSource.read[Tel]))

  private def hashOf(lineage: SchemaSignature.Lineage, name: Text): Data =
    lineage.layers.seek(_.name == name).or(panic(m"no layer $name")).hash

  private def element(lineage: SchemaSignature.Lineage, composition: List[Data], source: Text)
  :   Tel.Element =

    Tel.Type.assign(source.read[Tel], lineage.compose(composition))

  private def keywords(element: Tel.Element, schema: Tels): List[Text] = element match
    case Tel.Element.Node(_, struct: Tels.Struct, children) =>
      val flat = Bintel.flattenKeywords(struct, schema)

      proscenium.List.from(children.readable.toList).map:
        case Tel.Element.Node(index, _, _)  => flat.readable(index.or(0))(0)
        case Tel.Element.Value(index, _, _) => flat.readable(index)(0)

    case _ => List()

  private def alternative(lineage: SchemaSignature.Lineage, selection: List[Text],
      selfContained: Boolean = false, anyPublished: Boolean = false,
      components: List[Tel.Acceptance.Component] = List())
  :   Tel.Acceptance.Alternative =

    Tel.Acceptance.Alternative(Tel.Acceptance.Signature(lineage.signature(selection)),
        selfContained, anyPublished, components)

  def run(): Unit =
    suite(m"Serving the worked request"):
      lazy val composition = List(contact.base, hashOf(contact, t"with-address"), hashOf(contact, t"with-phone"))

      lazy val held = element(contact, composition,
          t"name Bea\nemail bea@example.com\naddress\n  street High\n  city Town\n  country Land\nphone\n  country-code 44\n  number 123\n")

      lazy val served = Tel.Acceptance.serve(request, contact, composition, held)

      test(m"the first alternative is served, with the named phone layer included"):
        served.let { served => (served.alternative == first, served.hashes.size) }
      . assert(_ == (true, 3))

      test(m"the document is framed externally with the composition's signature"):
        served.let: served =>
          val framed = Bintel.unframe(served.document)
          SchemaSignature.decode(framed.signature, contact.candidates).size
      . assert(_ == 3)

      test(m"the reader reads it under the first alternative, projected to its invocation schema"):
        served.let: served =>
          Tel.Acceptance.receive(request, library, served.document).let: (reading, element) =>
            (reading.alternative == first, keywords(element, reading.invocation))
      . assert(_ == (true, List(t"name", t"email", t"address")))

      test(m"the phone layer's members are decodable before projection"):
        served.let: served =>
          val framed = Bintel.unframe(served.document)
          keywords(Bintel.decode(framed.body, served.schema), served.schema).size
      . assert(_ == 4)

      test(m"a writer holding only the base serves the requirement itself"):
        val base = List(contact.base)
        val plain = element(contact, base, t"name Bea\n")
        Tel.Acceptance.serve(request, contact, base, plain).let(_.hashes.size)
      . assert(_ == 2)

    suite(m"Alternatives and permissions"):
      lazy val base = List(people.base)
      lazy val withAlpha = List(people.base, hashOf(people, t"alpha"))
      lazy val plain = element(people, base, t"name Bea\n")
      lazy val emailed = element(people, withAlpha, t"name Bea\nemail bea@example.com\n")

      test(m"a writer unable to meet a required layer falls back to the next alternative"):
        val acceptance = Tel.Acceptance(alternative(people, List(t"strict")), alternative(people, List(), selfContained = true))
        Tel.Acceptance.serve(acceptance, people, base, plain).let { served => (served.alternative.selfContained, served.hashes.size) }
      . assert(_ == (true, 1))

      test(m"no servable alternative yields nothing"):
        Tel.Acceptance.serve(Tel.Acceptance(alternative(people, List(t"strict"))), people, base, plain).present
      . assert(_ == false)

      test(m"an unnamed layer is left out when no flag permits it"):
        Tel.Acceptance.serve(Tel.Acceptance(alternative(people, List())), people, withAlpha, emailed).let(_.hashes.size)
      . assert(_ == 1)

      test(m"any-published permits an unnamed layer in external mode"):
        val acceptance = Tel.Acceptance(alternative(people, List(), anyPublished = true))
        Tel.Acceptance.serve(acceptance, people, withAlpha, emailed).let: served =>
          (served.hashes.size, Bintel.unframe(served.document).signature.length)
      . assert(_ == (2, 37))

      test(m"self-contained permits an unnamed layer, embedding the schema"):
        val acceptance = Tel.Acceptance(alternative(people, List(), selfContained = true))
        Tel.Acceptance.serve(acceptance, people, withAlpha, emailed).let: served =>
          val document = Bintel.decodeDocumentSelfContained(served.document)
          (served.hashes.size, document.signature.length, keywords(document.root, served.schema))
      . assert(_ == (2, 37, List(t"name", t"email")))

      test(m"a named component the writer holds is included"):
        val components = List(Tel.Acceptance.Component(people.prefix(t"alpha")))
        val acceptance = Tel.Acceptance(alternative(people, List(), components = components))
        Tel.Acceptance.serve(acceptance, people, withAlpha, emailed).let(_.hashes.size)
      . assert(_ == 2)

      test(m"a named component of another lineage is ignored"):
        val components = List(Tel.Acceptance.Component(contact.prefix(t"with-phone")))
        val acceptance = Tel.Acceptance(alternative(people, List(), components = components))
        Tel.Acceptance.serve(acceptance, people, withAlpha, emailed).let(_.hashes.size)
      . assert(_ == 1)

      test(m"a document served under the emailed layer is left unread by a reader requiring strict"):
        val strict = Tel.Acceptance(alternative(people, List(t"strict")))
        val signature = people.signature(List(t"alpha"))
        Tel.Acceptance.served(strict, SchemaSignature.Library(List(people)), signature).present
      . assert(_ == false)

      test(m"a reader takes the first alternative the document satisfies"):
        val acceptance = Tel.Acceptance(alternative(people, List(t"strict")), alternative(people, List()))
        val signature = people.signature(List(t"alpha"))
        Tel.Acceptance.served(acceptance, SchemaSignature.Library(List(people)), signature).let(_.alternative.schema.count)
      . assert(_ == 1)
