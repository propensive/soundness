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

// The subtype relation of §24.3 (`Tels.Subtyping`), which decides compatibility between composed
// schemas (§8.2), and the projection of §24.5 (`Tels.Projection`), which restricts an element
// valid under a subtype to what its supertype can address, re-indexing it for BinTEL.
object SubtypingTests extends Suite(m"Stratiform subtyping tests"):

  private def resource(name: String): Data =
    val stream = getClass.getResourceAsStream(s"/stratiform/corpus/$name").nn
    try Array.unsafeFrozen(stream.readAllBytes().nn) finally stream.close()

  private def schema(source: Text): Tels = Tels.Reconstructor.fromTel(source.read[Tel])

  private def composed(source: Text, selection: List[Text]): Tels =
    Tels.Layers.compose(schema(source), selection)

  private lazy val contact: Tels =
    Tels.Reconstructor.fromTel(resource("contact-layered-schema.tel").read[Tel])

  // The remark of §24.4: `loose` declares `note` optional, `strict` declares it required, and
  // `[base, loose, strict]` leaves it optional, so the longer composition is not a subtype of
  // `[base, strict]` although its component sequence extends it.
  private val noteSource: Text = Text("""|tel 1.0
    |
    |name note
    |
    |document
    |  field id String
    |
    |layer
    |  name loose
    |  overlay
    |    field note String optional
    |
    |layer
    |  name strict
    |  overlay
    |    field note String
    |""".stripMargin)

  private val patternSource: Text = Text("""|tel 1.0
    |
    |name coded
    |
    |scalar Code
    |  pattern [a-z]+
    |
    |document
    |  field code Code
    |
    |layer
    |  name narrow
    |  scalar Code
    |    pattern [a-c]+
    |
    |layer
    |  name encoded
    |  scalar Code
    |    encoding base-256
    |""".stripMargin)

  private val recursiveSource: Text = Text("""|tel 1.0
    |
    |name tree
    |
    |record Node
    |  field label String
    |  field child Node optional
    |
    |document
    |  field root Node
    |
    |layer
    |  name labelled
    |  record Node
    |    field label String key
    |""".stripMargin)

  case class Flat(name: Text, count: Int, note: Optional[Text])

  private val flatSource: Text = Text("""|tel 1.0
    |
    |name flat
    |
    |document
    |  field name String
    |  field count String
    |  field note String optional
    |""".stripMargin)

  private def keywords(element: Tel.Element, schema: Tels): List[Text] = element match
    case Tel.Element.Node(_, struct: Tels.Struct, children) =>
      val flat = Bintel.flattenKeywords(struct, schema)

      proscenium.List.from(children.readable.toList).map:
        case Tel.Element.Node(index, _, _)  => flat.readable(index.or(0))(0)
        case Tel.Element.Value(index, _, _) => flat.readable(index)(0)

    case _ => List()

  def run(): Unit =
    suite(m"Records are subtyped by extension"):
      test(m"a layer adding a required field produces a subtype"):
        Tels.Subtyping.subtype(composed(noteSource, List(t"strict")), composed(noteSource, List()))
      . assert(identity)

      test(m"the base is not a subtype of a composition requiring more"):
        Tels.Subtyping.check(composed(noteSource, List()), composed(noteSource, List(t"strict")))
      . assert(_ == Tels.Subtyping.Failure.MissingRequired(List(), t"note"))

      test(m"the base is a subtype of a composition adding only optional members"):
        Tels.Subtyping.subtype(contact, Tels.Layers.compose(contact, List(t"with-address")))
      . assert(identity)

      test(m"the fully composed contact schema is a subtype of its base"):
        Tels.Subtyping.subtype(Tels.Layers.compose(contact), contact)
      . assert(identity)

      test(m"§24.4's remark: extending a composition is not a subsequence property"):
        val longer = composed(noteSource, List(t"loose", t"strict"))
        val shorter = composed(noteSource, List(t"strict"))
        (Tels.Subtyping.check(longer, shorter), Tels.Subtyping.subtype(shorter, longer))
      . assert(_ == (Tels.Subtyping.Failure.Required(List(), t"note"), true))

      test(m"every schema is a subtype of itself"):
        Tels.Subtyping.subtype(Tels.Layers.compose(contact), Tels.Layers.compose(contact))
      . assert(identity)

      test(m"a reordered supertype fails the order premise"):
        val reordered = schema(t"tel 1.0\n\nname contact\n\ndocument\n  field email String optional\n  field name String\n")
        Tels.Subtyping.check(contact, reordered)
      . assert(_ == Tels.Subtyping.Failure.MemberOrder(List(), t"name"))

      test(m"a keyed supertype member must be keyed in the subtype"):
        val keyed = composed(recursiveSource, List(t"labelled"))
        val plain = composed(recursiveSource, List())
        (Tels.Subtyping.subtype(keyed, plain), Tels.Subtyping.check(plain, keyed))
      . assert(_ == (true, Tels.Subtyping.Failure.Key(List(t"root"), t"label")))

      test(m"recursive definitions are decided coinductively"):
        val plain = composed(recursiveSource, List())
        Tels.Subtyping.subtype(plain, plain)
      . assert(identity)

    suite(m"Selects are subtyped by narrowing"):
      test(m"excluding a variant produces a subtype"):
        val narrowed = Tels.Layers.compose(contact, List(t"with-status", t"read-only-status"))
        val wide = Tels.Layers.compose(contact, List(t"with-status"))
        (Tels.Subtyping.subtype(narrowed, wide), Tels.Subtyping.check(wide, narrowed))
      . assert(_ == (true, Tels.Subtyping.Failure.VariantMissing(List(t"Status"), t"archived")))

    suite(m"Scalars are subtyped by tightening"):
      test(m"a narrower pattern produces a subtype"):
        val narrow = composed(patternSource, List(t"narrow"))
        val wide = composed(patternSource, List())
        (Tels.Subtyping.subtype(narrow, wide), Tels.Subtyping.check(wide, narrow))
      . assert(_ == (true, Tels.Subtyping.Failure.Patterns(List(t"code"))))

      test(m"adding an encoding produces a subtype, and dropping one does not"):
        val encoded = composed(patternSource, List(t"encoded"))
        val plain = composed(patternSource, List())
        (Tels.Subtyping.subtype(encoded, plain), Tels.Subtyping.check(plain, encoded))
      . assert(_ == (true, Tels.Subtyping.Failure.Encoding(List(t"code"))))

      test(m"a derived schema and its hand-written equivalent are subtypes of each other"):
        val derived = Tels.tels[Flat](t"flat")
        val written = schema(flatSource)
        (Tels.Subtyping.subtype(derived, written), Tels.Subtyping.subtype(written, derived))
      . assert(_ == (true, true))

      test(m"a supertype validator the subtype lacks fails"):
        val identifier = schema(t"tel 1.0\n\nname ids\n\ndocument\n  field id Identifier\n")
        val string = schema(t"tel 1.0\n\nname ids\n\ndocument\n  field id String\n")
        (Tels.Subtyping.subtype(identifier, string), Tels.Subtyping.check(string, identifier))
      . assert(_ == (true, Tels.Subtyping.Failure.Validators(List(t"id"))))

      test(m"a failure renders its position"):
        (Tels.Subtyping.Failure.Key(List(t"Node", t"root"), t"label"): Tels.Subtyping.Failure).communicate.text
      . assert(_ == t"root/Node does not key the member label")

    suite(m"Projection"):
      val fromSource = t"tel 1.0\n\nname wide\n\ndocument\n  field extra String optional\n  field name String\n  field flagged Flag optional\n"
      val toSource = t"tel 1.0\n\nname narrow\n\ndocument\n  field name String\n  field flagged Flag optional\n"
      lazy val from = schema(fromSource)
      lazy val to = schema(toSource)
      lazy val document = t"tel 1.0\n\nextra yes\nname Bea\nflagged\n".read[Tel]
      lazy val element = Tel.Type.assign(document, from)

      test(m"the wide schema is a subtype of the narrow one"):
        Tels.Subtyping.subtype(from, to)
      . assert(identity)

      test(m"projection drops the members the supertype cannot address"):
        keywords(Tels.Projection.project(element, from, to), to)
      . assert(_ == List(t"name", t"flagged"))

      test(m"projection re-indexes the surviving members"):
        Tels.Projection.project(element, from, to) match
          case Tel.Element.Node(_, _, children) => proscenium.List.from(children.readable.toList).map:
            case Tel.Element.Node(index, _, _)  => index.or(-1)
            case Tel.Element.Value(index, _, _) => index

          case _ => List()
      . assert(_ == List(0, 1))

      test(m"a projected element encodes and decodes under the supertype"):
        val projected = Tels.Projection.project(element, from, to)
        val bytes = Bintel.encode(projected, to)
        keywords(Bintel.decode(bytes, to), to)
      . assert(_ == List(t"name", t"flagged"))

      test(m"projection recurses into nested records"):
        val composedContact = Tels.Layers.compose(contact, List(t"with-address", t"extended-address"))
        val base = Tels.Layers.compose(contact, List(t"with-address"))
        val source = t"tel 1.0\n\nname Bea\naddress\n  street High\n  city Town\n  country Land\n  postcode X1\n"
        val projected = Tels.Projection.project(Tel.Type.assign(source.read[Tel], composedContact), composedContact, base)

        projected match
          case Tel.Element.Node(_, _, children) =>
            proscenium.List.from(children.readable.toList).filter:
              case Tel.Element.Node(_, _: Tels.Struct, _) => true
              case _                                      => false
            . map { node => keywords(node, base).size }
          case _ => List()
      . assert(_ == List(3))
