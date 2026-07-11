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
package xylophone

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import parsing.trackPositions

case class DPerson(name: Text, age: Int, email: Text) derives CanEqual
case class DContact(person: DPerson, company: Text) derives CanEqual

enum DShape derives CanEqual:
  case Circle(radius: Int)
  case Square(side: Int)

case class XmlIssues(items: List[(Text, XmlError)] = Nil)(using Diagnostics)
extends Error(m"${items.length} XML decoding issues"):
  def +(focus: Text, error: XmlError): XmlIssues = XmlIssues(items :+ (focus, error))

// Wrapped in an object so the `given Default[DPerson]` is in lexical scope
// *at the conjunction call site* — the `summonFrom` inside the inlined
// `Xml.DecodableDerivation.conjunction` body resolves against the call
// site's implicit context.
object DefaultPersonScope:
  given XmlSchema = XmlSchema.Freeform
  given Default[DPerson] = () => DPerson(t"", 0, t"")

  def run(): Set[String] =
    val xml = x"<root><company>Acme</company></root>"
    validate[Xml.Focus](XmlIssues()):
      case error: XmlError => accrual + (prior.let(_.path.encode).or(t"#"), error)
    . protect(xml.as[DContact]).items.map(_(0).s).to(Set)

case class DDrawing(shape: DShape, label: Text) derives CanEqual

// Sealed-trait Default test: when the discriminator doesn't match any
// variant, `Default[DShape]` lets the decode return a sentinel rather
// than aborting. The error registered carries whatever focus the
// enclosing scope set — here at the top level it's `Unset`, so the
// `or(t"#")` fallback fires for the path.
object DefaultShapeScope:
  given XmlSchema = XmlSchema.Freeform
  given Default[DShape] = () => DShape.Circle(-1)

  def run(): (Set[String], Int) =
    val xml = x"<Triangle><foo>bar</foo></Triangle>"
    val accrued = validate[Xml.Focus](XmlIssues()):
      case error: XmlError => accrual + (prior.let(_.path.encode).or(t"#"), error)
    . protect(xml.as[DShape])

    (accrued.items.map(_(0).s).to(Set), accrued.items.length)


object DecoderTests extends Suite(m"Xylophone case-class decoder tests"):

  // Inline, with a directly-constructed `Validate`: a `raises … tracks …` function VALUE
  // cannot be typed under capture checking (its honest type is a curried dependent context
  // function, an unimplemented compiler restriction), so the decode lambda must beta-reduce
  // away into `protect`'s inline position. See rep/DECISIONS.md.
  private inline def validateXml[result](xml: Xml)
    (inline decode: Xml => result raises XmlError tracks Xml.Focus)
  :   XmlIssues =
    Validate[XmlIssues, [r] =>> r raises XmlError, Xml.Focus]
      ( XmlIssues(),
        { case error: XmlError => accrual + (prior.let(_.path.encode).or(t"#"), error) } )
    . protect(decode(xml))

  def run(): Unit =
    given XmlSchema = XmlSchema.Freeform

    suite(m"Simple product"):
      test(m"Decode a flat case class"):
        x"<root><name>Alice</name><age>30</age><email>a@b.c</email></root>".as[DPerson]
      . assert(_ == DPerson(t"Alice", 30, t"a@b.c"))

      test(m"Field order doesn't matter"):
        x"<root><age>21</age><email>b@x</email><name>Bob</name></root>".as[DPerson]
      . assert(_ == DPerson(t"Bob", 21, t"b@x"))

    suite(m"Nested product"):
      test(m"Decode a nested case class"):
        x"""<root>
              <person><name>Carol</name><age>40</age><email>c@x</email></person>
              <company>Acme</company>
            </root>""".as[DContact]
      . assert(_ == DContact(DPerson(t"Carol", 40, t"c@x"), t"Acme"))

    suite(m"Sum type by element label"):
      test(m"Decode the Circle variant"):
        x"<Circle><radius>5</radius></Circle>".as[DShape]
      . assert(_ == DShape.Circle(5))

      test(m"Decode the Square variant"):
        x"<Square><side>4</side></Square>".as[DShape]
      . assert(_ == DShape.Square(4))

    suite(m"Validation accrual"):
      test(m"Fully-valid input accrues zero errors"):
        val xml = x"<root><name>Alice</name><age>30</age><email>a@b.c</email></root>"
        validateXml(xml)(_.as[DPerson]).items.length
      . assert(_ == 0)

      test(m"One missing field accrues one error"):
        val xml = x"<root><name>Alice</name><age>30</age></root>"
        validateXml(xml)(_.as[DPerson]).items.length
      . assert(_ == 1)

      test(m"Pointer identifies the missing field"):
        val xml = x"<root><name>Alice</name><age>30</age></root>"
        validateXml(xml)(_.as[DPerson]).items.map(_(0).s).to(Set)
      . assert(_ == Set("/email[1]"))

      test(m"Two missing primitive fields accrue two errors"):
        val xml = x"<root><name>Alice</name></root>"
        validateXml(xml)(_.as[DPerson]).items.length
      . assert(_ == 2)

      test(m"Pointers identify both missing primitive fields"):
        val xml = x"<root><name>Alice</name></root>"
        validateXml(xml)(_.as[DPerson]).items.map(_(0).s).to(Set)
      . assert(_ == Set("/age[1]", "/email[1]"))

      test(m"Wrong-type primitive field accrues an error"):
        val xml = x"<root><name>Alice</name><age>oldish</age><email>a@b.c</email></root>"
        validateXml(xml)(_.as[DPerson]).items.length
      . assert(_ == 1)

      test(m"Wrong-type primitive field reports the field's path"):
        val xml = x"<root><name>Alice</name><age>oldish</age><email>a@b.c</email></root>"
        validateXml(xml)(_.as[DPerson]).items.map(_(0).s).to(Set)
      . assert(_ == Set("/age[1]"))

      test(m"Wrong-type and missing-field errors mix"):
        val xml = x"<root><name>Alice</name><age>oldish</age></root>"
        validateXml(xml)(_.as[DPerson]).items.map(_(0).s).to(Set)
      . assert(_ == Set("/age[1]", "/email[1]"))

      test(m"Nested missing primitive field reports both segments"):
        val xml = x"""<root>
                       <person><name>Dave</name><age>22</age></person>
                       <company>Acme</company>
                     </root>"""
        validateXml(xml)(_.as[DContact]).items.map(_(0).s).to(Set)
      . assert(_ == Set("/person[1]/email[1]"))

      test(m"Missing nested case-class field expands per sub-field"):
        // A missing `person` triggers the nested conjunction's
        // raise+continue at `/person[1]` and then every sub-field's
        // missing-field raise at `/person[1]/<field>[1]` — the same
        // accrual rule that handles same-level missing fields.
        val xml = x"<root><company>Acme</company></root>"
        validateXml(xml)(_.as[DContact]).items.map(_(0).s).to(Set)
      . assert: paths =>
          paths == Set
            ( "/person[1]",
              "/person[1]/name[1]",
              "/person[1]/age[1]",
              "/person[1]/email[1]" )

    suite(m"Default-driven sentinels"):
      // A user-supplied `Default[T]` short-circuits the conjunction's
      // wrong-shape fallback and the disjunction's unknown-discriminator
      // fallback: instead of running `build` against an empty children
      // map (and accruing sub-field errors), or aborting, the focus
      // raises the error once and continues with the supplied default.

      test(m"Default[DPerson] collapses a missing nested into one error"):
        DefaultPersonScope.run()
      . assert(_ == Set("/person[1]"))

      test(m"Default[DShape] handles an unknown discriminator at the top level"):
        DefaultShapeScope.run()
      . assert((paths, count) => count == 1 && paths == Set("#"))

      test(m"Without Default[DShape], unknown discriminator aborts"):
        // Outside a `Default[DShape]`, the disjunction calls `abort`,
        // which the surrounding `validate` captures and reports as one
        // accrual entry. No `VariantError` punches through.
        val xml = x"<UnknownVariant><foo>bar</foo></UnknownVariant>"
        validateXml(xml)(_.as[DShape]).items.length
      . assert(_ == 1)

      test(m"Without Default[DPerson], a missing nested still expands"):
        // Confirms the existing (no-Default) accrual semantics from the
        // previous PR are unchanged for users who don't opt in.
        val xml = x"<root><company>Acme</company></root>"
        validateXml(xml)(_.as[DContact]).items.map(_(0).s).to(Set)
      . assert: paths =>
          paths == Set
            ( "/person[1]",
              "/person[1]/name[1]",
              "/person[1]/age[1]",
              "/person[1]/email[1]" )

    suite(m"Position-aware focus (tracked Xml)"):
      case class Tagged(items: List[(Text, Optional[Int], Optional[Int])] = Nil)
                       (using Diagnostics)
      extends Error(m"${items.length} validation issues"):
        def +(focus: Text, line: Optional[Int], column: Optional[Int]): Tagged =
          Tagged(items :+ (focus, line, column))

      inline def validateWithPositions[result]
        ( document: Document[Xml] )
        ( inline decode: Document[Xml] => result raises XmlError tracks Xml.Focus )
      :   List[(Text, Optional[Int], Optional[Int])] =
        Validate[Tagged, [r] =>> r raises XmlError, Xml.Focus]
          ( Tagged(),
            { case error: XmlError =>
                val position = prior.let(_.position)
                accrual + ( prior.let(_.path.encode).or(t"#"),
                            position.let(_.line.n1),
                            position.let(_.column.n1) ) } )
        . protect(decode(document)).items

      test(m"Missing-field error on a tracked Xml reports the parent's position"):
        val source = t"<root>\n  <name>Alice</name>\n  <age>30</age>\n</root>"
        val tracked = source.load[Xml]
        val results = validateWithPositions(tracked)(_.asTracked[DPerson])
        results.find(_(0) == t"/email[1]").map(_(1))
      . assert(_ == Some(Unset))

      test(m"Non-tracked Xml has Unset positions"):
        val xml = x"<root><name>Alice</name></root>"
        case class Tagged(items: List[Optional[Int]] = Nil)(using Diagnostics)
        extends Error(m"${items.length}"):
          def +(line: Optional[Int]): Tagged = Tagged(items :+ line)

        val lines: List[Optional[Int]] =
          validate[Xml.Focus](Tagged()):
            case error: XmlError =>
              accrual + prior.let(_.position).let(_.line.n1)
          . protect(xml.as[DPerson]).items

        lines.forall(_ == Unset)
      . assert(identity)
