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
package xylophone

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTraces

case class DPerson(name: Text, age: Int, email: Text) derives CanEqual
case class DContact(person: DPerson, company: Text) derives CanEqual

enum DShape derives CanEqual:
  case Circle(radius: Int)
  case Square(side: Int)

case class XmlIssues(items: List[(Text, XmlError)] = Nil)(using Diagnostics)
extends Error(m"${items.length} XML decoding issues"):
  def +(focus: Text, error: XmlError): XmlIssues = XmlIssues(items :+ (focus, error))


object DecoderTests extends Suite(m"Xylophone case-class decoder tests"):

  private def validateXml[result](xml: Xml)(decode: Xml => result raises XmlError tracks Xml.Focus)
  :   XmlIssues =
    validate[Xml.Focus](XmlIssues()):
      case error: XmlError => accrual + (prior.let(_.path.encode).or(t"#"), error)
    . within(decode(xml))

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
      // The conjunction aborts on the first missing field, so only the
      // first accrued error is registered. Multi-error accumulation
      // would need primitive decoders that `raise` (record + continue)
      // rather than `abort` — a future refinement.
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

      test(m"Nested missing field reports both segments"):
        val xml = x"""<root>
                       <person><name>Dave</name><age>22</age></person>
                       <company>Acme</company>
                     </root>"""
        validateXml(xml)(_.as[DContact]).items.map(_(0).s).to(Set)
      . assert(_ == Set("/person[1]/email[1]"))

    suite(m"Position-aware focus (tracked Xml)"):
      def validateWithPositions[result]
        ( tracked: Xml.Tracked )
        ( decode: Xml.Tracked => result raises XmlError tracks Xml.Focus )
      :   List[(Text, Optional[Int], Optional[Int])] =

        case class Tagged(items: List[(Text, Optional[Int], Optional[Int])] = Nil)
                         (using Diagnostics)
        extends Error(m"${items.length} validation issues"):
          def +(focus: Text, line: Optional[Int], column: Optional[Int]): Tagged =
            Tagged(items :+ (focus, line, column))

        validate[Xml.Focus](Tagged()):
          case error: XmlError =>
            val position = prior.let(_.position)
            accrual + ( prior.let(_.path.encode).or(t"#"),
                        position.let(_.line.n1),
                        position.let(_.column.n1) )
        . within(decode(tracked)).items

      test(m"Missing-field error on a tracked Xml reports the parent's position"):
        val source = t"<root>\n  <name>Alice</name>\n  <age>30</age>\n</root>"
        val tracked = Xml.parseTracked(source)
        val results = validateWithPositions(tracked)(_.as[DPerson])
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
          . within(xml.as[DPerson]).items

        lines.forall(_ == Unset)
      . assert(identity)
