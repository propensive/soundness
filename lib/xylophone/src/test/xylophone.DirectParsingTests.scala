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

// Fixtures for the direct-parsing suite: a flat product, a nested product,
// `@attribute` fields (plain and `@name`-renamed), `@name`-renamed elements,
// a declared default, a sum dispatched by element label, and a type with
// only a custom decoder (read through the AST bridge).
case class PWorker(name: Text, age: Int) derives CanEqual
case class PFirm(title: Text, boss: PWorker) derives CanEqual
case class PBook(title: Text, @attribute isbn: Text) derives CanEqual
case class PTagged(@name[Xml](t"ISBN") @attribute isbn: Text, title: Text) derives CanEqual
case class PLabelled(@name[Xml](t"Title") title: Text, pages: Int) derives CanEqual
case class PDefaulted(name: Text, age: Int = 18) derives CanEqual

enum PShape derives CanEqual:
  case Circle(radius: Int)
  case Square(side: Int)

case class PTemperature(celsius: Int) derives CanEqual
case class PBoxed[value](value: value) derives CanEqual
case class PReading(temp: PTemperature, station: Text) derives CanEqual

case class PIssues(items: List[(Text, XmlError)] = Nil)(using Diagnostics)
extends Error(m"${items.length} XML decoding issues"):
  def +(focus: Text, error: XmlError): PIssues = PIssues(items :+ (focus, error))

// Wrapped in an object so the `given Default[PWorker]` is in lexical scope at
// the conjunction call site, as in `DecoderTests.DefaultPersonScope` — here
// checking that the direct path's absent handling honors it identically.
object DirectDefaultScope:
  given XmlSchema = XmlSchema.Freeform
  given Default[PWorker] = () => PWorker(t"", 0)
  given PWorker is Xml.Parsable = Xml.Parsable.derived
  given PFirm is Xml.Parsable = Xml.Parsable.derived

  def run(): Set[String] =
    val input = t"<root><title>Acme</title></root>"
    validate[Xml.Focus](PIssues()):
      case error: XmlError => accrual + (prior.let(_.path.encode).or(t"#"), error)
    . protect(input.read[PFirm in Xml]).items.map(_(0).s).to(Set)

object DirectParsingTests extends Suite(m"Xylophone direct parsing tests"):

  // Inline, with a directly-constructed `Validate`, exactly as
  // `DecoderTests.validateXml` — a `raises … tracks …` function value cannot
  // be typed under capture checking, so the decode expression must
  // beta-reduce into `protect`'s inline position.
  private inline def issues[result]
    (inline decode: => result raises XmlError tracks Xml.Focus)
  :   Set[String] =
    Validate[PIssues, [r] =>> r raises XmlError, Xml.Focus]
      ( PIssues(),
        { case error: XmlError => accrual + (prior.let(_.path.encode).or(t"#"), error) } )
    . protect(decode).items.map(_(0).s).to(Set)

  def run(): Unit =
    given XmlSchema = XmlSchema.Freeform

    given PWorker is Xml.Parsable = Xml.Parsable.derived
    given PFirm is Xml.Parsable = Xml.Parsable.derived
    given PBook is Xml.Parsable = Xml.Parsable.derived
    given PTagged is Xml.Parsable = Xml.Parsable.derived
    given PLabelled is Xml.Parsable = Xml.Parsable.derived
    given PDefaulted is Xml.Parsable = Xml.Parsable.derived
    given PShape is Xml.Parsable = Xml.Parsable.derived

    given PTemperature is Decodable in Xml = xml => PTemperature(xml.as[Int])
    given PTemperature is Xml.Parsable =
      Xml.Parsable.fromDecodable(summon[PTemperature is Decodable in Xml])
    given PReading is Xml.Parsable = Xml.Parsable.derived

    // The acceptance criterion: the direct read must equal the AST-path
    // read (parse to an `Xml` tree, then decode), for the same input.
    inline def parity[value](input: Text)
      ( using value is Xml.Parsable, value is Decodable in Xml )
    :   Boolean =
      input.read[value in Xml] == input.read[Xml].as[value]

    suite(m"Products"):
      test(m"Derive a direct product parser"):
        t"<root><name>Alice</name><age>30</age></root>".read[PWorker in Xml]
      . assert(_ == PWorker(t"Alice", 30))

      test(m"Field order doesn't matter, equally on both paths"):
        parity[PWorker](t"<root><age>21</age><name>Bob</name></root>")
      . assert(identity)

      test(m"Nested products parse directly"):
        val input = t"""<root>
                          <title>Acme</title>
                          <boss><name>Carol</name><age>40</age></boss>
                        </root>"""
        (input.read[PFirm in Xml], parity[PFirm](input))
      . assert(_ == (PFirm(t"Acme", PWorker(t"Carol", 40)), true))

      test(m"Unknown children are skipped, including nested subtrees"):
        t"""<root><name>Amy</name>
              <extra a="1"><deep><x/>text</deep><more/></extra>
              <age>50</age></root>""".read[PWorker in Xml]
      . assert(_ == PWorker(t"Amy", 50))

      test(m"A duplicate child keeps the first occurrence, as on the AST path"):
        val input = t"<root><name>Amy</name><name>Bea</name><age>3</age></root>"
        (input.read[PWorker in Xml], parity[PWorker](input))
      . assert(_ == (PWorker(t"Amy", 3), true))

      test(m"A missing field takes the declared default"):
        t"<root><name>Kid</name></root>".read[PDefaulted in Xml]
      . assert(_ == PDefaulted(t"Kid", 18))

      test(m"A missing required field raises XmlError"):
        capture[XmlError](t"<root><age>30</age></root>".read[PWorker in Xml])
        true
      . assert(identity)

      test(m"A generic product parses directly"):
        // (Also exercises `Xml.Parsable.derived` at an applied-generic root,
        // the `derivedOne` requirement.)
        given PBoxed[Int] is Xml.Parsable = Xml.Parsable.derived
        t"<root><value>42</value></root>".read[PBoxed[Int] in Xml]
      . assert(_ == PBoxed(42))

    suite(m"Content shapes"):
      test(m"Mixed content between children is ignored, equally on both paths"):
        val input = t"<root>hello<name>A</name><!--c--><?pi data?> <age>4</age>bye</root>"
        (input.read[PWorker in Xml], parity[PWorker](input))
      . assert(_ == (PWorker(t"A", 4), true))

      test(m"Entities in leaf text expand, equally on both paths"):
        val input = t"<root><name>a&amp;b&#33;</name><age>1</age></root>"
        (input.read[PWorker in Xml], parity[PWorker](input))
      . assert(_ == (PWorker(t"a&b!", 1), true))

      test(m"A self-closing leaf reads as empty text"):
        val input = t"<root><name/><age>1</age></root>"
        (input.read[PWorker in Xml], parity[PWorker](input))
      . assert(_ == (PWorker(t"", 1), true))

      test(m"An empty start/end leaf reads as empty text"):
        val input = t"<root><name></name><age>1</age></root>"
        (input.read[PWorker in Xml], parity[PWorker](input))
      . assert(_ == (PWorker(t"", 1), true))

      test(m"CDATA in a leaf is wrong-shape on both paths"):
        // `textOf` accepts only a single `TextNode` child, so a CDATA leaf
        // raises (and continues with the empty sentinel) on the AST path;
        // the direct path must accrue the same focus.
        val input = t"<root><name><![CDATA[A]]></name><age>1</age></root>"
        ( issues(input.read[PWorker in Xml]),
          issues(input.read[Xml].as[PWorker]) )
      . assert { (direct, ast) => direct == ast && direct == Set("/name[1]") }

      test(m"A primitive root element parses directly"):
        val input = t"<message>1</message>"
        (input.read[Int in Xml], parity[Int](input))
      . assert(_ == (1, true))

      test(m"A bad primitive root raises XmlError on both paths"):
        capture[XmlError](t"<message>ABC</message>".read[Int in Xml])
        true
      . assert(identity)

    suite(m"Attributes and renames"):
      test(m"An @attribute field reads from the attribute"):
        val input = t"""<PBook isbn="0441013597"><title>Dune</title></PBook>"""
        (input.read[PBook in Xml], parity[PBook](input))
      . assert(_ == (PBook(t"Dune", t"0441013597"), true))

      test(m"An @name-renamed @attribute field reads from the renamed attribute"):
        val input = t"""<x ISBN="99"><title>T</title></x>"""
        (input.read[PTagged in Xml], parity[PTagged](input))
      . assert(_ == (PTagged(t"99", t"T"), true))

      test(m"A child element sharing an @attribute field's name is skipped"):
        val input = t"""<x isbn="1"><isbn>2</isbn><title>T</title></x>"""
        (input.read[PBook in Xml], parity[PBook](input))
      . assert(_ == (PBook(t"T", t"1"), true))

      test(m"@name[Xml] renames an element field, equally on both paths"):
        val input = t"<x><Title>Dune</Title><pages>412</pages></x>"
        (input.read[PLabelled in Xml], parity[PLabelled](input))
      . assert(_ == (PLabelled(t"Dune", 412), true))

    suite(m"Sums by element label"):
      test(m"Decode the Circle variant directly"):
        val input = t"<Circle><radius>5</radius></Circle>"
        (input.read[PShape in Xml], parity[PShape](input))
      . assert(_ == (PShape.Circle(5), true))

      test(m"Decode the Square variant directly"):
        val input = t"<Square><side>4</side></Square>"
        (input.read[PShape in Xml], parity[PShape](input))
      . assert(_ == (PShape.Square(4), true))

      test(m"An unknown variant aborts on both paths"):
        capture[XmlError](t"<Triangle><foo>1</foo></Triangle>".read[PShape in Xml])
        true
      . assert(identity)

    suite(m"Custom-Decodable bridge"):
      test(m"A type with only a custom Decodable reads through the bridge"):
        t"<t>21</t>".read[PTemperature in Xml]
      . assert(_ == PTemperature(21))

      test(m"A bridged field materializes just its own element"):
        val input = t"<r><temp>21</temp><station>Kew</station></r>"
        (input.read[PReading in Xml], parity[PReading](input))
      . assert(_ == (PReading(PTemperature(21), t"Kew"), true))

    suite(m"Accrual parity"):
      test(m"A fully-valid input accrues zero errors"):
        issues(t"<root><name>A</name><age>1</age></root>".read[PWorker in Xml])
      . assert(_ == Set())

      test(m"A missing field accrues the same focus on both paths"):
        val input = t"<root><name>Alice</name></root>"
        ( issues(input.read[PWorker in Xml]),
          issues(input.read[Xml].as[PWorker]) )
      . assert { (direct, ast) => direct == ast && direct == Set("/age[1]") }

      test(m"A wrong-type primitive accrues the same focus on both paths"):
        val input = t"<root><name>Alice</name><age>old</age></root>"
        ( issues(input.read[PWorker in Xml]),
          issues(input.read[Xml].as[PWorker]) )
      . assert { (direct, ast) => direct == ast && direct == Set("/age[1]") }

      test(m"A missing nested product expands per sub-field on both paths"):
        val input = t"<root><title>Acme</title></root>"
        ( issues(input.read[PFirm in Xml]),
          issues(input.read[Xml].as[PFirm]) )
      . assert: (direct, ast) =>
          direct == ast &&
            direct == Set("/boss[1]", "/boss[1]/name[1]", "/boss[1]/age[1]")

      test(m"An empty root element accrues every field on both paths"):
        val input = t"<root/>"
        ( issues(input.read[PWorker in Xml]),
          issues(input.read[Xml].as[PWorker]) )
      . assert { (direct, ast) => direct == ast && direct == Set("/name[1]", "/age[1]") }

      test(m"Default[PWorker] collapses a missing nested value into one error"):
        DirectDefaultScope.run()
      . assert(_ == Set("/boss[1]"))

    suite(m"Malformed input"):
      test(m"A mismatched close tag is a ParseError on the direct path too"):
        capture[ParseError](t"<root><name>A</name><age>1</b></root>".read[PWorker in Xml])
        . issue
      . assert: issue =>
          issue match
            case Xml.Issue.MismatchedTag(t"age", t"b") => true
            case _                                     => false

      test(m"A mismatched close tag inside a skipped element is still checked"):
        capture[ParseError](t"<root><junk><a></b></junk></root>".read[PWorker in Xml]).issue
      . assert: issue =>
          issue match
            case Xml.Issue.MismatchedTag(t"a", t"b") => true
            case _                                   => false

      test(m"An unclosed element is Incomplete on the direct path too"):
        capture[ParseError](t"<root><name>A</name>".read[PWorker in Xml]).issue
      . assert: issue =>
          issue match
            case Xml.Issue.Incomplete(t"root") => true
            case _                             => false
