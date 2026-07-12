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
import threading.virtualThreading
import probates.cancelProbate

case class Worker(name: Text, age: Int)
case class Firm(name: Text, ceo: Worker)

case class Book(title: Text, @attribute isbn: Text) derives CanEqual
case class Bibliography(author: Text, book: Book) derives CanEqual

enum ColorVal:
  case Rgb(red: Int, green: Int, blue: Int)
  case Cmyk(cyan: Int, magenta: Int, yellow: Int, key: Int)

case class Pixel(x: Int, y: Int, color: ColorVal)

object Tests extends Suite(m"Xylophone tests"):

  def elem(label: Text, children: Node*): Element =
    Element(label, Attributes.empty, IArray.from(children))

  def elem(label: Text, attrs: Map[Text, Text], children: Node*): Element =
    Element(label, Attributes.from(attrs), IArray.from(children))

  def run(): Unit =
    given XmlSchema = XmlSchema.Freeform

    suite(m"Interpolator tests"):
      test(m"Simple interpolator"):
        x"<message>1</message>"
      . assert(_ == t"<message>1</message>".read[Xml])

      test(m"Processing instruction"):
        x"<?foo bar baz?>"
      . assert(_ == t"<?foo bar baz?>".read[Xml])

      test(m"Serialize content"):
        x"<message>hello world</message>".show
      . assert(_ == t"<message>hello world</message>")

      test(m"Serialize document"):
        supervise:
          unsafely(t"""<?xml  version="1.0"?><message>hello world</message>""".load[Xml].lazyList[Text].read[Text])
      . assert(_ == t"""<?xml version="1.0"?><message>hello world</message>""")

    suite(m"`in Xml` decoder shorthand"):
      test(m"`read[T in Xml]` resolves a value directly from text"):
        t"<Worker><name>Alice</name><age>30</age></Worker>".read[Worker in Xml]
      . assert(_ == Worker(t"Alice", 30))

      test(m"`read[T in Xml]` works for nested case classes"):
        t"<Firm><name>Acme</name><ceo><name>Alice</name><age>30</age></ceo></Firm>"
          . read[Firm in Xml]
      . assert(_ == Firm(t"Acme", Worker(t"Alice", 30)))

    test(m"extract integer"):
      x"""<message>1</message>""".as[Int]
    . assert(_ == 1)

    test(m"fail to extract bad integer"):
      // The explicit `Int is Decodable in Xml` raises `XmlError` (not
      // `NumberError`) so multi-error accrual can register and continue
      // through every bad field of a case class; outside `validate`,
      // `raise` still throws via the ambient `Tactic`.
      capture[XmlError](x"""<message>ABC</message>""".as[Int])
      true
    . assert(identity)

    test(m"extract email address"):
      x"""<email>test@example.com</email>""".as[EmailAddress]
    . assert(_ == t"test@example.com".as[EmailAddress])

    suite(m"Element parsing"):
      test(m"Empty self-closing element"):
        t"<a/>".read[Xml]
      . assert(_ == elem(t"a"))

      test(m"Empty start/end element"):
        t"<a></a>".read[Xml]
      . assert(_ == elem(t"a"))

      test(m"Element with text"):
        t"<a>hello</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"hello")))

      test(m"Nested element"):
        t"<a><b/></a>".read[Xml]
      . assert(_ == elem(t"a", elem(t"b")))

      test(m"Deeply nested elements"):
        t"<a><b><c><d/></c></b></a>".read[Xml]
      . assert(_ == elem(t"a", elem(t"b", elem(t"c", elem(t"d")))))

      test(m"Mixed content (text and elements)"):
        t"<a>text<b/>more</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"text"), elem(t"b"), TextNode(t"more")))

      test(m"Self-closing element with whitespace before slash"):
        t"<a />".read[Xml]
      . assert(_ == elem(t"a"))

      test(m"Element name is case-sensitive"):
        t"<Foo/>".read[Xml]
      . assert(_ == elem(t"Foo"))

      test(m"Element name with mixed case is preserved"):
        t"<FooBar/>".read[Xml]
      . assert(_ == elem(t"FooBar"))

      test(m"Element name with underscore"):
        t"<a_b/>".read[Xml]
      . assert(_ == elem(t"a_b"))

      test(m"Element name starting with underscore"):
        t"<_a/>".read[Xml]
      . assert(_ == elem(t"_a"))

      test(m"Element name with hyphen"):
        t"<a-b/>".read[Xml]
      . assert(_ == elem(t"a-b"))

      test(m"Element name with period"):
        t"<a.b/>".read[Xml]
      . assert(_ == elem(t"a.b"))

      test(m"Element name with digit (not first)"):
        t"<a1/>".read[Xml]
      . assert(_ == elem(t"a1"))

      test(m"Whitespace between elements is preserved"):
        t"<a> <b/> </a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t" "), elem(t"b"), TextNode(t" ")))

      test(m"Multiple sibling elements"):
        t"<a><b/><c/><d/></a>".read[Xml]
      . assert(_ == elem(t"a", elem(t"b"), elem(t"c"), elem(t"d")))


    suite(m"Element name validation"):
      test(m"Element name cannot start with digit"):
        capture[ParseError](t"<1a/>".read[Xml]).issue
      . assert: issue =>
          issue match
            case _: Xml.Issue.InvalidTagStart => true
            case _: Xml.Issue.Unexpected     => true
            case _                           => false

      test(m"Element name cannot start with hyphen"):
        capture[ParseError](t"<-a/>".read[Xml]).issue
      . assert: issue =>
          issue match
            case _: Xml.Issue.Unexpected => true
            case _                       => false

      test(m"Element name cannot start with period"):
        capture[ParseError](t"<.a/>".read[Xml]).issue
      . assert: issue =>
          issue match
            case _: Xml.Issue.Unexpected => true
            case _                       => false


    suite(m"Attribute parsing"):
      test(m"Single attribute with double quotes"):
        t"""<a x="1"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"1")))

      test(m"Single attribute with single quotes"):
        t"<a x='1'/>".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"1")))

      test(m"Multiple attributes"):
        t"""<a x="1" y="2"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"1", t"y" -> t"2")))

      test(m"Empty attribute value"):
        t"""<a x=""/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"")))

      test(m"Whitespace around equals sign"):
        t"""<a x = "1"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"1")))

      test(m"Whitespace before equals sign"):
        t"""<a x ="1"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"1")))

      test(m"Whitespace after equals sign"):
        t"""<a x= "1"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"1")))

      test(m"Attribute name with hyphen"):
        t"""<a a-b="c"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"a-b" -> t"c")))

      test(m"Attribute name with underscore"):
        t"""<a a_b="c"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"a_b" -> t"c")))

      test(m"Attribute name with period"):
        t"""<a a.b="c"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"a.b" -> t"c")))

      test(m"Attribute name with digit"):
        t"""<a a1="c"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"a1" -> t"c")))

      test(m"Attribute name starting with underscore"):
        t"""<a _x="c"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"_x" -> t"c")))

      test(m"Duplicate attribute is rejected"):
        capture[ParseError](t"""<a x="1" x="2"/>""".read[Xml]).issue
      . assert: issue =>
          issue match
            case Xml.Issue.DuplicateAttribute(t"x") => true
            case _                                  => false

      test(m"Unquoted attribute is rejected"):
        capture[ParseError](t"<a x=1/>".read[Xml]).issue
      . assert: issue =>
          issue match
            case Xml.Issue.UnquotedAttribute => true
            case _                           => false

      test(m"Attribute value with entity reference"):
        t"""<a x="&amp;"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"&")))

      test(m"Attribute value with character reference"):
        t"""<a x="&#65;"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"A")))

      test(m"Attribute value with hex character reference"):
        t"""<a x="&#x41;"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"A")))

      test(m"Single-quoted attribute may contain double quote"):
        t"""<a x='one "two" three'/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"""one "two" three""")))

      test(m"Double-quoted attribute may contain single quote"):
        t"""<a x="one 'two' three"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"one 'two' three")))


    suite(m"Comments"):
      test(m"Simple comment inside element"):
        t"<a><!-- hello --></a>".read[Xml]
      . assert(_ == elem(t"a", Comment(t" hello ")))

      test(m"Empty comment"):
        t"<a><!----></a>".read[Xml]
      . assert(_ == elem(t"a", Comment(t"")))

      test(m"Comment with single hyphen"):
        t"<a><!-- a-b --></a>".read[Xml]
      . assert(_ == elem(t"a", Comment(t" a-b ")))

      test(m"Comment containing entity-like text"):
        t"<a><!-- &amp; --></a>".read[Xml]
      . assert(_ == elem(t"a", Comment(t" &amp; ")))

      test(m"Comment containing tag-like text"):
        t"<a><!-- <foo/> --></a>".read[Xml]
      . assert(_ == elem(t"a", Comment(t" <foo/> ")))

      test(m"Comment with double-hyphen is rejected"):
        capture[ParseError](t"<a><!-- a -- b --></a>".read[Xml])
      . assert(_.issue.isInstanceOf[Xml.Issue.Unexpected])


    suite(m"CDATA sections"):
      test(m"Simple CDATA section"):
        t"<a><![CDATA[hello]]></a>".read[Xml]
      . assert(_ == elem(t"a", Cdata(t"hello")))

      test(m"Empty CDATA section"):
        t"<a><![CDATA[]]></a>".read[Xml]
      . assert(_ == elem(t"a", Cdata(t"")))

      test(m"CDATA containing tag-like text"):
        t"<a><![CDATA[<not a tag>]]></a>".read[Xml]
      . assert(_ == elem(t"a", Cdata(t"<not a tag>")))

      test(m"CDATA containing entity-like text"):
        t"<a><![CDATA[&not;]]></a>".read[Xml]
      . assert(_ == elem(t"a", Cdata(t"&not;")))

      test(m"CDATA with single closing bracket"):
        t"<a><![CDATA[a]b]]></a>".read[Xml]
      . assert(_ == elem(t"a", Cdata(t"a]b")))


    suite(m"Processing instructions"):
      test(m"Simple processing instruction"):
        t"<a><?target data?></a>".read[Xml]
      . assert(_ == elem(t"a", ProcessingInstruction(t"target", t"data")))

      test(m"Processing instruction with no data"):
        t"<a><?target?></a>".read[Xml]
      . assert(_ == elem(t"a", ProcessingInstruction(t"target", t"")))

      test(m"Processing instruction with multi-word data"):
        t"<a><?target one two three?></a>".read[Xml]
      . assert(_ == elem(t"a", ProcessingInstruction(t"target", t"one two three")))

      test(m"Processing instruction at root level"):
        t"<?xml-stylesheet href='style.xsl'?><a/>".read[Xml]
      . assert: result =>
          result == Fragment(
            ProcessingInstruction(t"xml-stylesheet", t"href='style.xsl'"),
            elem(t"a"))

      test(m"Processing instruction target cannot be xml"):
        capture[ParseError](t"<a><?xml foo?></a>".read[Xml])
      . assert(_.issue.isInstanceOf[Xml.Issue.InvalidTag])

      test(m"Processing instruction target cannot be XML"):
        capture[ParseError](t"<a><?XML foo?></a>".read[Xml])
      . assert(_.issue.isInstanceOf[Xml.Issue.InvalidTag])


    suite(m"Character and entity references"):
      test(m"Built-in entity &amp;"):
        t"<a>&amp;</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"&")))

      test(m"Built-in entity &lt;"):
        t"<a>&lt;</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"<")))

      test(m"Built-in entity &gt;"):
        t"<a>&gt;</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t">")))

      test(m"Built-in entity &quot;"):
        t"""<a>&quot;</a>""".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"\"")))

      test(m"Built-in entity &apos;"):
        t"<a>&apos;</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"'")))

      test(m"Decimal character reference"):
        t"<a>&#65;</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"A")))

      test(m"Hex character reference (lowercase)"):
        t"<a>&#x41;</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"A")))

      test(m"Hex character reference (uppercase X)"):
        t"<a>&#X41;</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"A")))

      test(m"Mixed text and entities"):
        t"<a>foo &amp; bar</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"foo & bar")))

      test(m"Multiple entities in sequence"):
        t"<a>&lt;&gt;&amp;</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"<>&")))

      test(m"Hex char ref for emoji"):
        t"<a>&#x1f600;</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"😀")))


    suite(m"Whitespace handling"):
      test(m"Leading whitespace inside element"):
        t"<a>  hello</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"  hello")))

      test(m"Trailing whitespace inside element"):
        t"<a>hello  </a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"hello  ")))

      test(m"Internal whitespace preserved"):
        t"<a>one  two</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"one  two")))

      test(m"Newlines preserved"):
        t"<a>one\ntwo</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"one\ntwo")))

      test(m"Tab preserved"):
        t"<a>one\ttwo</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"one\ttwo")))


    suite(m"XML declarations"):
      test(m"Simple XML declaration"):
        supervise:
          t"""<?xml version="1.0"?><a/>""".load[Xml]
      . assert(_ == Document(elem(t"a"), Header(t"1.0", Unset, Unset)))

      test(m"XML declaration with encoding"):
        supervise:
          t"""<?xml version="1.0" encoding="UTF-8"?><a/>""".load[Xml]
      . assert(_ == Document(elem(t"a"), Header(t"1.0", t"UTF-8", Unset)))

      test(m"XML declaration with standalone yes"):
        supervise:
          t"""<?xml version="1.0" standalone="yes"?><a/>""".load[Xml]
      . assert(_ == Document(elem(t"a"), Header(t"1.0", Unset, true)))

      test(m"XML declaration with standalone no"):
        supervise:
          t"""<?xml version="1.0" standalone="no"?><a/>""".load[Xml]
      . assert(_ == Document(elem(t"a"), Header(t"1.0", Unset, false)))

      test(m"XML declaration with encoding and standalone"):
        supervise:
          t"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?><a/>""".load[Xml]
      . assert(_ == Document(elem(t"a"), Header(t"1.0", t"UTF-8", true)))

      test(m"XML declaration with single-quoted version"):
        supervise:
          t"""<?xml version='1.0'?><a/>""".load[Xml]
      . assert(_ == Document(elem(t"a"), Header(t"1.0", Unset, Unset)))


    suite(m"Document errors"):
      test(m"Mismatched closing tag"):
        capture[ParseError](t"<a></b>".read[Xml]).issue
      . assert: issue =>
          issue match
            case Xml.Issue.MismatchedTag(t"a", t"b") => true
            case _                                   => false

      test(m"Unopened closing tag"):
        capture[ParseError](t"</a>".read[Xml]).issue
      . assert: issue =>
          issue match
            case Xml.Issue.UnopenedTag(t"a") => true
            case _                           => false

      test(m"Unclosed element"):
        capture[ParseError](t"<a>".read[Xml]).issue
      . assert: issue =>
          issue match
            case Xml.Issue.Incomplete(t"a") => true
            case _                          => false

      test(m"Unclosed nested element"):
        capture[ParseError](t"<a><b></a>".read[Xml]).issue
      . assert: issue =>
          issue match
            case Xml.Issue.MismatchedTag(t"b", t"a") => true
            case _                                   => false

      test(m"Unknown entity reference"):
        capture[ParseError](t"<a>&unknown;</a>".read[Xml]).issue
      . assert: issue =>
          issue match
            case Xml.Issue.UnknownEntity(t"unknown") => true
            case _                                   => false

      test(m"Literal ]]> in text is rejected"):
        capture[ParseError](t"<a>foo]]>bar</a>".read[Xml])
      . assert(_.issue.isInstanceOf[Xml.Issue.Unexpected])


    suite(m"Position ranges"):
      def focus(input: Text): Text =
        val error = capture[ParseError](input.read[Xml])
        val pos = error.position.asInstanceOf[Xml.Position]
        val start = pos.offset.or(0)
        val length = pos.length.or(0)
        input.s.substring(start, (start + length).min(input.s.length)).nn.tt

      test(m"Mismatched closing tag focus contains close-tag name"):
        focus(t"<a></b>")
      . assert(_.s.contains("b"))

      test(m"Unopened closing tag focus contains close-tag name"):
        focus(t"</a>")
      . assert(_.s.contains("a"))

      test(m"Unknown entity focus contains entity name"):
        focus(t"<a>&unknown;</a>")
      . assert(_.s.contains("unknown"))

      test(m"Duplicate attribute focus contains second attribute name"):
        focus(t"""<a x="1" x="2"/>""")
      . assert(_.s.contains("x"))

      test(m"Position ranges are non-empty for parse errors"):
        capture[ParseError](t"<a></b>".read[Xml])
        . position.asInstanceOf[Xml.Position].length
      . assert(_ != Unset)


    suite(m"Compile-time hole-position errors"):
      test(m"unencodable splice in element body is highlighted at the splice"):
        case class NotEncodable()
        val bad: NotEncodable = NotEncodable()
        demilitarize:
          x"<foo>$bad</foo>"
        . map(_.focus)
      . assert(_ == List("bad"))

      test(m"unrenderable splice in node hole is highlighted at the splice"):
        case class NotShowable()
        val whatever: NotShowable = NotShowable()
        demilitarize:
          x"<foo>$whatever</foo>"
        . map(_.focus)
      . assert(_ == List("whatever"))

    suite(m"Compile-time parse-error sub-positioning"):
      test(m"mismatched close tag in literal lands inside the literal"):
        val errors = demilitarize:
          x"<foo></bar>"
        // Focus should be inside the literal text, NOT cover the whole `x"..."`.
        // The whole literal is 14 chars; a precise focus is shorter.
        errors.map(_.focus.length < 14)
      . assert(_ == List(true))

      test(m"mismatched close tag focus contains close-tag name"):
        demilitarize:
          x"<foo></bar>"
        . map(_.focus)
      . assert(_.headOption.exists(_.contains("bar")))

      test(m"focus is identity-mapped through literal \\n (interpolators don't decode it)"):
        // In an interpolator, `\n` is passed through verbatim — both source
        // and value have 2 chars for `\n` — so the mapping is the identity.
        demilitarize:
          x"<foo>\n</bar>"
        . map(_.focus)
      . assert(_.headOption.exists(_.contains("bar")))

      test(m"focus respects literal é in source (single value char)"):
        // A non-ASCII char in source is one char in value too; identity.
        demilitarize:
          x"<foo>é</bar>"
        . map(_.focus)
      . assert(_.headOption.exists(_.contains("bar")))

      test(m"focus respects \\u#### lexer escape"):
        // The fixture has the literal 6 chars \\u00e9 in source. Whether the
        // subcompiler decodes \\u#### or preserves it raw, the byte-by-byte
        // walker in buildMapping picks the right step.
        demilitarize:
          x"<foo>\u00e9</bar>"
        . map(_.focus)
      . assert(_ == List("bar>"))

      test(m"focus is exact in triple-quoted literal"):
        // Triple-quoted strings don't process escapes, so source ≡ value.
        demilitarize:
          x"""<foo></bar>"""
        . map(_.focus)
      . assert(_.headOption.exists(_.contains("bar")))

      test(m"focus on bad name is exactly the offending span (with escape)"):
        // After `\n` (1 value char, 2 source chars), the `</bar>` close span
        // should still resolve precisely. Parser reports closeStart at the
        // `b` and length 4 (consumes `>`); we expect the substring `bar>`.
        demilitarize:
          x"<foo>\n</bar>"
        . map(_.focus)
      . assert(_ == List("bar>"))

      test(m"focus respects $$$$ escape in literal"):
        // $$ in source decodes to a single $ in the value (2 source chars,
        // 1 value char). The bad close tag after it should still focus
        // precisely on `bar>`.
        demilitarize:
          x"<foo>$$</bar>"
        . map(_.focus)
      . assert(_ == List("bar>"))


    suite(m"Extractor tests"):
      test(m"Extract a text node from an element"):
        val scrutinee: Xml = x"<message>hello</message>"
        scrutinee.absolve match
          case x"<message>$text</message>" => text
      . assert(_ == TextNode(t"hello"))

      test(m"Capture a whole child element"):
        val scrutinee: Xml = x"<a><b>1</b></a>"
        scrutinee.absolve match
          case x"<a>$child</a>" => child
      . assert(_ == x"<b>1</b>")

      test(m"Non-matching tag falls through to the wildcard"):
        val scrutinee: Xml = x"<a>hello</a>"
        scrutinee match
          case x"<b>$text</b>" => true
          case _               => false
      . assert(!_)

      test(m"Zero-hole literal match returns a Boolean result"):
        val scrutinee: Xml = x"<a>hello</a>"
        scrutinee match
          case x"<a>hello</a>" => 1
          case _               => 2
      . assert(_ == 1)

      test(m"Zero-hole literal mismatch falls through"):
        val scrutinee: Xml = x"<a>hello</a>"
        scrutinee match
          case x"<a>goodbye</a>" => 1
          case _                 => 2
      . assert(_ == 2)

      test(m"Capture a child element via a tag hole"):
        val scrutinee: Xml = x"<a><b>1</b></a>"
        scrutinee.absolve match
          case x"<a><$element></a>" => element
      . assert(_ == x"<b>1</b>")

      test(m"Extract an attribute value and a child as a tuple"):
        val scrutinee: Xml = x"""<a id="5"><b>1</b></a>"""
        scrutinee.absolve match
          case x"<a id=$value>$child</a>" => (value, child)
      . assert(_ == (t"5", x"<b>1</b>"))

      test(m"Extract comment content alongside an attribute"):
        val scrutinee: Xml = x"""<a id="1"><!--note--></a>"""
        scrutinee.absolve match
          case x"<a id=$value><!--$text--></a>" => (value, text)
      . assert(_ == (t"1", t"note"))

      test(m"Extract CDATA content alongside an attribute"):
        val scrutinee: Xml = x"""<a id="1"><![CDATA[data]]></a>"""
        scrutinee.absolve match
          case x"<a id=$value><![CDATA[$text]]></a>" => (value, text)
      . assert(_ == (t"1", t"data"))

      test(m"Extract from a nested element"):
        val scrutinee: Xml = x"""<a><b id="9">deep</b></a>"""
        scrutinee.absolve match
          case x"<a><b id=$value>$child</b></a>" => (value, child)
      . assert(_ == (t"9", TextNode(t"deep")))

      test(m"A child-count mismatch falls through"):
        val scrutinee: Xml = x"<a><b/><c/></a>"
        scrutinee match
          case x"<a>$only</a>" => true
          case _               => false
      . assert(!_)

      test(m"Match a literal processing instruction"):
        val scrutinee: Xml = x"<a><?foo bar?>hi</a>"
        scrutinee.absolve match
          case x"<a><?foo bar?>$node</a>" => node
      . assert(_ == TextNode(t"hi"))

      test(m"Capture the remaining attributes as a map"):
        val scrutinee: Xml = x"""<a x="1" y="2">t</a>"""
        scrutinee.absolve match
          case x"<a $attrs>$body</a>" => (attrs, body)
      . assert(_ == (Map(t"x" -> t"1", t"y" -> t"2"), TextNode(t"t")))

      test(m"A literal attribute value that differs falls through"):
        val scrutinee: Xml = x"""<a id="5">x</a>"""
        scrutinee match
          case x"""<a id="6">$body</a>""" => true
          case _                          => false
      . assert(!_)


    suite(m"Compile-time extractor errors"):
      test(m"A single text value cannot be captured on its own"):
        demilitarize:
          (x"""<a id="5">x</a>""": Xml).absolve match
            case x"<a id=$value>x</a>" => value
        . map(_.message)
      . assert(_.exists(_.contains("single text value")))

      test(m"A hole inside comment text is rejected"):
        demilitarize:
          (x"<a/>": Xml).absolve match
            case x"<a><!--pre$middle-->post</a>" => middle
        . map(_.message)
      . assert(_.exists(_.contains("entire comment text")))

      test(m"A hole inside CDATA content is rejected"):
        demilitarize:
          (x"<a/>": Xml).absolve match
            case x"<a><![CDATA[pre$middle]]></a>" => middle
        . map(_.message)
      . assert(_.exists(_.contains("entire CDATA content")))

      test(m"A DOCTYPE pattern is rejected"):
        demilitarize:
          (x"<a/>": Xml) match
            case x"<!DOCTYPE html>" => 1
            case _                  => 2
        . map(_.message)
      . assert(_.exists(_.contains("DOCTYPE")))

      test(m"A hole in processing-instruction position is rejected"):
        demilitarize:
          (x"<a/>": Xml).absolve match
            case x"<a><?$pi?></a>" => pi
      . assert(_.nonEmpty)


    suite(m"Namespaces"):
      test(m"Element with default namespace declaration"):
        t"""<a xmlns="http://example.com"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"xmlns" -> t"http://example.com")))

      test(m"Element with prefixed namespace declaration"):
        t"""<a xmlns:p="http://example.com"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"xmlns:p" -> t"http://example.com")))

      test(m"Prefixed element name"):
        t"<p:a/>".read[Xml]
      . assert(_ == elem(t"p:a"))

      test(m"Prefixed element with prefixed close"):
        t"<p:a></p:a>".read[Xml]
      . assert(_ == elem(t"p:a"))

      test(m"Prefixed attribute name"):
        t"""<a p:b="c"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"p:b" -> t"c")))

      test(m"Element with namespace declaration and prefixed attribute"):
        t"""<a xmlns:p="http://example.com" p:b="c"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"xmlns:p" -> t"http://example.com", t"p:b" -> t"c")))

      test(m"Nested prefixed elements"):
        t"""<root xmlns:h="http://example.com"><h:table><h:tr><h:td>cell</h:td></h:tr></h:table></root>"""
        . read[Xml]
      . assert: result =>
          result == elem(t"root", Map(t"xmlns:h" -> t"http://example.com"),
            elem(t"h:table",
              elem(t"h:tr",
                elem(t"h:td", TextNode(t"cell")))))


    suite(m"DOCTYPE"):
      test(m"Simple DOCTYPE is preserved as a node"):
        t"""<!DOCTYPE root><root/>""".read[Xml]
      . assert(_ == Fragment(Doctype(t"root"), elem(t"root")))

      test(m"DOCTYPE roundtrip"):
        Doctype(t"root").show
      . assert(_ == t"<!DOCTYPE root>")


    suite(m"Roundtrip serialization"):
      test(m"Empty element"):
        t"<a/>".read[Xml].show
      . assert(_ == t"<a/>")

      test(m"Element with text"):
        t"<a>hello</a>".read[Xml].show
      . assert(_ == t"<a>hello</a>")

      test(m"Element with attribute"):
        t"""<a x="1"/>""".read[Xml].show
      . assert(_ == t"""<a x="1"/>""")

      test(m"Element escapes special characters in text"):
        elem(t"a", TextNode(t"<&>")).show
      . assert(_ == t"<a>&lt;&amp;&gt;</a>")

      test(m"Element escapes special characters in attribute"):
        elem(t"a", Map(t"x" -> t"\"&'<")).show
      . assert(_ == t"""<a x="&quot;&amp;'&lt;"/>""")

      test(m"Comment roundtrip"):
        elem(t"a", Comment(t" hello ")).show
      . assert(_ == t"<a><!-- hello --></a>")

      test(m"CDATA roundtrip"):
        elem(t"a", Cdata(t"raw <data>")).show
      . assert(_ == t"<a><![CDATA[raw <data>]]></a>")

      test(m"Processing instruction roundtrip"):
        elem(t"a", ProcessingInstruction(t"target", t"data")).show
      . assert(_ == t"<a><?target data?></a>")

      test(m"Empty PI roundtrip"):
        elem(t"a", ProcessingInstruction(t"target", t"")).show
      . assert(_ == t"<a><?target?></a>")

      test(m"Header without encoding or standalone roundtrips"):
        Header(t"1.0", Unset, Unset).show
      . assert(_ == t"""<?xml version="1.0"?>""")

      test(m"Header with encoding roundtrips"):
        Header(t"1.0", t"UTF-8", Unset).show
      . assert(_ == t"""<?xml version="1.0" encoding="UTF-8"?>""")

      test(m"Header with standalone roundtrips"):
        Header(t"1.0", Unset, true).show
      . assert(_ == t"""<?xml version="1.0" standalone="yes"?>""")

    suite(m"Serializer formatting"):
      test(m"Compact is the default (no indentation, no trailing newline)"):
        elem(t"a", elem(t"b"), elem(t"c")).show
      . assert(_ == t"<a><b/><c/></a>")

      test(m"Indented formatting lays out element children one per line"):
        import formatting.indentedXmlFormatting
        elem(t"a", elem(t"b"), elem(t"c")).show
      . assert(_ == t"<a>\n  <b/>\n  <c/>\n</a>\n")

      test(m"Indented formatting nests deeper levels"):
        import formatting.indentedXmlFormatting
        elem(t"a", elem(t"b", elem(t"c"))).show
      . assert(_ == t"<a>\n  <b>\n    <c/>\n  </b>\n</a>\n")

      test(m"Indented formatting keeps character data inline"):
        import formatting.indentedXmlFormatting
        elem(t"a", TextNode(t"hello")).show
      . assert(_ == t"<a>hello</a>\n")

      test(m"emit indents a document and adds the header and trailing newlines"):
        import formatting.indentedXmlFormatting
        val document = Document[Xml](elem(t"a", elem(t"b")), Header(t"1.0", Unset, Unset))
        supervise(Xml.emit(document).to(List).join)
      . assert(_ == t"<?xml version=\"1.0\"?>\n<a>\n  <b/>\n</a>\n")


    suite(m"Documents with prologs"):
      test(m"Document with comment in prolog"):
        supervise:
          t"""<?xml version="1.0"?><!--prolog comment--><a/>""".load[Xml]
      . assert: doc =>
          doc == Document(
            Fragment(Comment(t"prolog comment"), elem(t"a")),
            Header(t"1.0", Unset, Unset))

      test(m"Document with PI in prolog"):
        supervise:
          t"""<?xml version="1.0"?><?stylesheet href="x"?><a/>""".load[Xml]
      . assert: doc =>
          doc == Document(
            Fragment(ProcessingInstruction(t"stylesheet", t"""href="x""""), elem(t"a")),
            Header(t"1.0", Unset, Unset))

      test(m"Document with whitespace in prolog"):
        supervise:
          t"""<?xml version="1.0"?>\n<a/>""".load[Xml]
      . assert: doc =>
          doc == Document(elem(t"a"), Header(t"1.0", Unset, Unset))


    suite(m"Headerless documents"):
      test(m"Single root element without XML declaration"):
        supervise:
          t"<a/>".load[Xml]
      . assert(_ == Document(elem(t"a"), Xml.header))

      test(m"Root element with content but no XML declaration"):
        supervise:
          t"<a>hello</a>".load[Xml]
      . assert(_ == Document(elem(t"a", TextNode(t"hello")), Xml.header))

      test(m"Nested elements without XML declaration"):
        supervise:
          t"<a><b/></a>".load[Xml]
      . assert(_ == Document(elem(t"a", elem(t"b")), Xml.header))

      test(m"Headerless document with prolog comment"):
        supervise:
          t"<!--prolog comment--><a/>".load[Xml]
      . assert: doc =>
          doc == Document(
            Fragment(Comment(t"prolog comment"), elem(t"a")),
            Xml.header)

      test(m"Headerless document with prolog PI"):
        supervise:
          t"""<?stylesheet href="x"?><a/>""".load[Xml]
      . assert: doc =>
          doc == Document(
            Fragment(ProcessingInstruction(t"stylesheet", t"""href="x""""), elem(t"a")),
            Xml.header)

      test(m"Headerless document with DOCTYPE"):
        supervise:
          t"<!DOCTYPE a><a/>".load[Xml]
      . assert: doc =>
          doc == Document(
            Fragment(Doctype(t"a"), elem(t"a")),
            Xml.header)

      test(m"Lone XML declaration is rejected"):
        supervise:
          capture[ParseError](t"""<?xml version="1.0"?>""".load[Xml]).issue
      . assert: issue =>
          issue match
            case Xml.Issue.BadDocument => true
            case _                     => false


    suite(m"Additional element tests"):
      test(m"Element with multiple attributes preserves order"):
        t"""<a x="1" y="2" z="3"/>""".read[Xml].absolve match
          case Element(_, attributes, _) => attributes.toList.map(_(0))
      . assert(_ == List(t"x", t"y", t"z"))

      test(m"Tab and newline allowed in attribute value (normalized)"):
        t"""<a x="line one"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"line one")))

      test(m"Empty element with attribute"):
        t"""<a x="1"></a>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"1")))

      test(m"Multiple comments inside element"):
        t"<a><!--c1--><!--c2--></a>".read[Xml]
      . assert(_ == elem(t"a", Comment(t"c1"), Comment(t"c2")))

      test(m"Comment inside nested element"):
        t"<a><b><!--c--></b></a>".read[Xml]
      . assert(_ == elem(t"a", elem(t"b", Comment(t"c"))))


    suite(m"Additional attribute tests"):
      test(m"Multiple attributes with mixed quoting"):
        t"""<a x='1' y="2"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"1", t"y" -> t"2")))

      test(m"Attribute value with multiple entities"):
        t"""<a x="&lt;&amp;&gt;"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"x" -> t"<&>")))

      test(m"Attribute value containing literal < is rejected"):
        capture[ParseError](t"""<a x="<"/>""".read[Xml])
      . assert(_.issue.isInstanceOf[Xml.Issue.Unexpected])


    suite(m"Additional CDATA tests"):
      test(m"Multiple CDATA sections"):
        t"<a><![CDATA[one]]><![CDATA[two]]></a>".read[Xml]
      . assert(_ == elem(t"a", Cdata(t"one"), Cdata(t"two")))

      test(m"CDATA next to text"):
        t"<a>before<![CDATA[mid]]>after</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"before"), Cdata(t"mid"), TextNode(t"after")))


    suite(m"Additional entity tests"):
      test(m"Decimal char ref for newline"):
        t"<a>&#10;</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"\n")))

      test(m"Hex char ref for tab"):
        t"<a>&#x9;</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"\t")))

      test(m"Entity at start of text"):
        t"<a>&amp;rest</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"&rest")))

      test(m"Entity at end of text"):
        t"<a>start&amp;</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"start&")))

      test(m"Only an entity in text"):
        t"<a>&amp;</a>".read[Xml]
      . assert(_ == elem(t"a", TextNode(t"&")))

      test(m"Entity in nested element"):
        t"<a><b>&lt;</b></a>".read[Xml]
      . assert(_ == elem(t"a", elem(t"b", TextNode(t"<"))))


    suite(m"Additional namespace tests"):
      test(m"Multiple namespace declarations on one element"):
        t"""<a xmlns:p="uri-p" xmlns:q="uri-q"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"xmlns:p" -> t"uri-p", t"xmlns:q" -> t"uri-q")))

      test(m"Default and prefixed namespace together"):
        t"""<a xmlns="uri-default" xmlns:p="uri-p"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"xmlns" -> t"uri-default", t"xmlns:p" -> t"uri-p")))

      test(m"xml:lang attribute is preserved"):
        t"""<a xml:lang="en"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"xml:lang" -> t"en")))

      test(m"xml:space attribute is preserved"):
        t"""<a xml:space="preserve"/>""".read[Xml]
      . assert(_ == elem(t"a", Map(t"xml:space" -> t"preserve")))

      test(m"Element with prefixed name preserves case"):
        t"<P:Tag/>".read[Xml]
      . assert(_ == elem(t"P:Tag"))

      test(m"Mismatched prefixed close tag"):
        capture[ParseError](t"<p:a></q:a>".read[Xml]).issue
      . assert: issue =>
          issue match
            case Xml.Issue.MismatchedTag(t"p:a", t"q:a") => true
            case _                                       => false


    suite(m"Additional DOCTYPE tests"):
      test(m"DOCTYPE with system identifier"):
        t"""<!DOCTYPE root SYSTEM "url"><root/>""".read[Xml]
      . assert(_ == Fragment(Doctype(t"""root SYSTEM "url""""), elem(t"root")))

      test(m"DOCTYPE before XML declaration not present"):
        supervise:
          t"""<?xml version="1.0"?><!DOCTYPE root><root/>""".load[Xml]
      . assert: doc =>
          doc == Document(
            Fragment(Doctype(t"root"), elem(t"root")),
            Header(t"1.0", Unset, Unset))


    suite(m"Document load tests"):
      test(m"Document with single root element"):
        supervise:
          t"""<?xml version="1.0"?><root>content</root>""".load[Xml]
      . assert: doc =>
          doc == Document(
            elem(t"root", TextNode(t"content")),
            Header(t"1.0", Unset, Unset))

      test(m"Document with nested root"):
        supervise:
          t"""<?xml version="1.0"?><root><child/></root>""".load[Xml]
      . assert: doc =>
          doc == Document(
            elem(t"root", elem(t"child")),
            Header(t"1.0", Unset, Unset))

    suite(m"xp\"...\" interpolator"):
      test(m"an absolute path with ordinals parses"):
        xp"/root[1]/child[2]".encode
      . assert(_ == t"/root[1]/child[2]")

      test(m"an attribute step parses"):
        xp"/root[1]/@id".encode
      . assert(_ == t"/root[1]/@id")

      test(m"a step without an ordinal defaults to [1]"):
        xp"/root/child".encode
      . assert(_ == t"/root[1]/child[1]")

      test(m"the root path parses"):
        xp"/".encode
      . assert(_ == t"/")

      test(m"a path not beginning with '/' is rejected at the first character"):
        demilitarize:
          xp"root/child"
        . map(_.focus)
      . assert(_ == List("r"))

      test(m"an empty step is rejected at the offending separator"):
        demilitarize:
          xp"/root//child"
        . map(_.focus)
      . assert(_ == List("/"))

      test(m"a non-numeric ordinal is rejected"):
        demilitarize:
          xp"/root[x]"
        . map(_.focus.nonEmpty)
      . assert(_ == List(true))

    suite(m"HTTP content-type integration"):
      import charEncoders.utf8Encoder

      test(m"serialises with an application/xml media type"):
        x"<doc/>".generic(0)
      . assert(_.starts(t"application/xml"))

      test(m"request body parses back via Instantiable"):
        val instantiable = summon[Xml is Instantiable across HttpRequests from Text]
        instantiable(t"<doc><a>1</a></doc>").show
      . assert(_ == t"<doc><a>1</a></doc>".read[Xml].show)

    suite(m"Dynamic access"):
      test(m"Dynamic dereference selects a child element's text"):
        import dynamicXmlAccess.enabled
        t"<root><foo>bar</foo></root>".read[Xml].foo().as[Text]
      . assert(_ == t"bar")

      test(m"Chained dynamic dereference navigates nested elements"):
        import dynamicXmlAccess.enabled
        t"<a><b><c>42</c></b></a>".read[Xml].b().c().as[Int]
      . assert(_ == 42)

      test(m"Dynamic dereference flattens non-unique tags into a Fragment"):
        import dynamicXmlAccess.enabled
        t"<r><x>1</x><x>2</x></r>".read[Xml].x.nodes.length
      . assert(_ == 2)

      test(m"Empty-parens dereference is the same as `(Prim)`"):
        import dynamicXmlAccess.enabled
        val xml = t"<r><x>1</x><x>2</x></r>".read[Xml]
        xml.x() == xml.x(Prim)
      . assert(_ == true)

      test(m"An ordinal selects the nth matching element"):
        import dynamicXmlAccess.enabled
        t"<r><x>1</x><x>2</x></r>".read[Xml].x(Sec).as[Int]
      . assert(_ == 2)

      test(m"Dynamic dereference flattens across multiple parents"):
        import dynamicXmlAccess.enabled
        t"<r><a><b>1</b><b>2</b></a><a><b>3</b></a></r>".read[Xml].a.b.nodes.length
      . assert(_ == 3)

      test(m"A missing tag yields an empty Fragment"):
        import dynamicXmlAccess.enabled
        t"<r><x>1</x></r>".read[Xml].nope.nodes.isEmpty
      . assert(_ == true)

      test(m"An out-of-range ordinal yields an empty Fragment"):
        import dynamicXmlAccess.enabled
        t"<r><x>1</x></r>".read[Xml].x(Sec).nodes.isEmpty
      . assert(_ == true)

      test(m"Dynamic access does not compile without the enabler import"):
        demilitarize:
          t"<r><x>1</x></r>".read[Xml].x()
        . nonEmpty
      . assert(identity)

    suite(m"Optics"):
      import dynamicXmlAccess.enabled
      def doc: Xml = t"<doc><x>1</x><x>2</x><x>3</x></doc>".read[Xml]

      test(m"lens replaces the first matching child element"):
        doc.lens(_.x = x"<x>9</x>").show
      . assert(_ == t"<doc><x>9</x><x>2</x><x>3</x></doc>")

      test(m"lens appends when no matching child exists"):
        doc.lens(_.y = x"<y>5</y>").show
      . assert(_ == t"<doc><x>1</x><x>2</x><x>3</x><y>5</y></doc>")

      test(m"lens navigates and updates a nested element"):
        t"<doc><item><name>a</name></item></doc>".read[Xml]
         .lens(_.item.name = x"<name>b</name>").show
      . assert(_ == t"<doc><item><name>b</name></item></doc>")

      test(m"ordinal optic replaces the n-th child element"):
        doc.lens(_(Sec) = x"<x>9</x>").show
      . assert(_ == t"<doc><x>1</x><x>9</x><x>3</x></doc>")

      test(m"each optic replaces every child element"):
        doc.lens(_(Each) = x"<x>0</x>").show
      . assert(_ == t"<doc><x>0</x><x>0</x><x>0</x></doc>")

      test(m"an out-of-range ordinal is a no-op"):
        doc.lens(_(Sen) = x"<x>9</x>").show
      . assert(_ == t"<doc><x>1</x><x>2</x><x>3</x></doc>")

    PositionTests()
    DecoderTests()
    EncoderTests()
