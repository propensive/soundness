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
import threading.virtual
import codicils.cancel

case class Worker(name: Text, age: Int)
case class Firm(name: Text, ceo: Worker)

case class Book(title: Text, @attribute isbn: Text)
case class Bibliography(author: Text, book: Book)

enum ColorVal:
  case Rgb(red: Int, green: Int, blue: Int)
  case Cmyk(cyan: Int, magenta: Int, yellow: Int, key: Int)

case class Pixel(x: Int, y: Int, color: ColorVal)

object Tests extends Suite(m"Xylophone tests"):

  def elem(label: Text, children: Node*): Element =
    Element(label, scala.collection.immutable.ListMap(), IArray.from(children))

  def elem(label: Text, attrs: Map[Text, Text], children: Node*): Element =
    Element(label, attrs, IArray.from(children))

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
          unsafely(t"""<?xml  version="1.0"?><message>hello world</message>""".load[Xml].read[Text])
      . assert(_ == t"""<?xml version="1.0"?><message>hello world</message>""")

    test(m"extract integer"):
      x"""<message>1</message>""".as[Int]
    . assert(_ == 1)

    test(m"fail to extract bad integer"):
      capture[NumberError](x"""<message>ABC</message>""".as[Int])
    . assert(_ == NumberError("ABC", Int, NumberError.Reason.Unparseable))

    test(m"extract email address"):
      x"""<email>test@example.com</email>""".as[EmailAddress]
    . assert(_ == t"test@example.com".decode[EmailAddress])

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
      . assert(_ == t"<a>&lt;&amp;></a>")

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


    suite(m"Additional element tests"):
      test(m"Element with multiple attributes preserves order"):
        t"""<a x="1" y="2" z="3"/>""".read[Xml].absolve match
          case Element(_, attributes, _) => attributes.to(List).map(_(0))
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
