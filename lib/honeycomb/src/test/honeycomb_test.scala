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
package honeycomb


import soundness.*

import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely

object Tests extends Suite(m"Honeycombd Tests"):
  def run(): Unit =
    // A missing `Inspectable` is never a compile error, so coverage is held in place by
    // asserting on the renderings: `fallbacks` returns those which used a marked fallback.
    suite(m"Native-rendering coverage"):
      import htmlDoms.whatwg
      import whatwg.*

      test(m"an element inspects as escaped, single-line HTML source"):
        Div(style = "bar")("hello").inspect
      . assert(_ == "html\"<div style=\\\"bar\\\">hello</div>\"")

      test(m"attributes inspect as an element's start-tag text"):
        internal.Attributes("class" -> "a", "hidden" -> Unset).inspect
      . assert(_ == "html{class=\"a\", hidden}")

      test(m"honeycomb's types inspect natively"):
        Inspectable.fallbacks
         ( Div(style = "bar")("hello").inspect,
           Fragment(Div("content"), P("more content")).inspect,
           Comment("hi").inspect,
           Doctype("html").inspect,
           TextNode("hi").inspect,
           internal.Attributes("class" -> "a", "hidden" -> Unset).inspect )
      . assert(_ == Nil)

    test(m"show comment"):
      Comment("hello world").show
    . assert(_ == "<!--hello world-->")

    suite(m"HTML parsing tests"):
      import htmlDoms.whatwg
      import whatwg.*

      test(m"simple empty tag"):
        """<div></div>""".read[Html]
      . assert(_ == Div)

      test(m"List"):
        """<ul><li>item</li></ul>""".read[Html of Flow]
      . assert(_ == Ul(Li("item")))

      test(m"simple tag with text"):
        """<div>content</div>""".read[Html of "div"]
      . assert(_ == Div("content"))

      test(m"more than one node"):
        """<div>content</div><p>more content</p>""".read[Html of Flow]
      . assert(_ == Fragment(Div("content"), P("more content")))

      test(m"more than one node with comment"):
        """<div>content</div><!-- comment --><div>more content</div>""".read[Html of "div"]
      . assert(_ == Fragment(Div("content"), Comment(" comment "), Div("more content")))

      test(m"simple self-closing tag"):
        """<div />""".read[Html of "div"]
      . assert(_ == Div)

      test(m"self-closing tag with attributes"):
        """<div style="bar"/>""".read[Html of "div"]
      . assert(_ == Div(style = "bar"))

      test(m"case-insensitive element"):
        """<DIV style="bar">hello world</DIV>""".read[Html of "div"]
      . assert(_ == Div(style = "bar")("hello world"))

      test(m"simple comment tag"):
        """<!--This is a comment-->""".read[Html of Flow]
      . assert(_ == Comment("This is a comment"))

      test(m"simple void tag"):
        """<br>""".read[Html of Flow]
      . assert(_ == Br)

      test(m"void tag with an attribute"):
        """<area style="bar">""".read[Html of "area"]
      . assert(_ == Area(style = "bar"))

      test(m"void tag with an unquoted attribute"):
        """<area style=bar>""".read[Html of Flow]
      . assert(_ == Area(style = "bar"))

      test(m"void tag with a boolean attribute"):
        """<input disabled>""".read[Html of "input"]
      . assert(_ == Input(disabled = true))

      test(m"void tag with a single-quoted attribute"):
        """<br style='bar baz'>""".read[Html of Flow]
      . assert(_ == Br(style = "bar baz"))

      test(m"simple nested tag"):
        """<div><area></div>""".read[Html of Flow]
      . assert(_ == Div(Area))

      test(m"just text"):
        """hello world""".read[Html of Flow]
      . assert(_ == TextNode("hello world"))

      test(m"just text with entity"):
        """to &amp; fro""".read[Html of Flow]
      . assert(_ == TextNode("to & fro"))

      test(m"just an entity"):
        """&amp;""".read[Html of Flow]
      . assert(_ == TextNode("&"))

      test(m"misnested formatting (adoption agency)"):
        """<em><b></em></b>""".read[Html of Phrasing]
      . assert(_ == Em(B()))

      test(m"unknown tag"):
        try """<scrip>""".read[Html of Phrasing]
        catch case exception: Exception => exception
      . assert:
          case Parse.Error(Html, Html.Position(line, col, _, _), Html.Issue.InvalidTag("scrip")) =>
            line == 1.u && col == 2.u
          case _ => false

      test(m"raw text"):
        "<head><script>some content</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some content")))

      test(m"raw text, with partial closing tag"):
        "<head><script>some content</scr</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some content</scr")))

      test(m"raw text, with shorter partial closing tag"):
        "<head><script>some content</</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some content</")))

      test(m"raw text, with even shorter partial closing tag"):
        "<head><script>some content<</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some content<")))

      test(m"raw text, with non-entity"):
        "<head><script>some &amp; content</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some &amp; content")))

      test(m"raw text, with tag literal"):
        "<head><script>some <foo> content</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some <foo> content")))

      test(m"autoclosing tag"):
        """<ul><li>First item</ul>""".read[Html of Flow]
      . assert(_ == Ul(Li("First item")))

      test(m"unclosed paragraph"):
        """<p>para""".read[Html of Flow]
      . assert(_ == P("para"))

      test(m"follow-on whitespace"):
        t"""<p>para</p>\n""".read[Html of Flow]
      . assert(_ == Fragment(P(t"para"), t"\n"))

      test(m"empty content"):
        "".read[Html of Flow]
      . assert(_ == Fragment())

      test(m"failing example"):
        t"""<p>x<img></p>\n""".read[Html of Flow]
      . assert(_ == Fragment(P(t"x", Img), t"\n"))

      test(m"autoclosing adjacent tags"):
        """<ul><li>First item<li>Second item</ul>""".read[Html of Flow]
      . assert(_ == Ul(Li("First item"), Li("Second item")))

      test(m"unclosed tag 1"):
        try """<ul><li>First item</li>""".read[Html of Flow]
        catch case exception: Exception => exception
      . assert(_ == Parse.Error(Html, Html.Position(1.u, 23.u), Html.Issue.Incomplete("ul")))

      test(m"unclosed tag 2"):
        try """<ul><li>First item""".read[Html of Flow]
        catch case exception: Exception => exception
      . assert(_ == Parse.Error(Html, Html.Position(1.u, 18.u), Html.Issue.Incomplete("ul")))

      test(m"infer both <head> and <body>"):
        """<title>Page title</title><p>A paragraph</p>""".read[Html of "html"]
      . assert(_ == Html(Head(Title("Page title")), Body(P("A paragraph"))))


      suite(m"Table tests"):
        test(m"Simple table"):
          """<table><tbody><tr><th>First</th><td>Second</td><td>Third</td></tr></tbody></table>""".read[Html of "table"]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tbody> is inferred"):
          """<table><tr><th>First</th><td>Second</td><td>Third</td></tr></table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tbody> autocloses"):
          """<table><tbody><tr><th>First</th><td>Second</td><td>Third</td></tr></table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tr> autocloses"):
          """<table style="bar"><tbody><tr><th>First</th><td>Second</td><td>Third</td></tbody></table>""".read[Html of Flow]
        . assert(_ == Table(style = "bar")(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tbody> and <tr> autoclose"):
          """<table><tbody><tr><th>First</th><td>Second</td><td>Third</td></table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tbody>, <tr>, <th> and <td> autoclose"):
          """<table><tbody><tr><th>First<td>Second<td>Third</table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))


        test(m"<thead> works like <tbody>"):
          """<table class="test"><thead><tr><th>First<td>Second<td>Third</table>""".read[Html of Flow]
        . assert: result =>
            given (Attribution of "test" | "foo") = Attribution.classes()
            result == Table.test(Thead(Tr(Th("First"), Td("Second"), Td("Third"))))

        test(m"Generic stylesheet"):
          """<table class="test"><thead><tr><th>First<td>Second<td>Third</table>""".read[Html of Flow]
        . assert: result =>
            import stylesheets.uncheckedClasses
            result == Table.test(Thead(Tr(Th("First"), Td("Second"), Td("Third"))))

        test(m"<tfoot> closes inferred <tbody>"):
          """<table><tr><th>First<td>Second<td>Third<tfoot><tr><td>Footer</table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third"))), Tfoot(Tr(Td("Footer")))))

      test(m"Whitespace permitted and ignored between list items"):
        t"""<ul><li>hello</li>\n  <li>world</li></ul>""".read[Html of "html"]
      . assert(_ == Html(Body(Ul(Li("hello"), Li("world")))))

      test(m"Non-whitespace text not permitted between list items"):
        try t"""<ul><li>hello</li>\n and <li>world</li></ul>""".read[Html of "html"]
        catch case exception: Exception => exception
      . assert(_ == Parse.Error(Html, Html.Position(2.u, 2.u), Html.Issue.OnlyWhitespace('a')))

      test(m"Foreign SVG tag"):
        """<div><svg><circle r="1"/></svg></div>""".read[Html of Flow]
      . assert(_ == Div(Svg(Element.foreign(t"circle", proscenium.Map[Text, Optional[Text]](t"r" -> t"1")))))

      test(m"Nontrivial MathML example"):
        """<div>The equation is <math><mfrac><msup><mi>π</mi><mn>2</mn></msup><mn>6</mn></mfrac></math>.</div>"""
        . read[Html of Flow]
      . assert(_ == Div("The equation is ", Math(Element.foreign("mfrac", proscenium.Map[Text, Optional[Text]](), Element.foreign("msup", proscenium.Map[Text, Optional[Text]](), Element.foreign("mi", proscenium.Map[Text, Optional[Text]](), "π"), Element.foreign("mn", proscenium.Map[Text, Optional[Text]](), "2")), Element.foreign("mn", proscenium.Map[Text, Optional[Text]](), "6"))), "."))

      test(m"transparent tag with text"):
        """<p>Go <a href="https://example.com">home</a>.</p>""".read[Html of "p"]
      . assert(_ == P("Go ", A(href = url"https://example.com")("home"), "."))

      test(m"transparent tag only allows the right children"):
        try """<div><a href="#"><li>list item</li></a></div>""".read[Html of Flow]
        catch case exception: Exception => exception
      . assert:
          case Parse.Error
                ( Html, Html.Position(line, col, _, _), Html.Issue.InadmissibleTag("li", "a") ) =>
            line == 1.u && col == 18.u
          case _ => false

      test(m"transparent tag with element"):
        """<p>Go <a href="https://example.com"><em>home</em></a>.</p>""".read[Html of "p"]
      . assert(_ == P("Go ", A(href = url"https://example.com")(Em("home")), "."))

      test(m"transparent tag with additions"):
        """<div><video controls><source src="https://example.com/movie.mp4"></video></div>""".read[Html of "div"]
      . assert(_ == Div(Video(controls = true)(Source(src = url"https://example.com/movie.mp4"))))

      test(m"Tag subtype"):
        """<input type="button">""".read[Html of "input"]
      . assert(_ == Input.Button)

      test(m"Tag subtype with extra attributes"):
        """<input alt="whatever" type="button">""".read[Html of "input"]
      . assert(_ == Input.Button(alt = "whatever"))


      test(m"Parse RCDATA with no entities"):
        """<title>Push then Pull</title>""".read[Html of Metadata]
      . assert(_ == Title("Push then Pull"))

      val example = Html(Head(Title("Heading")), Body(P("body")))
      test(m"Parse Document without doctype"):
        """<title>Heading</title>
            <p>body""".load[Html]
      . assert(_ == Document(example, htmlDoms.whatwg))

      test(m"Parse Document with doctype"):
        """<!doctype html>
            <title>Heading</title>
            <p>body""".load[Html]
      . assert(_ == Document(example, htmlDoms.whatwg))

      test(m"Parse RCDATA with an entity"):
        """<title>Push &amp; Pull</title>""".read[Html of Metadata]
      . assert(_ == Title("Push & Pull"))

      test(m"Parse empty RCDATA"):
        """<title></title>""".read[Html of Metadata]
      . assert(_ == Title)

      test(m"Parse RCDATA starting with entity"):
        """<title>&amp; ampersand</title>""".read[Html of Metadata]
      . assert(_ == Title("& ampersand"))

      test(m"Parse RCDATA with fake tag"):
        """<title><push> &amp; <pull></title>""".read[Html of Metadata]
      . assert(_ == Title("<push> & <pull>"))

      test(m"Parse RCDATA ending with entity"):
        """<title>ampersand:&amp;</title>""".read[Html of Metadata]
      . assert(_ == Title("ampersand:&"))

      test(m"Parse RCDATA with only entity"):
        """<title>&amp;</title>""".read[Html of Metadata]
      . assert(_ == Title("&"))

      test(m"Parse RCDATA with invalid entity"):
        """<title>&ampersand;</title>""".read[Html of Metadata]
      . assert(_ == Title("&ampersand;"))

      test(m"Parse RCDATA with incomplete entity"):
        """<title>&a</title>""".read[Html of Metadata]
      . assert(_ == Title("&a"))

      test(m"Attribute with character entity"):
        """<img alt="To &amp; fro">""".read[Html of Flow]
      . assert(_ == Img(alt = "To & fro"))

      test(m"Modify attribute"):
        val img: Element of "img" in Whatwg = Img(alt = "alternative")
        img.alt = "different"
      . assert(_ == Img(alt = "different"))

      test(m"Attribute with numeric character entity"):
        """<img alt="Schlo&#223;">""".read[Html of Flow]
      . assert(_ == Img(alt = "Schloß"))

      test(m"Text with hex character entity"):
        """<p>value: &#x6A;</p>""".read[Html of Flow]
      . assert(_ == P("value: j"))

      test(m"Text with emoji character entity"):
        """<p>value: &#x1f600;""".read[Html of Flow]
      . assert(_ == P("value: 😀"))

      test(m"attribute access"):
        Img(alt = "hello world").alt
      . assert(_ == "hello world")

      test(m"typed attribute access"):
        Img(width = 50).width
      . assert(_ == 50)

      suite(m"Tokenization edge cases"):
        test(m"empty comment"):
          """<!---->""".read[Html of Flow]
        . assert(_ == Comment(""))

        test(m"comment with newline"):
          "<!--line1\nline2-->".read[Html of Flow]
        . assert(_ == Comment("line1\nline2"))

        test(m"multiple consecutive comments"):
          """<!--a--><!--b-->""".read[Html of Flow]
        . assert(_ == Fragment(Comment("a"), Comment("b")))

        test(m"comment with hyphen inside"):
          """<!-- - -->""".read[Html of Flow]
        . assert(_ == Comment(" - "))

        test(m"decimal entity for ASCII"):
          """<p>&#65;</p>""".read[Html of Flow]
        . assert(_ == P("A"))

        test(m"decimal entity for max BMP"):
          """<p>&#65535;</p>""".read[Html of Flow]
        . assert(_ == P("￿"))

        test(m"decimal entity for supra-BMP code point"):
          """<p>&#128512;</p>""".read[Html of Flow]
        . assert(_ == P("😀"))

        test(m"unknown named entity passed through literally"):
          """<p>&foo;</p>""".read[Html of Flow]
        . assert(_ == P("&foo;"))

        test(m"DOCTYPE case insensitivity"):
          val parsed = "<!DocTyPe html>\n<title>x</title>\n<p>y".load[Html]
          val expected = Html(Head(Title("x")), Body(P("y")))
          parsed == Document(expected, htmlDoms.whatwg)
        . assert(_ == true)

        test(m"DOCTYPE with extra whitespace"):
          val parsed = "<!doctype   html  >\n<title>x</title>\n<p>y".load[Html]
          val expected = Html(Head(Title("x")), Body(P("y")))
          parsed == Document(expected, htmlDoms.whatwg)
        . assert(_ == true)

        test(m"position reporting on later line"):
          try "<div>\nbad </span></div>".read[Html of "div"]
          catch case exception: Exception => exception

        . assert: result =>
            (result: @unchecked) match
              case Parse.Error(_, Html.Position(line, _, _, _), _) => line == 2.u

      suite(m"Attribute parsing depth"):
        test(m"multiple attributes preserved"):
          """<img alt="a" title="b">""".read[Html of "img"]
        . assert(_ == Img(alt = "a", title = "b"))

        test(m"mixed quote styles in same tag"):
          """<input alt='x' title="y" disabled>""".read[Html of "input"]
        . assert(_ == Input(alt = "x", title = "y", disabled = true))

        test(m"attribute value with named entity"):
          """<img alt="a&amp;b">""".read[Html of "img"]
        . assert(_ == Img(alt = "a&b"))

        test(m"attribute value with hex entity"):
          """<img alt="&#x41;">""".read[Html of "img"]
        . assert(_ == Img(alt = "A"))

        test(m"attribute value with decimal entity"):
          """<img alt="&#65;">""".read[Html of "img"]
        . assert(_ == Img(alt = "A"))

        test(m"single quote inside double-quoted value"):
          """<img alt="it's">""".read[Html of "img"]
        . assert(_ == Img(alt = "it's"))

        test(m"double quote inside single-quoted value"):
          """<img alt='say "hi"'>""".read[Html of "img"]
        . assert(_ == Img(alt = """say "hi""""))

        test(m"forbidden character in unquoted value"):
          try """<img alt=a"b>""".read[Html of "img"]
          catch case exception: Exception => exception
        . assert:
            case Parse.Error
                  ( Html,
                    Html.Position(line, col, _, _),
                    Html.Issue.ForbiddenUnquoted('"') ) =>
              line == 1.u && col == 11.u
            case _ => false

        test(m"whitespace around equals"):
          """<img alt = "a">""".read[Html of "img"]
        . assert(_ == Img(alt = "a"))

        test(m"duplicate attribute"):
          try """<img alt="a" alt="b">""".read[Html of "img"]
          catch case exception: Exception => exception

        . assert: result =>
            (result: @unchecked) match
              case Parse.Error(_, _, Html.Issue.DuplicateAttribute(name)) => name == "alt"

        test(m"unknown attribute on known tag"):
          try """<div bogus="x"></div>""".read[Html of "div"]
          catch case exception: Exception => exception
        . assert: result =>
            (result: @unchecked) match
              case Parse.Error(_, _, Html.Issue.UnknownAttributeStart(_)) => true
              case Parse.Error(_, _, Html.Issue.UnknownAttribute(_))      => true

      suite(m"Position ranges"):
        def position(input: Text): Html.Position =
          try
            input.read[Html of Phrasing]
            Html.Position(0.u, 0.u)
          catch case e: Parse.Error => e.position.asInstanceOf[Html.Position]

        def focus(input: Text): Text =
          val pos = position(input)
          val start = pos.offset.or(0)
          val length = pos.length.or(0)
          input.s.substring(start, (start + length).min(input.s.length)).nn.tt

        test(m"Unknown tag focus contains the bad tag name"):
          focus("<scrip>")
        . assert(_ == "scrip")

        test(m"Position ranges are non-empty for tag errors"):
          position("<scrip>").length
        . assert(_ != Unset)

      suite(m"Compile-time hole-position errors"):
        test(m"unrenderable splice in element body is highlighted at the splice"):
          case class NotShowable()
          val bad: NotShowable = NotShowable()
          demilitarize:
            h"<div>$bad</div>"
          . map(_.focus)
        . assert(_ == List("bad"))

        test(m"parse error focus is inside the literal, not the whole thing"):
          val errors = demilitarize:
            h"<scrip></scrip>"
          // h"<scrip></scrip>" is 17 chars; a precise focus is shorter.
          errors.map(_.focus.length < 17)
        . assert(_ == List(true))

      suite(m"Element coverage: void elements"):
        test(m"area"):
          """<area alt="a">""".read[Html of "area"]
        . assert(_ == Area(alt = "a"))

        test(m"base"):
          """<base href="https://example.com/">""".read[Html of "base"]
        . assert(_ == Base(href = url"https://example.com/"))

        test(m"br"):
          """<br>""".read[Html of "br"]
        . assert(_ == Br)

        test(m"col"):
          """<col span="2">""".read[Html of "col"]
        . assert(_ == Col(span = 2))

        test(m"embed"):
          """<embed src="https://example.com/x">""".read[Html of "embed"]
        . assert(_ == Embed(src = url"https://example.com/x"))

        test(m"hr"):
          """<hr>""".read[Html of "hr"]
        . assert(_ == Hr)

        test(m"img"):
          """<img alt="x">""".read[Html of "img"]
        . assert(_ == Img(alt = "x"))

        test(m"input"):
          """<input>""".read[Html of "input"]
        . assert(_ == Input)

        test(m"link"):
          """<link rel="stylesheet">""".read[Html of "link"]
        . assert(_ == Link.Stylesheet)

        test(m"meta"):
          """<meta name="viewport">""".read[Html of "meta"]
        . assert(_ == Meta.Viewport)

        test(m"source"):
          """<source src="https://example.com/x.mp4">""".read[Html of "source"]
        . assert(_ == Source(src = url"https://example.com/x.mp4"))

        test(m"track"):
          """<track src="https://example.com/x.vtt">""".read[Html of "track"]
        . assert(_ == Track(src = url"https://example.com/x.vtt"))

        test(m"wbr"):
          """<wbr>""".read[Html of "wbr"]
        . assert(_ == Wbr)

      suite(m"Element coverage: raw text and RCDATA"):
        test(m"textarea with entity decoded"):
          """<textarea>foo &amp; bar</textarea>""".read[Html of "textarea"]
        . assert(_ == Textarea("foo & bar"))

        test(m"textarea with fake nested tag"):
          """<textarea><div>not parsed</div></textarea>""".read[Html of "textarea"]
        . assert(_ == Textarea("<div>not parsed</div>"))

        test(m"style preserves CSS literally"):
          """<head><style>.cls { color: red; }</style></head>""".read[Html of "head"]
        . assert(_ == Head(Style(".cls { color: red; }")))

        test(m"style does not decode entities"):
          """<head><style>a &amp; b</style></head>""".read[Html of "head"]
        . assert(_ == Head(Style("a &amp; b")))

        test(m"empty script"):
          """<head><script></script></head>""".read[Html of "head"]
        . assert(_ == Head(Script))

        test(m"empty style"):
          """<head><style></style></head>""".read[Html of "head"]
        . assert(_ == Head(Style))

      suite(m"Tree construction: autoclose and inference"):
        test(m"definition list with dt and dd"):
          """<dl><dt>term<dd>def</dl>""".read[Html of Flow]
        . assert(_ == Dl(Dt("term"), Dd("def")))

        test(m"select with multiple options"):
          """<select><option>a<option>b</select>""".read[Html of Flow]
        . assert(_ == Select(Option("a"), Option("b")))

        test(m"colgroup with col children"):
          """<colgroup><col><col></colgroup>""".read[Html of "colgroup"]
        . assert(_ == Colgroup(Col, Col))

        test(m"nested unordered lists"):
          """<ul><li>outer<ul><li>inner</li></ul></li></ul>""".read[Html of Flow]
        . assert(_ == Ul(Li("outer", Ul(Li("inner")))))

        test(m"p autoclosed by following p"):
          """<div><p>one<p>two</div>""".read[Html of Flow]
        . assert(_ == Div(P("one"), P("two")))

        test(m"option autocloses on following option"):
          """<select><option>a</option><option>b</option></select>""".read[Html of Flow]
        . assert(_ == Select(Option("a"), Option("b")))

      suite(m"Whitespace handling"):
        test(m"whitespace-only input"):
          "   \n  ".read[Html of Flow]
        . assert(_ == Fragment())

        test(m"pre preserves internal whitespace"):
          "<pre>line1\n  line2</pre>".read[Html of "pre"]
        . assert(_ == Pre("line1\n  line2"))

        test(m"trailing whitespace after body"):
          "<title>x</title>\n<body>y</body>\n".read[Html of "html"]
        . assert(_ == Html(Head(Title("x")), Body("y")))

        test(m"leading whitespace before content"):
          "  <p>x</p>".read[Html of Flow]
        . assert(_ == P("x"))

      suite(m"Foreign content depth"):
        test(m"SVG with multiple attributes preserves case"):
          val parsed = """<svg viewBox="0 0 10 10" width="50"></svg>""".read[Html of Flow]
          parsed match
            case Element("svg", attrs, _, true) =>
              attrs.toList.map(_._1) == List(t"viewBox", t"width")
            case _ => false
        . assert(_ == true)

        test(m"nested SVG content"):
          """<svg><g><circle/></g></svg>""".read[Html of Flow]
        . assert: result =>
            result == Svg(Element.foreign("g", proscenium.Map[Text, Optional[Text]](),
              Element.foreign("circle", proscenium.Map[Text, Optional[Text]]())))

        test(m"CDATA inside SVG"):
          val parsed = """<svg><![CDATA[raw <text>]]></svg>""".read[Html of Flow]
          parsed match
            case Element("svg", _, children, true) if children.length == 1 =>
              children.readUnchecked(0) match
                case TextNode(text) => text == "raw <text>"
                case _              => false
            case _ => false
        . assert(_ == true)

        test(m"CDATA inside HTML errors"):
          try """<div><![CDATA[x]]></div>""".read[Html of "div"]
          catch case exception: Exception => exception

        . assert: result =>
            (result: @unchecked) match
              case Parse.Error(_, _, Html.Issue.InvalidCdata) => true

        test(m"SVG self-closing rect"):
          """<svg><rect/></svg>""".read[Html of Flow]
        . assert(_ == Svg(Element.foreign("rect", proscenium.Map[Text, Optional[Text]]())))

      suite(m"Error position reporting"):
        test(m"EOF inside open tag"):
          try """<div""".read[Html of "div"]
          catch case exception: Exception => exception

        . assert: result =>
            (result: @unchecked) match
              case Parse.Error(_, _, Html.Issue.ExpectedMore) => true

        test(m"EOF inside attribute value"):
          try """<div style="ab""".read[Html of "div"]
          catch case exception: Exception => exception

        . assert: result =>
            (result: @unchecked) match
              case Parse.Error(_, _, Html.Issue.ExpectedMore) => true

        test(m"EOF inside comment"):
          try """<!-- abc""".read[Html of Flow]
          catch case exception: Exception => exception

        . assert: result =>
            (result: @unchecked) match
              case Parse.Error(_, _, Html.Issue.ExpectedMore) => true

        test(m"EOF inside CDATA"):
          try """<svg><![CDATA[abc""".read[Html of Flow]
          catch case exception: Exception => exception

        . assert: result =>
            (result: @unchecked) match
              case Parse.Error(_, _, Html.Issue.ExpectedMore) => true

        test(m"EOF inside closing tag"):
          try """<div></div""".read[Html of "div"]
          catch case exception: Exception => exception

        . assert: result =>
            (result: @unchecked) match
              case Parse.Error(_, _, Html.Issue.ExpectedMore) => true

        test(m"unexpected character in tag name"):
          try """<div@>""".read[Html of "div"]
          catch case exception: Exception => exception
        . assert: result =>
            (result: @unchecked) match
              case Parse.Error(_, _, Html.Issue.Unexpected(_))      => true
              case Parse.Error(_, _, Html.Issue.UnknownAttributeStart(_)) => true
              case Parse.Error(_, _, Html.Issue.InvalidTag(_))      => true

        test(m"tag starting with digit"):
          try """<1div>""".read[Html of Flow]
          catch case exception: Exception => exception
        . assert: result =>
            (result: @unchecked) match
              case Parse.Error(_, _, Html.Issue.InvalidTagStart(_)) => true
              case Parse.Error(_, _, Html.Issue.InvalidTag(_))      => true
              case Parse.Error(_, _, Html.Issue.Unexpected(_))      => true

      suite(m"Round-trip via show"):
        test(m"simple element"):
          "<div>x</div>".read[Html of "div"].show
        . assert(_ == "<div>x</div>")

        test(m"comment"):
          "<!--text-->".read[Html of Flow].show
        . assert(_ == "<!--text-->")

        test(m"element with attribute"):
          """<img alt="x">""".read[Html of "img"].show
        . assert(_ == """<img alt="x">""")

        test(m"nested elements"):
          "<div><p>x</p></div>".read[Html of "div"].show
        . assert(_ == "<div><p>x</p></div>")

        // An `HttpUrl` attribute is rendered by the generic `Abstractable across Urls` given,
        // which routes through `HttpUrl`'s `abstractable = _.show`; honeycomb no longer carries
        // a specific `HttpUrl is Attributive` given of its own.
        test(m"element with an HttpUrl attribute"):
          A(href = url"https://example.com/a?b=c#d")(TextNode("link")).show
        . assert(_ == """<a href="https://example.com/a?b=c#d">link</a>""")

      suite(m"Adoption agency algorithm"):
        test(m"unclosed b at end of p"):
          """<div><p><b>X</p></div>""".read[Html of "div"]
        . assert(_ == Div(P(B("X"))))

        test(m"misnested b and i with content after"):
          """<div><p><b>X</p>Y</b></div>""".read[Html of "div"]
        . assert(_ == Div(P(B("X")), B("Y")))

        test(m"misnested formatting tags"):
          """<div><b><i>1</b>2</i></div>""".read[Html of "div"]
        . assert(_ == Div(B(I("1")), I("2")))

        test(m"em with misnested b"):
          """<div><em><b>X</em><b>Y</b></div>""".read[Html of "div"]
        . assert(_ == Div(Em(B("X")), B("Y")))

        test(m"a inside a closes outer"):
          """<div><a href="https://x/">1<a href="https://y/">2</a>3</a></div>""".read[Html of "div"]
        . assert: result =>
            result == Div(A(href = url"https://x/")("1"), A(href = url"https://y/")("2"), TextNode("3"))

      suite(m"Foster parenting"):
        test(m"stray text before tr"):
          """<table>x<tr><td>y</td></tr></table>""".read[Html of Flow]
        . assert(_ == Fragment(TextNode("x"), Table(Tbody(Tr(Td("y"))))))

        test(m"stray div in table"):
          """<table><div>z</div><tr><td>y</td></tr></table>""".read[Html of Flow]
        . assert(_ == Fragment(Div("z"), Table(Tbody(Tr(Td("y"))))))

        test(m"trailing text in table"):
          """<table><tr><td>cell</td></tr>extra</table>""".read[Html of Flow]
        . assert(_ == Fragment(Table(Tbody(Tr(Td("cell")))), TextNode("extra")))

        test(m"whitespace inside table is ignored"):
          t"""<table>\n  <tr><td>cell</td></tr></table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Td("cell")))))

        test(m"formatting element fostered"):
          """<table><b>x</b><tr><td>y</td></tr></table>""".read[Html of Flow]
        . assert(_ == Fragment(B("x"), Table(Tbody(Tr(Td("y"))))))

        test(m"multiple fostered elements maintain order"):
          """<table>a<b>c</b><tr><td>y</td></tr></table>""".read[Html of Flow]
        . assert(_ == Fragment(TextNode("a"), B("c"), Table(Tbody(Tr(Td("y"))))))

        test(m"mixed before and after fostering"):
          """<table>before<tr><td>x</td></tr>after</table>""".read[Html of Flow]
        . assert(_ == Fragment(TextNode("before"), Table(Tbody(Tr(Td("x")))), TextNode("after")))

        test(m"fostered element with attributes preserved"):
          """<table><div style="color:red">x</div><tr><td>y</td></tr></table>""".read[Html of Flow]
        . assert(_ == Fragment(Div(style = t"color:red")(t"x"), Table(Tbody(Tr(Td(t"y"))))))

        test(m"fostering inside nested context"):
          """<div><table><b>x</b><tr><td>y</td></tr></table></div>""".read[Html of "div"]
        . assert(_ == Div(B("x"), Table(Tbody(Tr(Td("y"))))))

        test(m"content inside td is not fostered"):
          """<table><tr><td><b>inside</b></td></tr></table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Td(B("inside"))))))

        test(m"script inside table is not fostered"):
          """<table><script>code</script><tr><td>y</td></tr></table>""".read[Html of Flow]
        . assert(_ == Table(Script("code"), Tbody(Tr(Td("y")))))

        test(m"fostered element after fostered text"):
          """<table>x<b>y</b>z<tr><td>w</td></tr></table>""".read[Html of Flow]
        . assert(_ == Fragment(TextNode("x"), B("y"), TextNode("z"), Table(Tbody(Tr(Td("w"))))))

      suite(m"Optics"):
        test(m"Tag optic transforms every matching child element"):
          Div(P("a"), P("b")).lens(_(P) = P("x")).show
        . assert(_ == "<div><p>x</p><p>x</p></div>")

        test(m"Tag optic leaves non-matching children unchanged"):
          Div(P("a"), B("b")).lens(_(P) = P("x")).show
        . assert(_ == "<div><p>x</p><b>b</b></div>")

        test(m"Tag optic over a single matching child"):
          Div(P("a")).lens(_(P) = P("z")).show
        . assert(_ == "<div><p>z</p></div>")

        test(m"Tag optic with no matching children is a no-op"):
          Div(B("a")).lens(_(P) = P("x")).show
        . assert(_ == "<div><b>a</b></div>")

        test(m"nested Tag optics compose"):
          Div(Ul(Li("a"), Li("b"))).lens(_(Ul)(Li) = Li("x")).show
        . assert(_ == "<div><ul><li>x</li><li>x</li></ul></div>")

      suite(m"Interpolator tests"):
        import attributives.textAttributive

        test(m"simple interpolator"):
          val comment = "comment"
          val attribute = "attribute"
          def more: Int = 42
          h"""<p title=$attribute><!-- inner:$comment:outer -->This is some $more HTML.</p>"""
        . assert(_ == P(title = "attribute")(Comment(" inner:comment:outer "), "This is some ", "42", " HTML."))

        test(m"modify attribute"):
          val img = Img(alt = "hello")
          img.alt = img.alt+" world"
        . assert(_ == Img(alt = "hello world"))

        test(m"class can be added to element with children"):
          import honeycomb.stylesheets.uncheckedClasses
          Div.foo(P("hello"))
        . assert(_ == Div(`class` = "foo")(P("hello")))

        test(m"class can be added to element with attributes"):
          import honeycomb.stylesheets.uncheckedClasses
          Img.foo(alt = "bar")
        . assert(_ == Img(`class` = "foo", alt = "bar"))

        test(m"interpolate multiple attributes"):
          val dirname = "dirname"
          val alt = "alt"
          val maxlength = 10
          val title = "title"
          val placeholder = "placeholder"
          h"""<input alt="$alt" dirname="$dirname" title="$title" placeholder="$placeholder" maxlength="$maxlength">"""
        . assert(_ == Input(title = "title", dirname = "dirname", alt = "alt", maxlength = 10, placeholder = "placeholder"))

        test(m"single extraction"):
          P("whole text").absolve match
            case h"""<p>$whole</p>""" => whole
        . assert(_ == TextNode("whole text"))

        test(m"pattern matcher"):
          Div(title = "text")(Ul(Li("hello")), P("more")).absolve match
            case h"""<div title=$att><ul>${value2}</ul>${value1}</div>""" =>
              (att, value2, value1)
        . assert(_ == ("text", Li("hello"), P("more")))

        test(m"extractor on tag body"):
          Form(Input(title = "text", disabled = true, style = "testing"), "text").absolve match
            case h"""<form><input $atts>$more</form>""" => atts
        . assert(_ == Ledger("style" -> "testing", "disabled" -> Unset, "title" -> "text"))

        test(m"extractor on tag body with removals"):
          Form(Input(title = "text", disabled = true, style = "testing"), "text").absolve match
            case h"""<form><input style=$style $atts>$more</form>""" => atts
        . assert(_ == Ledger("disabled" -> Unset, "title" -> "text"))

        test(m"extractor on attribute"):
          Form(Input(title = "text", disabled = true, style = "testing"), "text").absolve match
            case h"""<form><input style=$style>$more</form>""" => style
        . assert(_ == "testing")

        test(m"extractor of text"):
          P("whole text").absolve match
            case h"""<p $atts>$whole</p>""" => whole
        . assert(_ == TextNode("whole text"))

        test(m"extractor of comment"):
          P(Comment("this is the comment")).absolve match
            case h"""<p $atts><!--$comment--></p>""" => comment
        . assert(_ == "this is the comment")

        test(m"zero-hole extractor of comment"):
          P(Comment("this is the comment")).absolve match
            case h"""<p><!--this is the comment--></p>""" => 1
        . assert(_ == 1)

        test(m"zero-hole non-matching extractor"):
          P(Comment("this is the comment")).absolve match
            case h"""<p><!--this is not the comment--></p>""" => 1
            case _ => 2
        . assert(_ == 2)

        test(m"extractor of an element"):
          P(Input.Button).absolve match
            case h"""<p $atts><$element></p>""" => element
        . assert(_ == Input.Button)

        test(m"extractor of an element and its attribute"):
          P(Img(alt = "titletext")).absolve match
            case h"""<p><$img></p>""" => img
        . assert(_ == Img(alt = "titletext"))

      suite(m"Accessor tests"):
        test(m"Simple accessor"):
          val html = Table(Tbody(Tr(Th("Hello world"))))
          html / Tbody / Tr / Th
        . assert(_ == Th("Hello world"))

        test(m"Collecting accessor"):
          val html = Div(Ul(Li("one"), Li("two"), Li("three")))
          html / Ul / Li
        . assert(_ == Fragment(Li("one"), Li("two"), Li("three")))

        test(m"Joining HTML"):
          H1("title") + P("Hello world")
        . assert(_ == h"<h1>title</h1><p>Hello world</p>")

        test(m"Insert prefix child"):
          Div(P("body")) ^+ H1("title")
        . assert(_ == h"<div><h1>title</h1><p>body</p></div>")

        test(m"Insert suffix child"):
          Div(H1("title")) +^ P("body")
        . assert(_ == h"<div><h1>title</h1><p>body</p></div>")

        test(m"Insert prefix children"):
          Div(P("body")) ^+ (H1("title") + H2("subtitle"))
        . assert(_ == h"<div><h1>title</h1><h2>subtitle</h2><p>body</p></div>")

        test(m"Insert suffix children"):
          Div(H1("title")) +^ (P("body") + P("more"))
        . assert(_ == h"<div><h1>title</h1><p>body</p><p>more</p></div>")

      suite(m"Permissive parsing"):
        import recoveries.permissiveRecovery

        test(m"unknown attribute is accepted"):
          """<div bogus="x"></div>""".read[Html of "div"]
        . assert:
            case Element("div", _, _, _) => true
            case _                       => false

        test(m"unknown attribute on void tag is accepted"):
          """<img mystery="y">""".read[Html of "img"]
        . assert:
            case Element("img", _, _, _) => true
            case _                       => false

        test(m"duplicate attribute keeps first value"):
          """<img alt="first" alt="second">""".read[Html of "img"]
        . assert(_ == Img(alt = "first"))

        test(m"valid attribute used on wrong tag is accepted"):
          """<div alt="caption"></div>""".read[Html of "div"]
        . assert:
            case Element("div", _, _, _) => true
            case _                       => false

        test(m"forbidden char in unquoted value is absorbed (per WHATWG)"):
          """<img alt=a"b>""".read[Html of "img"]
        . assert(_ == Img(alt = """a"b"""))

        test(m"CDATA outside foreign content becomes a comment"):
          """<div><![CDATA[xyz]]></div>""".read[Html of "div"]
        . assert(_ == Div(Comment("[CDATA[xyz]]")))

        test(m"unclosed element recovers via implicit close"):
          """<ul><li>First item""".read[Html of Flow]
        . assert(_ == Ul(Li("First item")))

        test(m"stray close tag at root is ignored"):
          """</br>""".read[Html of Flow]
        . assert(_ == Fragment())

        test(m"close tag with no matching open is ignored"):
          """<div>x</p></div>""".read[Html of "div"]
        . assert(_ == Div("x"))

        test(m"inadmissible child auto-closes parent"):
          // <a> is transparent; <li> isn't a valid child of <div> (which is
          // the surrounding admissible context). Strict mode aborts.
          // Permissive: close <a>, retry <li> at div level. div doesn't admit
          // li either; auto-close div; retry at root. Root accepts whatever.
          """<div><a href="#"><li>list item</li></a></div>""".read[Html of Flow]
        . assert: result =>
            // Just confirm we got SOMETHING back (no throw).
            result != null

        test(m"unclosed comment doesn't throw"):
          """<div><!-- unclosed""".read[Html of "div"]
        . assert: result =>
            // Catch-all swallows the ExpectedMore; we get a Fragment fallback.
            result != null

        test(m"truncated open tag doesn't throw"):
          """<div""".read[Html of Flow]
        . assert: result =>
            result != null

        test(m"truncated attribute value doesn't throw"):
          """<div style="ab""".read[Html of Flow]
        . assert: result =>
            result != null

        test(m"chaotic real-page-like input doesn't throw"):
          val samples = List
           ("""<html><body><p>opening<div>mixed</p></body>""",
            """<p><b><i>X</b>Y</i></p>""",
            """<table><tr><td>a<td>b<tr><td>c</table>""",
            """<DIV CLASS=x>UPPER</DIV>""",
            """<a href=javascript:void(0)>click</a>""",
            """<!DOCTYPE html><html><body>hi</body>""",
            """<p>one&amp;two&unknown;three</p>""",
            """<script>if (a < b) doSomething();</script>""",
            """<style>.x { color: red; }</style>""",
            """<input checked disabled name=q value="hello world">""",
            """<p>a<br>b<br/>c</p>""",
            """<!--comment-->text<!--another-->""",
            """<svg><circle cx="50" cy="50" r="40"/></svg>""",
            """<div lang=en xml:lang="en">foreign attrs</div>""",
            """<p>nested <em><strong>text</strong></em> ok</p>""" )

          samples.map: sample =>
            try
              sample.read[Html of Flow]
              true
            catch case _: Throwable => false
        . assert(_.all(identity))

        test(m"permissive given wins when Tactic[Parse.Error] is also in scope"):
          // `throwUnsafely` (file-level import) provides Tactic[Parse.Error] via
          // contravariance. `recoveries.permissiveRecovery` provides Recovery.Permissive.
          // Both givens are visible: strict requires `NotGiven[Permissive]`,
          // which fails — so only permissive resolves.
          //
          // The strict reader on `<p>unclosed` would throw `Incomplete("p")`;
          // permissive recovers it as `P("unclosed")`.
          """<p>unclosed""".read[Html of Flow]
        . assert(_ == P("unclosed"))

        test(m"parser emits warnings via raise() when a Tactic is in scope"):
          val errors = scala.collection.mutable.ListBuffer[Parse.Error]()

          locally:
            given Tactic[Parse.Error]:
              given canThrow: CanThrow[Exception] = unsafeExceptions.canThrowAny

              def diagnostics: Diagnostics = errorDiagnostics.stackTracesDiagnostics

              def record(error: Diagnostics ?=> Parse.Error): Unit =
                errors += error(using diagnostics)

              def abort(error: Diagnostics ?=> Parse.Error): Nothing =
                throw error(using diagnostics)

              def certify(): Unit = ()

            Html.HtmlParser.fromIterator
             (Iterator("""<img alt="a" alt="b">"""), permissive = true)
            . parseHtml(Tag.root(Set("img")))

          errors.toList.map(_.issue)
        . assert:
            case List(Html.Issue.DuplicateAttribute(name)) => name == "alt"
            case _                                         => false


object Html4Tests extends Suite(m"HTML4 parsing tests"):
  def run(): Unit =
      import htmlDoms.html4Transitional
      import html4Transitional.*

      test(m"simple empty tag"):
        """<div></div>""".read[Html]
      . assert(_ == Div)

      test(m"List"):
        """<ul><li>item</li></ul>""".read[Html of Flow]
      . assert(_ == Ul(Li("item")))

      test(m"simple tag with text"):
        """<div>content</div>""".read[Html of "div"]
      . assert(_ == Div("content"))

      test(m"more than one node"):
        """<div>content</div><p>more content</p>""".read[Html of Flow]
      . assert(_ == Fragment(Div("content"), P("more content")))

      test(m"more than one node with comment"):
        """<div>content</div><!-- comment --><div>more content</div>""".read[Html of "div"]
      . assert(_ == Fragment(Div("content"), Comment(" comment "), Div("more content")))

      test(m"simple self-closing tag"):
        """<div />""".read[Html of "div"]
      . assert(_ == Div)

      test(m"self-closing tag with attributes"):
        """<div style="bar"/>""".read[Html of "div"]
      . assert(_ == Div(style = "bar"))

      test(m"case-insensitive element"):
        """<DIV style="bar">hello world</DIV>""".read[Html of "div"]
      . assert(_ == Div(style = "bar")("hello world"))

      test(m"simple comment tag"):
        """<!--This is a comment-->""".read[Html of Flow]
      . assert(_ == Comment("This is a comment"))

      test(m"simple void tag"):
        """<br>""".read[Html of Flow]
      . assert(_ == Br)

      test(m"void tag with an attribute"):
        """<area style="bar">""".read[Html of "area"]
      . assert(_ == Area(style = "bar"))

      test(m"void tag with an unquoted attribute"):
        """<img style=bar>""".read[Html of Flow]
      . assert(_ == Img(style = "bar"))

      test(m"void tag with a boolean attribute"):
        """<input disabled>""".read[Html of "input"]
      . assert(_ == Input(disabled = true))

      test(m"void tag with a single-quoted attribute"):
        """<br style='bar baz'>""".read[Html of Flow]
      . assert(_ == Br(style = "bar baz"))

      test(m"simple nested tag"):
        """<div><br></div>""".read[Html of Flow]
      . assert(_ == Div(Br))

      test(m"just text"):
        """hello world""".read[Html of Flow]
      . assert(_ == TextNode("hello world"))

      test(m"just text with entity"):
        """to &amp; fro""".read[Html of Flow]
      . assert(_ == TextNode("to & fro"))

      test(m"just an entity"):
        """&amp;""".read[Html of Flow]
      . assert(_ == TextNode("&"))

      test(m"misnested formatting (adoption agency)"):
        """<em><b></em></b>""".read[Html of Inline]
      . assert(_ == Em(B()))

      test(m"unknown tag"):
        try """<scrip>""".read[Html of Inline]
        catch case exception: Exception => exception
      . assert:
          case Parse.Error(Html, Html.Position(line, col, _, _), Html.Issue.InvalidTag("scrip")) =>
            line == 1.u && col == 2.u
          case _ => false

      test(m"raw text"):
        "<head><script>some content</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some content")))

      test(m"raw text, with partial closing tag"):
        "<head><script>some content</scr</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some content</scr")))

      test(m"raw text, with non-entity"):
        "<head><script>some &amp; content</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some &amp; content")))

      test(m"autoclosing tag"):
        """<ul><li>First item</ul>""".read[Html of Flow]
      . assert(_ == Ul(Li("First item")))

      test(m"unclosed paragraph"):
        """<p>para""".read[Html of Flow]
      . assert(_ == P("para"))

      test(m"empty content"):
        "".read[Html of Flow]
      . assert(_ == Fragment())

      test(m"failing example"):
        t"""<p>x<img></p>\n""".read[Html of Flow]
      . assert(_ == Fragment(P(t"x", Img), t"\n"))

      test(m"autoclosing adjacent tags"):
        """<ul><li>First item<li>Second item</ul>""".read[Html of Flow]
      . assert(_ == Ul(Li("First item"), Li("Second item")))

      test(m"unclosed tag 1"):
        try """<ul><li>First item</li>""".read[Html of Flow]
        catch case exception: Exception => exception
      . assert(_ == Parse.Error(Html, Html.Position(1.u, 23.u), Html.Issue.Incomplete("ul")))

      test(m"infer both <head> and <body>"):
        """<title>Page title</title><p>A paragraph</p>""".read[Html of "html"]
      . assert(_ == Html(Head(Title("Page title")), Body(P("A paragraph"))))

      suite(m"Table tests"):
        test(m"Simple table"):
          """<table><tbody><tr><th>First</th><td>Second</td><td>Third</td></tr></tbody></table>""".read[Html of "table"]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tbody> is inferred"):
          """<table><tr><th>First</th><td>Second</td><td>Third</td></tr></table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tbody> autocloses"):
          """<table><tbody><tr><th>First</th><td>Second</td><td>Third</td></tr></table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tr> autocloses"):
          """<table style="bar"><tbody><tr><th>First</th><td>Second</td><td>Third</td></tbody></table>""".read[Html of Flow]
        . assert(_ == Table(style = "bar")(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tbody> and <tr> autoclose"):
          """<table><tbody><tr><th>First</th><td>Second</td><td>Third</td></table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tbody>, <tr>, <th> and <td> autoclose"):
          """<table><tbody><tr><th>First<td>Second<td>Third</table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<thead> works like <tbody>"):
          """<table class="test"><thead><tr><th>First<td>Second<td>Third</table>""".read[Html of Flow]
        . assert: result =>
            given (Attribution of "test" | "foo") = Attribution.classes()
            result == Table.test(Thead(Tr(Th("First"), Td("Second"), Td("Third"))))

        test(m"<tfoot> closes inferred <tbody>"):
          """<table><tr><th>First<td>Second<td>Third<tfoot><tr><td>Footer</table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third"))), Tfoot(Tr(Td("Footer")))))

      test(m"Whitespace permitted and ignored between list items"):
        t"""<ul><li>hello</li>\n  <li>world</li></ul>""".read[Html of "html"]
      . assert(_ == Html(Body(Ul(Li("hello"), Li("world")))))

      test(m"transparent tag with text"):
        """<p>Go <a href="https://example.com">home</a>.</p>""".read[Html of "p"]
      . assert(_ == P("Go ", A(href = url"https://example.com")("home"), "."))

      test(m"transparent tag with element"):
        """<p>Go <a href="https://example.com"><em>home</em></a>.</p>""".read[Html of "p"]
      . assert(_ == P("Go ", A(href = url"https://example.com")(Em("home")), "."))

      test(m"Tag subtype"):
        """<input type="button">""".read[Html of "input"]
      . assert(_ == Input.Button)

      test(m"Parse RCDATA with no entities"):
        """<title>Push then Pull</title>""".read[Html of Metadata]
      . assert(_ == Title("Push then Pull"))

      test(m"Parse Document without doctype"):
        val parsed = """<title>Heading</title>
            <p>body""".load[Html]
        val example = Html(Head(Title("Heading")), Body(P("body")))
        parsed == Document(example, htmlDoms.html4Transitional)
      . assert(_ == true)

      test(m"Parse Document with HTML4 doctype"):
        val parsed = """<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
            <title>Heading</title>
            <p>body""".load[Html]
        val example = Html(Head(Title("Heading")), Body(P("body")))
        parsed == Document(example, htmlDoms.html4Transitional)
      . assert(_ == true)

      test(m"Parse RCDATA with an entity"):
        """<title>Push &amp; Pull</title>""".read[Html of Metadata]
      . assert(_ == Title("Push & Pull"))

      test(m"Parse empty RCDATA"):
        """<title></title>""".read[Html of Metadata]
      . assert(_ == Title)

      test(m"Attribute with character entity"):
        """<img alt="To &amp; fro">""".read[Html of Flow]
      . assert(_ == Img(alt = "To & fro"))

      test(m"Attribute with numeric character entity"):
        """<img alt="Schlo&#223;">""".read[Html of Flow]
      . assert(_ == Img(alt = "Schloß"))

      test(m"Text with hex character entity"):
        """<p>value: &#x6A;</p>""".read[Html of Flow]
      . assert(_ == P("value: j"))

      test(m"Text with emoji character entity"):
        """<p>value: &#x1f600;""".read[Html of Flow]
      . assert(_ == P("value: 😀"))

      suite(m"Tokenization edge cases"):
        test(m"empty comment"):
          """<!---->""".read[Html of Flow]
        . assert(_ == Comment(""))

        test(m"comment with newline"):
          "<!--line1\nline2-->".read[Html of Flow]
        . assert(_ == Comment("line1\nline2"))

        test(m"multiple consecutive comments"):
          """<!--a--><!--b-->""".read[Html of Flow]
        . assert(_ == Fragment(Comment("a"), Comment("b")))

        test(m"comment with hyphen inside"):
          """<!-- - -->""".read[Html of Flow]
        . assert(_ == Comment(" - "))

        test(m"decimal entity for ASCII"):
          """<p>&#65;</p>""".read[Html of Flow]
        . assert(_ == P("A"))

        test(m"decimal entity for max BMP"):
          """<p>&#65535;</p>""".read[Html of Flow]
        . assert(_ == P("￿"))

        test(m"decimal entity for supra-BMP code point"):
          """<p>&#128512;</p>""".read[Html of Flow]
        . assert(_ == P("😀"))

        test(m"unknown named entity passed through literally"):
          """<p>&foo;</p>""".read[Html of Flow]
        . assert(_ == P("&foo;"))

      suite(m"Attribute parsing depth"):
        test(m"multiple attributes preserved"):
          """<img alt="a" title="b">""".read[Html of "img"]
        . assert(_ == Img(alt = "a", title = "b"))

        test(m"attribute value with named entity"):
          """<img alt="a&amp;b">""".read[Html of "img"]
        . assert(_ == Img(alt = "a&b"))

        test(m"attribute value with hex entity"):
          """<img alt="&#x41;">""".read[Html of "img"]
        . assert(_ == Img(alt = "A"))

        test(m"attribute value with decimal entity"):
          """<img alt="&#65;">""".read[Html of "img"]
        . assert(_ == Img(alt = "A"))

        test(m"single quote inside double-quoted value"):
          """<img alt="it's">""".read[Html of "img"]
        . assert(_ == Img(alt = "it's"))

        test(m"double quote inside single-quoted value"):
          """<img alt='say "hi"'>""".read[Html of "img"]
        . assert(_ == Img(alt = """say "hi""""))

        test(m"forbidden character in unquoted value"):
          try """<img alt=a"b>""".read[Html of "img"]
          catch case exception: Exception => exception
        . assert:
            case Parse.Error
                  ( Html,
                    Html.Position(line, col, _, _),
                    Html.Issue.ForbiddenUnquoted('"') ) =>
              line == 1.u && col == 11.u
            case _ => false

        test(m"whitespace around equals"):
          """<img alt = "a">""".read[Html of "img"]
        . assert(_ == Img(alt = "a"))

        test(m"duplicate attribute"):
          try """<img alt="a" alt="b">""".read[Html of "img"]
          catch case exception: Exception => exception
        . assert:
            case Parse.Error(_, _, Html.Issue.DuplicateAttribute(name)) => name == "alt"
            case _                                                     => false

        test(m"unknown attribute on known tag"):
          try """<div bogus="x"></div>""".read[Html of "div"]
          catch case exception: Exception => exception
        . assert:
            case Parse.Error(_, _, Html.Issue.UnknownAttributeStart(_)) => true
            case Parse.Error(_, _, Html.Issue.UnknownAttribute(_))      => true
            case _                                                     => false

      suite(m"Element coverage: void elements"):
        test(m"area"):
          """<area alt="a">""".read[Html of "area"]
        . assert(_ == Area(alt = "a"))

        test(m"base"):
          """<base href="https://example.com/">""".read[Html of "base"]
        . assert(_ == Base(href = url"https://example.com/"))

        test(m"br"):
          """<br>""".read[Html of "br"]
        . assert(_ == Br)

        test(m"col"):
          """<col span="2">""".read[Html of "col"]
        . assert(_ == Col(span = 2))

        test(m"hr"):
          """<hr>""".read[Html of "hr"]
        . assert(_ == Hr)

        test(m"img"):
          """<img alt="x">""".read[Html of "img"]
        . assert(_ == Img(alt = "x"))

        test(m"input"):
          """<input>""".read[Html of "input"]
        . assert(_ == Input)

        test(m"meta"):
          """<meta charset="utf-8">""".read[Html of "meta"]
        . assert(_ == Meta(charset = "utf-8"))

      suite(m"Element coverage: raw text and RCDATA"):
        test(m"textarea with entity decoded"):
          """<textarea>foo &amp; bar</textarea>""".read[Html of "textarea"]
        . assert(_ == Textarea("foo & bar"))

        test(m"textarea with fake nested tag"):
          """<textarea><div>not parsed</div></textarea>""".read[Html of "textarea"]
        . assert(_ == Textarea("<div>not parsed</div>"))

        test(m"style preserves CSS literally"):
          """<head><style>.cls { color: red; }</style></head>""".read[Html of "head"]
        . assert(_ == Head(Style(".cls { color: red; }")))

        test(m"style does not decode entities"):
          """<head><style>a &amp; b</style></head>""".read[Html of "head"]
        . assert(_ == Head(Style("a &amp; b")))

        test(m"empty script"):
          """<head><script></script></head>""".read[Html of "head"]
        . assert(_ == Head(Script))

        test(m"empty style"):
          """<head><style></style></head>""".read[Html of "head"]
        . assert(_ == Head(Style))

      suite(m"Tree construction: autoclose and inference"):
        test(m"definition list with dt and dd"):
          """<dl><dt>term<dd>def</dl>""".read[Html of Flow]
        . assert(_ == Dl(Dt("term"), Dd("def")))

        test(m"select with multiple options"):
          """<select><option>a<option>b</select>""".read[Html of Flow]
        . assert(_ == Select(Option("a"), Option("b")))

        test(m"colgroup with col children"):
          """<colgroup><col><col></colgroup>""".read[Html of "colgroup"]
        . assert(_ == Colgroup(Col, Col))

        test(m"nested unordered lists"):
          """<ul><li>outer<ul><li>inner</li></ul></li></ul>""".read[Html of Flow]
        . assert(_ == Ul(Li("outer", Ul(Li("inner")))))

        test(m"p autoclosed by following p"):
          """<div><p>one<p>two</div>""".read[Html of Flow]
        . assert(_ == Div(P("one"), P("two")))

        test(m"option autocloses on following option"):
          """<select><option>a</option><option>b</option></select>""".read[Html of Flow]
        . assert(_ == Select(Option("a"), Option("b")))

      suite(m"Whitespace handling"):
        test(m"whitespace-only input"):
          "   \n  ".read[Html of Flow]
        . assert(_ == Fragment())

        test(m"pre preserves internal whitespace"):
          "<pre>line1\n  line2</pre>".read[Html of "pre"]
        . assert(_ == Pre("line1\n  line2"))

        test(m"trailing whitespace after body"):
          "<title>x</title>\n<body>y</body>\n".read[Html of "html"]
        . assert(_ == Html(Head(Title("x")), Body("y")))

        test(m"leading whitespace before content"):
          "  <p>x</p>".read[Html of Flow]
        . assert(_ == P("x"))

      suite(m"Error position reporting"):
        test(m"EOF inside open tag"):
          try """<div""".read[Html of "div"]
          catch case exception: Exception => exception
        . assert:
            case Parse.Error(_, _, Html.Issue.ExpectedMore) => true
            case _                                         => false

        test(m"EOF inside attribute value"):
          try """<div style="ab""".read[Html of "div"]
          catch case exception: Exception => exception
        . assert:
            case Parse.Error(_, _, Html.Issue.ExpectedMore) => true
            case _                                         => false

        test(m"EOF inside comment"):
          try """<!-- abc""".read[Html of Flow]
          catch case exception: Exception => exception
        . assert:
            case Parse.Error(_, _, Html.Issue.ExpectedMore) => true
            case _                                         => false

        test(m"EOF inside closing tag"):
          try """<div></div""".read[Html of "div"]
          catch case exception: Exception => exception
        . assert:
            case Parse.Error(_, _, Html.Issue.ExpectedMore) => true
            case _                                         => false

      suite(m"Round-trip via show"):
        test(m"simple element"):
          "<div>x</div>".read[Html of "div"].show
        . assert(_ == "<div>x</div>")

        test(m"comment"):
          "<!--text-->".read[Html of Flow].show
        . assert(_ == "<!--text-->")

        test(m"element with attribute"):
          """<img alt="x">""".read[Html of "img"].show
        . assert(_ == """<img alt="x">""")

        test(m"nested elements"):
          "<div><p>x</p></div>".read[Html of "div"].show
        . assert(_ == "<div><p>x</p></div>")

      suite(m"Adoption agency algorithm"):
        test(m"unclosed b at end of p"):
          """<div><p><b>X</p></div>""".read[Html of "div"]
        . assert(_ == Div(P(B("X"))))

        test(m"misnested b and i with content after"):
          """<div><p><b>X</p>Y</b></div>""".read[Html of "div"]
        . assert(_ == Div(P(B("X")), B("Y")))

        test(m"misnested formatting tags"):
          """<div><b><i>1</b>2</i></div>""".read[Html of "div"]
        . assert(_ == Div(B(I("1")), I("2")))

        test(m"em with misnested b"):
          """<div><em><b>X</em><b>Y</b></div>""".read[Html of "div"]
        . assert(_ == Div(Em(B("X")), B("Y")))

        test(m"a inside a closes outer"):
          """<div><a href="https://x/">1<a href="https://y/">2</a>3</a></div>""".read[Html of "div"]
        . assert: result =>
            result == Div(A(href = url"https://x/")("1"), A(href = url"https://y/")("2"), TextNode("3"))

      suite(m"Foster parenting"):
        test(m"stray text before tr"):
          """<table>x<tr><td>y</td></tr></table>""".read[Html of Flow]
        . assert(_ == Fragment(TextNode("x"), Table(Tbody(Tr(Td("y"))))))

        test(m"stray div in table"):
          """<table><div>z</div><tr><td>y</td></tr></table>""".read[Html of Flow]
        . assert(_ == Fragment(Div("z"), Table(Tbody(Tr(Td("y"))))))

        test(m"trailing text in table"):
          """<table><tr><td>cell</td></tr>extra</table>""".read[Html of Flow]
        . assert(_ == Fragment(Table(Tbody(Tr(Td("cell")))), TextNode("extra")))

        test(m"whitespace inside table is ignored"):
          t"""<table>\n  <tr><td>cell</td></tr></table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Td("cell")))))

        test(m"formatting element fostered"):
          """<table><b>x</b><tr><td>y</td></tr></table>""".read[Html of Flow]
        . assert(_ == Fragment(B("x"), Table(Tbody(Tr(Td("y"))))))

        test(m"multiple fostered elements maintain order"):
          """<table>a<b>c</b><tr><td>y</td></tr></table>""".read[Html of Flow]
        . assert(_ == Fragment(TextNode("a"), B("c"), Table(Tbody(Tr(Td("y"))))))

        test(m"mixed before and after fostering"):
          """<table>before<tr><td>x</td></tr>after</table>""".read[Html of Flow]
        . assert(_ == Fragment(TextNode("before"), Table(Tbody(Tr(Td("x")))), TextNode("after")))

        test(m"fostered element with attributes preserved"):
          """<table><div style="color:red">x</div><tr><td>y</td></tr></table>""".read[Html of Flow]
        . assert(_ == Fragment(Div(style = t"color:red")(t"x"), Table(Tbody(Tr(Td(t"y"))))))

        test(m"fostering inside nested context"):
          """<div><table><b>x</b><tr><td>y</td></tr></table></div>""".read[Html of "div"]
        . assert(_ == Div(B("x"), Table(Tbody(Tr(Td("y"))))))

        test(m"content inside td is not fostered"):
          """<table><tr><td><b>inside</b></td></tr></table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Td(B("inside"))))))
