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
┃    Soundness, version 0.46.0.                                                                    ┃
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

import scala.collection.immutable as sci

import soundness.{Table as _, *}

import autopsies.contrastExpectations
import errorDiagnostics.stackTraces
import strategies.throwUnsafely

object Tests extends Suite(m"Honeycomb Tests"):
  def run(): Unit =
    // suite(m"Showing HTML"):
    //   import html5.*

    //   test(m"empty normal tag"):
    //     Div.show
    //   .check(_ == t"<div/>")

    //   test(m"empty unclosed tag"):
    //     Br.show
    //   .check(_ == t"<br>")

    //   test(m"tag with one attribute"):
    //     P(id = id"abc").show
    //   .check(_ == t"""<p id="abc"/>""")

    //   test(m"tag with two attributes"):
    //     P(id = id"abc", style = t"def").show
    //   .check(_ == t"""<p id="abc" style="def"/>""")

    //   test(m"unclosed tag with one attribute"):
    //     Hr(id = id"foo").show
    //   .check(_ == t"""<hr id="foo">""")

    //   test(m"unclosed tag with two attributes"):
    //     Hr(id = id"foo", style = t"bar").show
    //   .check(_ == t"""<hr id="foo" style="bar">""")

    //   test(m"non-self-closing tag"):
    //     Script.show
    //   .check(_ == t"""<script></script>""")

    //   test(m"tag with no attributes and children"):
    //     Div(Hr, Br).show
    //   .check(_ == t"""<div><hr><br></div>""")

    //   test(m"tag with text child"):
    //     P(t"hello world").show
    //   .check(_ == t"<p>hello world</p>")

    //   test(m"tag with mixed children"):
    //     P(t"hello ", Em(t"world"), t"!").show
    //   .check(_ == t"<p>hello <em>world</em>!</p>")

    //   test(m"deeper-nested elements"):
    //     Table(Tbody(Tr(Td(t"A")))).show
    //   .check(_ == t"<table><tbody><tr><td>A</td></tr></tbody></table>")

    suite(m"HTML parsing tests"):
      import doms.whatwg
      import whatwg.*

      test(m"simple empty tag"):
        t"""<div></div>""".read[Html]
      .assert(_ == Div)

      test(m"List"):
        t"""<ul><li>item</li></ul>""".read[Html of Flow]
      .assert(_ == Ul(Li("item")))

      test(m"simple tag with text"):
        t"""<div>content</div>""".read[Html of "div"]
      .assert(_ == Div("content"))

      test(m"more than one node"):
        t"""<div>content</div><p>more content</p>""".read[Html of Flow]
      .assert(_ == Html.Fragment(Div("content"), P("more content")))

      test(m"more than one node with comment"):
        t"""<div>content</div><!-- comment --><div>more content</div>""".read[Html of "div"]
      .assert(_ == Html.Fragment(Div("content"), Html.Comment(" comment "), Div("more content")))

      test(m"simple self-closing tag"):
        t"""<div />""".read[Html of "div"]
      .assert(_ == Div)

      test(m"self-closing tag with attributes"):
        t"""<div style="bar"/>""".read[Html of "div"]
      .assert(_ == Div(style = t"bar"))

      test(m"case-insensitive element"):
        t"""<DIV style="bar">hello world</DIV>""".read[Html of "div"]
      .assert(_ == Div(style = t"bar")("hello world"))

      test(m"simple comment tag"):
        t"""<!--This is a comment-->""".read[Html of Flow]
      .assert(_ == Html.Comment("This is a comment"))

      test(m"simple void tag"):
        t"""<br>""".read[Html of Flow]
      .assert(_ == Br)

      test(m"void tag with an attribute"):
        t"""<area style="bar">""".read[Html of "area"]
      .assert(_ == Area(style = t"bar"))

      test(m"void tag with an unquoted attribute"):
        t"""<area style=bar>""".read[Html of Flow]
      .assert(_ == Area(style = t"bar"))

      test(m"void tag with a boolean attribute"):
        t"""<input disabled>""".read[Html of "input"]
      .assert(_ == Input(disabled = true))

      test(m"void tag with a single-quoted attribute"):
        t"""<br style='bar baz'>""".read[Html of Flow]
      .assert(_ == Br(style = t"bar baz"))

      test(m"simple nested tag"):
        t"""<div><area></div>""".read[Html of Flow]
      .assert(_ == Div(Area))

      test(m"just text"):
        t"""hello world""".read[Html of Flow]
      .assert(_ == Html.Textual("hello world"))

      test(m"just text with entity"):
        t"""to &amp; fro""".read[Html of Flow]
      .assert(_ == Html.Textual("to & fro"))

      test(m"just an entity"):
        t"""&amp;""".read[Html of Flow]
      .assert(_ == Html.Textual("&"))

      test(m"mismatched closing tag"):
        try t"""<em><b></em></b>""".read[Html of Phrasing]
        catch case exception: Exception => exception
      .assert(_ == ParseError(Html, Html.Position(8.u), Html.Issue.MismatchedTag("b", "em")))

      test(m"unknown tag"):
        try t"""<scrip>""".read[Html of Phrasing]
        catch case exception: Exception => exception
      .assert(_ == ParseError(Html, Html.Position(2.u), Html.Issue.InvalidTag("scrip")))

      test(m"raw text"):
        t"<head><script>some content</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some content")))

      test(m"raw text, with partial closing tag"):
        t"<head><script>some content</scr</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some content</scr")))

      test(m"raw text, with shorter partial closing tag"):
        t"<head><script>some content</</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some content</")))

      test(m"raw text, with even shorter partial closing tag"):
        t"<head><script>some content<</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some content<")))

      test(m"raw text, with non-entity"):
        t"<head><script>some &amp; content</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some &amp; content")))

      test(m"raw text, with tag literal"):
        t"<head><script>some <foo> content</script></head>".read[Html of "head"]
      . assert(_ == Head(Script("some <foo> content")))

      test(m"autoclosing tag"):
        t"""<ul><li>First item</ul>""".read[Html of Flow]
      .assert(_ == Ul(Li("First item")))

      test(m"unclosed paragraph"):
        t"""<p>para""".read[Html of Flow]
      .assert(_ == P("para"))

      test(m"autoclosing adjacent tags"):
        t"""<ul><li>First item<li>Second item</ul>""".read[Html of Flow]
      .assert(_ == Ul(Li("First item"), Li("Second item")))

      test(m"unclosed tag 1"):
        try t"""<ul><li>First item</li>""".read[Html of Flow]
        catch case exception: Exception => exception
      .assert(_ == ParseError(Html, Html.Position(24.u), Html.Issue.Incomplete("ul")))

      test(m"unclosed tag 2"):
        try t"""<ul><li>First item""".read[Html of Flow]
        catch case exception: Exception => exception
      .assert(_ == ParseError(Html, Html.Position(19.u), Html.Issue.Incomplete("ul")))

      test(m"infer both <head> and <body>"):
        t"""<title>Page title</title><p>A paragraph</p>""".read[Html of "html"]
      .assert(_ == Html(Head(Title("Page title")), Body(P("A paragraph"))))


      suite(m"Table tests"):
        test(m"Simple table"):
          t"""<table><tbody><tr><th>First</th><td>Second</td><td>Third</td></tr></tbody></table>""".read[Html of "table"]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tbody> is inferred"):
          t"""<table><tr><th>First</th><td>Second</td><td>Third</td></tr></table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tbody> autocloses"):
          t"""<table><tbody><tr><th>First</th><td>Second</td><td>Third</td></tr></table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tr> autocloses"):
          t"""<table style="bar"><tbody><tr><th>First</th><td>Second</td><td>Third</td></tbody></table>""".read[Html of Flow]
        . assert(_ == Table(style = t"bar")(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tbody> and <tr> autoclose"):
          t"""<table><tbody><tr><th>First</th><td>Second</td><td>Third</td></table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tbody>, <tr>, <th> and <td> autoclose"):
          t"""<table><tbody><tr><th>First<td>Second<td>Third</table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third")))))

        given (CssClass of "test") = CssClass["test"]()

        test(m"<thead> works like <tbody>"):
          t"""<table class="test"><thead><tr><th>First<td>Second<td>Third</table>""".read[Html of Flow]
        . assert(_ == Table.test(Thead(Tr(Th("First"), Td("Second"), Td("Third")))))

        test(m"<tfoot> closes inferred <tbody>"):
          t"""<table><tr><th>First<td>Second<td>Third<tfoot><tr><td>Footer</table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third"))), Tfoot(Tr(Td("Footer")))))

      test(m"Whitespace permitted and ignored between list items"):
        t"""<ul><li>hello</li>\n  <li>world</li></ul>""".read[Html of "html"]
      . assert(_ == Html(Body(Ul(Li("hello"), Li("world")))))

      test(m"Non-whitespace text not permitted between list items"):
        try t"""<ul><li>hello</li>\n and <li>world</li></ul>""".read[Html of "html"]
        catch case exception: Exception => exception
      . assert(_ == ParseError(Html, Html.Position(21.u), Html.Issue.OnlyWhitespace('a')))

      test(m"Foreign SVG tag"):
        t"""<div><svg><circle r="1"/></svg></div>""".read[Html of Flow]
      .assert(_ == Div(Svg(Html.Node.foreign("circle", sci.Map(t"r" -> t"1")))))

      test(m"Nontrivial MathML example"):
        t"""<div>The equation is <math><mfrac><msup><mi>π</mi><mn>2</mn></msup><mn>6</mn></mfrac></math>.</div>"""
        . read[Html of Flow]
      . assert(_ == Div("The equation is ", Math(Html.Node.foreign("mfrac", sci.Map(), Html.Node.foreign("msup", sci.Map(), Html.Node.foreign("mi", sci.Map(), "π"), Html.Node.foreign("mn", sci.Map(), "2")), Html.Node.foreign("mn", sci.Map(), "6"))), "."))

      test(m"transparent tag with text"):
        t"""<p>Go <a href="https://example.com">home</a>.</p>""".read[Html of "p"]
      . assert(_ == P("Go ", A(href = url"https://example.com")("home"), "."))

      test(m"transparent tag only allows the right children"):
        try t"""<div><a href="#"><li>list item</li></a></div>""".read[Html of Flow]
        catch case exception: Exception => exception
      .assert(_ == ParseError(Html, Html.Position(18.u), Html.Issue.InadmissibleTag("li", "a")))

      test(m"transparent tag with element"):
        t"""<p>Go <a href="https://example.com"><em>home</em></a>.</p>""".read[Html of "p"]
      . assert(_ == P("Go ", A(href = url"https://example.com")(Em("home")), "."))

      test(m"transparent tag with additions"):
        t"""<div><video controls><source src="https://example.com/movie.mp4"></video></div>""".read[Html of "div"]
      . assert(_ == Div(Video(controls = true)(Source(src = url"https://example.com/movie.mp4"))))

      test(m"Tag subtype"):
        t"""<input type="button">""".read[Html of "input"]
      . assert(_ == Input.Button)

      test(m"Tag subtype with extra attributes"):
        t"""<input alt="whatever" type="button">""".read[Html of "input"]
      . assert(_ == Input.Button(alt = "whatever"))


      test(m"Parse RCDATA with no entities"):
        t"""<title>Push then Pull</title>""".read[Html of Metadata]
      . assert(_ == Title("Push then Pull"))

      test(m"Parse RCDATA with an entity"):
        t"""<title>Push &amp; Pull</title>""".read[Html of Metadata]
      . assert(_ == Title("Push & Pull"))

      test(m"Parse empty RCDATA"):
        t"""<title></title>""".read[Html of Metadata]
      . assert(_ == Title)

      test(m"Parse RCDATA starting with entity"):
        t"""<title>&amp; ampersand</title>""".read[Html of Metadata]
      . assert(_ == Title("& ampersand"))

      test(m"Parse RCDATA with fake tag"):
        t"""<title><push> &amp; <pull></title>""".read[Html of Metadata]
      . assert(_ == Title("<push> & <pull>"))

      test(m"Parse RCDATA ending with entity"):
        t"""<title>ampersand:&amp;</title>""".read[Html of Metadata]
      . assert(_ == Title("ampersand:&"))

      test(m"Parse RCDATA with only entity"):
        t"""<title>&amp;</title>""".read[Html of Metadata]
      . assert(_ == Title("&"))

      test(m"Parse RCDATA with invalid entity"):
        t"""<title>&ampersand;</title>""".read[Html of Metadata]
      . assert(_ == Title("&ampersand;"))

      test(m"Parse RCDATA with incomplete entity"):
        t"""<title>&a</title>""".read[Html of Metadata]
      . assert(_ == Title("&a"))

      test(m"Attribute with character entity"):
        t"""<img alt="To &amp; fro">""".read[Html of Flow]
      . assert(_ == Img(alt = "To & fro"))

      test(m"Attribute with numeric character entity"):
        t"""<img alt="Schlo&#223;">""".read[Html of Flow]
      . assert(_ == Img(alt = "Schloß"))

      test(m"Text with hex character entity"):
        t"""<p>value: &#x6A;</p>""".read[Html of Flow]
      . assert(_ == P("value: j"))

      test(m"Text with emoji character entity"):
        t"""<p>value: &#x1f600;""".read[Html of Flow]
      . assert(_ == P("value: 😀"))

      suite(m"Interpolator tests"):
        test(m"simple interpolator"):
          val comment = "comment"
          val attribute = "attribute"
          val more = "more"
          h"""<p title=$attribute><!-- $comment -->This some $more HTML.</p>"""
        . assert(_ == P("This is some HTML"))
