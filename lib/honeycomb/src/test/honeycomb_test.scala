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
package honeycomb

import scala.collection.immutable as sci

import soundness.*

import errorDiagnostics.stackTraces
import strategies.throwUnsafely

object Tests extends Suite(m"Honeycombd Tests"):
  def run(): Unit =
    test(m"show comment"):
      Comment("hello world").show
    . assert(_ == t"<!--hello world-->")

    suite(m"HTML parsing tests"):
      import doms.html.whatwg
      import whatwg.*

      test(m"simple empty tag"):
        t"""<div></div>""".read[Html]
      . assert(_ == Div)

      test(m"List"):
        t"""<ul><li>item</li></ul>""".read[Html of Flow]
      . assert(_ == Ul(Li("item")))

      test(m"simple tag with text"):
        t"""<div>content</div>""".read[Html of "div"]
      . assert(_ == Div("content"))

      test(m"more than one node"):
        t"""<div>content</div><p>more content</p>""".read[Html of Flow]
      . assert(_ == Fragment(Div("content"), P("more content")))

      test(m"more than one node with comment"):
        t"""<div>content</div><!-- comment --><div>more content</div>""".read[Html of "div"]
      . assert(_ == Fragment(Div("content"), Comment(" comment "), Div("more content")))

      test(m"simple self-closing tag"):
        t"""<div />""".read[Html of "div"]
      . assert(_ == Div)

      test(m"self-closing tag with attributes"):
        t"""<div style="bar"/>""".read[Html of "div"]
      . assert(_ == Div(style = t"bar"))

      test(m"case-insensitive element"):
        t"""<DIV style="bar">hello world</DIV>""".read[Html of "div"]
      . assert(_ == Div(style = t"bar")("hello world"))

      test(m"simple comment tag"):
        t"""<!--This is a comment-->""".read[Html of Flow]
      . assert(_ == Comment("This is a comment"))

      test(m"simple void tag"):
        t"""<br>""".read[Html of Flow]
      . assert(_ == Br)

      test(m"void tag with an attribute"):
        t"""<area style="bar">""".read[Html of "area"]
      . assert(_ == Area(style = t"bar"))

      test(m"void tag with an unquoted attribute"):
        t"""<area style=bar>""".read[Html of Flow]
      . assert(_ == Area(style = t"bar"))

      test(m"void tag with a boolean attribute"):
        t"""<input disabled>""".read[Html of "input"]
      . assert(_ == Input(disabled = true))

      test(m"void tag with a single-quoted attribute"):
        t"""<br style='bar baz'>""".read[Html of Flow]
      . assert(_ == Br(style = t"bar baz"))

      test(m"simple nested tag"):
        t"""<div><area></div>""".read[Html of Flow]
      . assert(_ == Div(Area))

      test(m"just text"):
        t"""hello world""".read[Html of Flow]
      . assert(_ == TextNode("hello world"))

      test(m"just text with entity"):
        t"""to &amp; fro""".read[Html of Flow]
      . assert(_ == TextNode("to & fro"))

      test(m"just an entity"):
        t"""&amp;""".read[Html of Flow]
      . assert(_ == TextNode("&"))

      test(m"mismatched closing tag"):
        try t"""<em><b></em></b>""".read[Html of Phrasing]
        catch case exception: Exception => exception
      . assert(_ == ParseError(Html, Html.Position(1.u, 8.u), Html.Issue.MismatchedTag("b", "em")))

      test(m"unknown tag"):
        try t"""<scrip>""".read[Html of Phrasing]
        catch case exception: Exception => exception
      . assert(_ == ParseError(Html, Html.Position(1.u, 2.u), Html.Issue.InvalidTag("scrip")))

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
      . assert(_ == Ul(Li("First item")))

      test(m"unclosed paragraph"):
        t"""<p>para""".read[Html of Flow]
      . assert(_ == P("para"))

      test(m"follow-on whitespace"):
        t"""<p>para</p>\n""".read[Html of Flow]
      . assert(_ == Fragment(P("para"), t"\n"))

      test(m"empty content"):
        t"".read[Html of Flow]
      . assert(_ == Fragment())

      test(m"failing example"):
        t"""<p>x<img></p>\n""".read[Html of Flow]
      . assert(_ == Fragment(P("x", Img), t"\n"))

      test(m"autoclosing adjacent tags"):
        t"""<ul><li>First item<li>Second item</ul>""".read[Html of Flow]
      . assert(_ == Ul(Li("First item"), Li("Second item")))

      test(m"unclosed tag 1"):
        try t"""<ul><li>First item</li>""".read[Html of Flow]
        catch case exception: Exception => exception
      . assert(_ == ParseError(Html, Html.Position(1.u, 23.u), Html.Issue.Incomplete("ul")))

      test(m"unclosed tag 2"):
        try t"""<ul><li>First item""".read[Html of Flow]
        catch case exception: Exception => exception
      . assert(_ == ParseError(Html, Html.Position(1.u, 18.u), Html.Issue.Incomplete("ul")))

      test(m"infer both <head> and <body>"):
        t"""<title>Page title</title><p>A paragraph</p>""".read[Html of "html"]
      . assert(_ == Html(Head(Title("Page title")), Body(P("A paragraph"))))


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


        test(m"<thead> works like <tbody>"):
          t"""<table class="test"><thead><tr><th>First<td>Second<td>Third</table>""".read[Html of Flow]
        . assert: result =>
            given (Stylesheet of "test" | "foo") = Stylesheet()
            result == Table.test(Thead(Tr(Th("First"), Td("Second"), Td("Third"))))

        test(m"Generic stylesheet"):
          t"""<table class="test"><thead><tr><th>First<td>Second<td>Third</table>""".read[Html of Flow]
        . assert: result =>
            import stylesheets.uncheckedClasses
            result == Table.test(Thead(Tr(Th("First"), Td("Second"), Td("Third"))))

        test(m"<tfoot> closes inferred <tbody>"):
          t"""<table><tr><th>First<td>Second<td>Third<tfoot><tr><td>Footer</table>""".read[Html of Flow]
        . assert(_ == Table(Tbody(Tr(Th("First"), Td("Second"), Td("Third"))), Tfoot(Tr(Td("Footer")))))

      test(m"Whitespace permitted and ignored between list items"):
        t"""<ul><li>hello</li>\n  <li>world</li></ul>""".read[Html of "html"]
      . assert(_ == Html(Body(Ul(Li("hello"), Li("world")))))

      test(m"Non-whitespace text not permitted between list items"):
        try t"""<ul><li>hello</li>\n and <li>world</li></ul>""".read[Html of "html"]
        catch case exception: Exception => exception
      . assert(_ == ParseError(Html, Html.Position(2.u, 2.u), Html.Issue.OnlyWhitespace('a')))

      test(m"Foreign SVG tag"):
        t"""<div><svg><circle r="1"/></svg></div>""".read[Html of Flow]
      . assert(_ == Div(Svg(Element.foreign("circle", sci.Map(t"r" -> t"1")))))

      test(m"Nontrivial MathML example"):
        t"""<div>The equation is <math><mfrac><msup><mi>π</mi><mn>2</mn></msup><mn>6</mn></mfrac></math>.</div>"""
        . read[Html of Flow]
      . assert(_ == Div("The equation is ", Math(Element.foreign("mfrac", sci.Map(), Element.foreign("msup", sci.Map(), Element.foreign("mi", sci.Map(), "π"), Element.foreign("mn", sci.Map(), "2")), Element.foreign("mn", sci.Map(), "6"))), "."))

      test(m"transparent tag with text"):
        t"""<p>Go <a href="https://example.com">home</a>.</p>""".read[Html of "p"]
      . assert(_ == P("Go ", A(href = url"https://example.com")("home"), "."))

      test(m"transparent tag only allows the right children"):
        try t"""<div><a href="#"><li>list item</li></a></div>""".read[Html of Flow]
        catch case exception: Exception => exception
      . assert(_ == ParseError(Html, Html.Position(1.u, 18.u), Html.Issue.InadmissibleTag("li", "a")))

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

      val example = Html(Head(Title("Heading")), Body(P("body")))
      test(m"Parse Document without doctype"):
        t"""<title>Heading</title>
            <p>body""".load[Html]
      . assert(_ == Document(example, doms.html.whatwg))

      test(m"Parse Document with doctype"):
        t"""<!doctype html>
            <title>Heading</title>
            <p>body""".load[Html]
      . assert(_ == Document(example, doms.html.whatwg))

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

      test(m"Modify attribute"):
        val img: Element of "img" in Whatwg = Img(alt = "alternative")
        img.alt = t"different"
      . assert(_ == Img(alt = "different"))

      test(m"Attribute with numeric character entity"):
        t"""<img alt="Schlo&#223;">""".read[Html of Flow]
      . assert(_ == Img(alt = "Schloß"))

      test(m"Text with hex character entity"):
        t"""<p>value: &#x6A;</p>""".read[Html of Flow]
      . assert(_ == P("value: j"))

      test(m"Text with emoji character entity"):
        t"""<p>value: &#x1f600;""".read[Html of Flow]
      . assert(_ == P("value: 😀"))

      test(m"attribute access"):
        Img(alt = "hello world").alt
      . assert(_ == t"hello world")

      test(m"typed attribute access"):
        Img(width = 50).width
      . assert(_ == 50)

      suite(m"Interpolator tests"):
        import attributives.textAttributes

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
        . assert(_ == Div(`class` = t"foo")(P("hello")))

        test(m"class can be added to element with attributes"):
          import honeycomb.stylesheets.uncheckedClasses
          Img.foo(alt = t"bar")
        . assert(_ == Img(`class` = t"foo", alt = t"bar"))

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
        . assert(_ == TextNode(t"whole text"))

        test(m"pattern matcher"):
          Div(title = "text")(Ul(Li("hello")), P("more")).absolve match
            case h"""<div title=$att><ul>${value2}</ul>${value1}</div>""" =>
              (att, value2, value1)
        . assert(_ == ("text", Li("hello"), P("more")))

        test(m"extractor on tag body"):
          Form(Input(title = "text", disabled = true, style = t"testing"), "text").absolve match
            case h"""<form><input $atts>$more</form>""" => atts
        . assert(_ == ListMap(t"style" -> t"testing", t"disabled" -> Unset, t"title" -> t"text"))

        test(m"extractor on tag body with removals"):
          Form(Input(title = "text", disabled = true, style = t"testing"), "text").absolve match
            case h"""<form><input style=$style $atts>$more</form>""" => atts
        . assert(_ == ListMap(t"disabled" -> Unset, t"title" -> t"text"))

        test(m"extractor on attribute"):
          Form(Input(title = "text", disabled = true, style = t"testing"), "text").absolve match
            case h"""<form><input style=$style>$more</form>""" => style
        . assert(_ == t"testing")

        test(m"extractor of text"):
          P("whole text").absolve match
            case h"""<p $atts>$whole</p>""" => whole
        . assert(_ == TextNode(t"whole text"))

        test(m"extractor of comment"):
          P(Comment("this is the comment")).absolve match
            case h"""<p $atts><!--$comment--></p>""" => comment
        . assert(_ == t"this is the comment")

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
        . assert(_ == Img(alt = t"titletext"))

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
