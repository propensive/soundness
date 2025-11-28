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

import soundness.{Table as _, *}

import autopsies.contrastExpectations
import errorDiagnostics.stackTraces
import strategies.throwUnsafely

object Tests extends Suite(m"Honeycomb Tests"):
  def run(): Unit =
    suite(m"Showing HTML"):
      import html5.*

      test(m"empty normal tag"):
        Div.show
      .check(_ == t"<div/>")

      test(m"empty unclosed tag"):
        Br.show
      .check(_ == t"<br>")

      test(m"tag with one attribute"):
        P(id = id"abc").show
      .check(_ == t"""<p id="abc"/>""")

      test(m"tag with two attributes"):
        P(id = id"abc", style = t"def").show
      .check(_ == t"""<p id="abc" style="def"/>""")

      test(m"unclosed tag with one attribute"):
        Hr(id = id"foo").show
      .check(_ == t"""<hr id="foo">""")

      test(m"unclosed tag with two attributes"):
        Hr(id = id"foo", style = t"bar").show
      .check(_ == t"""<hr id="foo" style="bar">""")

      test(m"non-self-closing tag"):
        Script.show
      .check(_ == t"""<script></script>""")

      test(m"tag with no attributes and children"):
        Div(Hr, Br).show
      .check(_ == t"""<div><hr><br></div>""")

      test(m"tag with text child"):
        P(t"hello world").show
      .check(_ == t"<p>hello world</p>")

      test(m"tag with mixed children"):
        P(t"hello ", Em(t"world"), t"!").show
      .check(_ == t"<p>hello <em>world</em>!</p>")

      test(m"deeper-nested elements"):
        Table(Tbody(Tr(Td(t"A")))).show
      .check(_ == t"<table><tbody><tr><td>A</td></tr></tbody></table>")

    suite(m"HTML parsing tests"):
      import Dom.{Div, P, Li, Area, Br, Ul, Input}
      def parse(text: Text): HtmlNode = unsafely(Dom.parse(Iterator(text)))

      test(m"simple empty tag"):
        parse(t"""<div></div>""")
      .assert(_ == Div)

      test(m"List"):
        parse(t"""<ul><li>item</li></ul>""")
      .assert(_ == Ul(Li("item")))

      test(m"simple tag with text"):
        parse(t"""<div>content</div>""")
      .assert(_ == Div("content"))

      test(m"simple self-closing tag"):
        parse(t"""<div/>""")
      .assert(_ == Div)

      test(m"simple comment tag"):
        parse(t"""<!--This is a comment-->""")
      .assert(_ == HtmlNode.Comment("This is a comment"))

      test(m"simple void tag"):
        parse(t"""<br>""")
      .assert(_ == Br)

      test(m"void tag with an attribute"):
        parse(t"""<area foo="bar">""")
      .assert(_ == Area(foo = t"bar"))

      test(m"void tag with an unquoted attribute"):
        parse(t"""<area foo=bar>""")
      .assert(_ == Area(foo = "bar"))

      test(m"void tag with a boolean attribute"):
        parse(t"""<input disabled>""")
      .assert(_ == Input(disabled = true))

      test(m"void tag with a single-quoted attribute"):
        parse(t"""<area foo='bar baz'>""")
      .assert(_ == Area(foo = "bar baz"))

      test(m"simple nested tag"):
        parse(t"""<div><area></div>""")
      .assert(_ == Div(Area))

      test(m"just text"):
        parse(t"""hello world""")
      .assert(_ == HtmlNode.Textual("hello world"))

      test(m"just text with entity"):
        parse(t"""to &amp; fro""")
      .assert(_ == HtmlNode.Textual("to & fro"))

      test(m"just an entity"):
        parse(t"""&amp;""")
      .assert(_ == HtmlNode.Textual("&"))

      test(m"mismatched closing tag"):
        try parse(t"""<em><b></em></b>""")
        catch case exception: Exception => exception
      .assert(_ == ParseError(HtmlNode, HtmlNode.Position(12.u), HtmlNode.Issue.MismatchedTag("b", "em")))

      test(m"unknown tag"):
        try parse(t"""<xyz>""")
        catch case exception: Exception => exception
      .assert(_ == ParseError(HtmlNode, HtmlNode.Position(1.u), HtmlNode.Issue.InvalidTag("xyz")))

      test(m"autoclosing tag"):
        parse(t"""<ul><li>First item\n<li>Second item</ul>""")
      .assert()
