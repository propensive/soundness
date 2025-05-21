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
┃    Soundness, version 0.31.0 for Scala 3.7.                                                      ┃
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

import html5.*

object Tests extends Suite(m"Honeycomb Tests"):
  def run(): Unit =
    suite(m"Showing HTML"):
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
