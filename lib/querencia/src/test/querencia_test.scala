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
package querencia

import soundness.*

import htmlDoms.html4Transitional
import html4Transitional.*

// Querencia navigates the DOM (described by WebIDL) at compile time, builds a `Foreign.Expression`,
// renders it to JavaScript, and lets such a value be used as a Honeycomb event handler. These tests
// assert the rendered JavaScript and the expression AST, and that a foreign DOM value is accepted
// as an `onclick` attribute (where plain text now is not).
object Tests extends Suite(m"Querencia tests"):
  def run(): Unit =
    suite(m"DOM navigation and JavaScript rendering"):
      test(m"a navigated DOM call renders to JavaScript"):
        Javascript.serialize(document.getElementById(t"foo").focus().expr)
      . assert(_ == t"document.getElementById('foo').focus()")

      test(m"array indexing renders to a bracket access"):
        Javascript.serialize(document.querySelectorAll(t".btn")(0).click().expr)
      . assert(_ == t"document.querySelectorAll('.btn')[0].click()")

      test(m"the `window` global renders a method call"):
        Javascript.serialize(window.alert(t"hi").expr)
      . assert(_ == t"window.alert('hi')")

      test(m"navigating through `window.document` chains"):
        Javascript.serialize(window.document.getElementById(t"x").focus().expr)
      . assert(_ == t"window.document.getElementById('x').focus()")

      test(m"a string argument is escaped for the JavaScript string literal"):
        Javascript.serialize(document.getElementById(t"it's a \\ backslash").expr)
      . assert(_ == t"document.getElementById('it\\'s a \\\\ backslash')")

      test(m"a member access has the precise refined foreign type"):
        val element: Foreign of "HTMLElement" from Browser = document.getElementById(t"foo")

        element.expr
      . assert: expr =>
          val root = Foreign.Expression.Reference(t"document")
          val select = Foreign.Expression.Select(root, t"getElementById", t"Document")
          expr == Foreign.Expression.Apply(select, List(Foreign.Expression.Literal(t"foo")))

      test(m"indexing yields the element's foreign type"):
        val element: Foreign of "HTMLElement" from Browser = document.querySelectorAll(t".x")(0)

        element.expr
      . assert:
          case Foreign.Expression.Index(_, Foreign.Expression.Literal(index)) => index == 0
          case _                                                               => false

    suite(m"Honeycomb integration"):
      test(m"a foreign DOM expression is accepted as an `onclick` handler"):
        Button(onclick = document.getElementById(t"foo").focus()).show
      . assert(_.contains(t"onclick=\"document.getElementById('foo').focus()\""))

      test(m"a chained handler renders inside the rendered HTML"):
        Button(onclick = document.querySelector(t".x").click()).show
      . assert(_.contains(t"document.querySelector('.x').click()"))
