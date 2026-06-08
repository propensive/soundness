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
package cataclysm

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTraces

import Css.Node.*

object Tests extends Suite(m"Cataclysm Tests"):
  def decl(key: Text, value: Text): Css.Node = Declaration(key, value)
  def rule(selector: Text, body: Css.Node*): Css.Node = Rule(selector, body.toList)
  def at(name: Text, prelude: Text, body: Css.Node*): Css.Node = At(name, prelude, body.toList)

  def run(): Unit =
    suite(m"CSS parsing"):
      test(m"a single flat rule with one declaration"):
        t"a { color: red; }".read[Css].rules
      . assert(_ == List(rule(t"a", decl(t"color", t"red"))))

      test(m"multiple declarations in one rule"):
        t"p { margin: 0; padding: 1px; }".read[Css].rules
      . assert(_ == List(rule(t"p", decl(t"margin", t"0"), decl(t"padding", t"1px"))))

      test(m"a final declaration without a trailing semicolon"):
        t"a { color: red }".read[Css].rules
      . assert(_ == List(rule(t"a", decl(t"color", t"red"))))

      test(m"nested rules are supported"):
        t"a { color: red; & b { color: blue } }".read[Css].rules
      . assert(_ == List(rule(t"a", decl(t"color", t"red"), rule(t"& b", decl(t"color", t"blue")))))

      val media = at(t"media", t"screen and (min-width: 700px)", rule(t"a", decl(t"color", t"red")))

      test(m"an at-rule block keeps its full prelude"):
        t"@media screen and (min-width: 700px) { a { color: red } }".read[Css].rules
      . assert(_ == List(media))

      test(m"an at-rule statement has no body"):
        t"""@import url("x.css");""".read[Css].rules
      . assert(_ == List(At(t"import", t"""url("x.css")""", Unset)))

      test(m"comments are stripped from selectors and values"):
        t"a /* x */ { color: /* y */ red }".read[Css].rules
      . assert(_ == List(rule(t"a", decl(t"color", t"red"))))

      test(m"a colon inside parentheses does not split a declaration"):
        t"a { background: url(http://e.com/i.png) }".read[Css].rules
      . assert(_ == List(rule(t"a", decl(t"background", t"url(http://e.com/i.png)"))))

      test(m"a semicolon inside a string does not terminate the value"):
        t"""a { content: "a;b" }""".read[Css].rules
      . assert(_ == List(rule(t"a", decl(t"content", t"\"a;b\""))))

      test(m"a brace inside a string does not open a block"):
        t"""a { content: "}" }""".read[Css].rules
      . assert(_ == List(rule(t"a", decl(t"content", t"\"}\""))))

      test(m"whitespace in a selector is normalized"):
        t"a  ,\n  b { color: red }".read[Css].rules
      . assert(_ == List(rule(t"a , b", decl(t"color", t"red"))))

    suite(m"CSS errors"):
      test(m"an unterminated comment is reported"):
        capture[CssError](t"a { /* unterminated }".read[Css]).reason
      . assert(_ == CssError.Reason.UnterminatedComment)

      test(m"an unterminated string is reported"):
        capture[CssError](t"""a { content: "x }""".read[Css]).reason
      . assert(_ == CssError.Reason.UnterminatedString)

      test(m"a missing closing brace is reported"):
        capture[CssError](t"a { color: red;".read[Css]).reason
      . assert(_ == CssError.Reason.UnexpectedEnd)
