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
  // ── CSS-structure helpers ───────────────────────────────────────────────
  def decl(key: Text, value: Text): Css.Node = Declaration(key, value)
  def at(name: Text, prelude: Text, body: Css.Node*): Css.Node = At(name, prelude, body.toList)

  def rule(selector: Text, body: Css.Node*): Css.Node =
    Rule(SelectorParser.parse(selector), body.toList)

  // ── selector helpers ────────────────────────────────────────────────────
  def parse(text: Text): SelectorList = SelectorParser.parse(text)
  def sl(parts: Selector*): SelectorList = SelectorList(parts.toList)
  def cx(head: Compound, rest: (Combinator, Compound)*): Selector = Selector(Unset, head, rest.toList)
  def rel(lead: Combinator, head: Compound): Selector = Selector(lead, head, Nil)
  def cpd(parts: Simple*): Compound = Compound(parts.toList)
  def typ(name: Text): Simple = Simple.Type(Unset, name)
  def cls(name: Text): Simple = Simple.Class(name)
  def hid(name: Text): Simple = Simple.Id(name)
  def uni: Simple = Simple.Universal(Unset)
  def pc(name: Text): Simple = Simple.PseudoClass(name, Unset)

  val desc: Combinator = Combinator.Descendant
  val child: Combinator = Combinator.Child
  val next: Combinator = Combinator.NextSibling
  val subseq: Combinator = Combinator.SubsequentSibling
  val col: Combinator = Combinator.Column

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

    suite(m"Simple selectors"):
      test(m"a type selector"):
        parse(t"div")
      . assert(_ == sl(cx(cpd(typ(t"div")))))

      test(m"a class selector"):
        parse(t".header")
      . assert(_ == sl(cx(cpd(cls(t"header")))))

      test(m"an id selector"):
        parse(t"#main")
      . assert(_ == sl(cx(cpd(hid(t"main")))))

      test(m"a universal selector"):
        parse(t"*")
      . assert(_ == sl(cx(cpd(uni))))

      test(m"the nesting selector"):
        parse(t"&")
      . assert(_ == sl(cx(cpd(Simple.Nesting))))

      test(m"a compound of type, class and id"):
        parse(t"a.button#go")
      . assert(_ == sl(cx(cpd(typ(t"a"), cls(t"button"), hid(t"go")))))

      test(m"the nesting selector with a class"):
        parse(t"&.active")
      . assert(_ == sl(cx(cpd(Simple.Nesting, cls(t"active")))))

    suite(m"Combinators"):
      test(m"a descendant combinator"):
        parse(t"a b")
      . assert(_ == sl(cx(cpd(typ(t"a")), (desc, cpd(typ(t"b"))))))

      test(m"a child combinator with spaces"):
        parse(t"a > b")
      . assert(_ == sl(cx(cpd(typ(t"a")), (child, cpd(typ(t"b"))))))

      test(m"a child combinator without spaces"):
        parse(t"a>b")
      . assert(_ == sl(cx(cpd(typ(t"a")), (child, cpd(typ(t"b"))))))

      test(m"a next-sibling combinator"):
        parse(t"a + b")
      . assert(_ == sl(cx(cpd(typ(t"a")), (next, cpd(typ(t"b"))))))

      test(m"a subsequent-sibling combinator"):
        parse(t"a ~ b")
      . assert(_ == sl(cx(cpd(typ(t"a")), (subseq, cpd(typ(t"b"))))))

      test(m"a column combinator"):
        parse(t"col || td")
      . assert(_ == sl(cx(cpd(typ(t"col")), (col, cpd(typ(t"td"))))))

      val complex3 =
        sl(cx(cpd(typ(t"ul"), cls(t"nav")), (child, cpd(typ(t"li"))), (desc, cpd(typ(t"a")))))

      test(m"a three-compound complex selector"):
        parse(t"ul.nav > li a")
      . assert(_ == complex3)

      test(m"a comma-separated selector list"):
        parse(t"a, .b")
      . assert(_ == sl(cx(cpd(typ(t"a"))), cx(cpd(cls(t"b")))))

    suite(m"Attribute selectors"):
      val exactTest = AttributeTest(AttributeMatcher.Exact, t"\"text\"", Unset)
      val prefixTest = AttributeTest(AttributeMatcher.Prefix, t"\"https\"", Unset)
      val substringTest = AttributeTest(AttributeMatcher.Substring, t"foo", Unset)
      val modifierTest = AttributeTest(AttributeMatcher.Exact, t"\"x\"", 'i')

      test(m"attribute presence"):
        parse(t"[disabled]")
      . assert(_ == sl(cx(cpd(Simple.Attribute(Unset, t"disabled", Unset)))))

      test(m"an exact attribute match with a quoted value"):
        parse(t"""[type="text"]""")
      . assert(_ == sl(cx(cpd(Simple.Attribute(Unset, t"type", exactTest)))))

      test(m"a prefix attribute match"):
        parse(t"""[href^="https"]""")
      . assert(_ == sl(cx(cpd(Simple.Attribute(Unset, t"href", prefixTest)))))

      test(m"a substring attribute match with an unquoted value"):
        parse(t"[data*=foo]")
      . assert(_ == sl(cx(cpd(Simple.Attribute(Unset, t"data", substringTest)))))

      test(m"an attribute match with a case-insensitive modifier"):
        parse(t"""[lang="x" i]""")
      . assert(_ == sl(cx(cpd(Simple.Attribute(Unset, t"lang", modifierTest)))))

    suite(m"Namespaces"):
      test(m"a namespaced type selector"):
        parse(t"svg|rect")
      . assert(_ == sl(cx(cpd(Simple.Type(Namespace.Named(t"svg"), t"rect")))))

      test(m"an any-namespace type selector"):
        parse(t"*|a")
      . assert(_ == sl(cx(cpd(Simple.Type(Namespace.Any, t"a")))))

      test(m"a default-namespace type selector"):
        parse(t"|a")
      . assert(_ == sl(cx(cpd(Simple.Type(Namespace.Default, t"a")))))

    suite(m"Pseudo-classes and pseudo-elements"):
      val notArg = PseudoArgument.Selectors(sl(cx(cpd(cls(t"a"))), cx(cpd(cls(t"b")))))
      val hasArg = PseudoArgument.Selectors(sl(rel(child, cpd(typ(t"img")))))

      test(m"a simple pseudo-class"):
        parse(t"a:hover")
      . assert(_ == sl(cx(cpd(typ(t"a"), pc(t"hover")))))

      test(m"a pseudo-element"):
        parse(t"p::before")
      . assert(_ == sl(cx(cpd(typ(t"p"), Simple.PseudoElement(t"before", Unset)))))

      test(m":not() takes a selector list"):
        parse(t":not(.a, .b)")
      . assert(_ == sl(cx(cpd(Simple.PseudoClass(t"not", notArg)))))

      test(m":has() takes a relative selector list"):
        parse(t":has(> img)")
      . assert(_ == sl(cx(cpd(Simple.PseudoClass(t"has", hasArg)))))

      test(m":lang() keeps its argument as raw text"):
        parse(t":lang(en)")
      . assert(_ == sl(cx(cpd(Simple.PseudoClass(t"lang", PseudoArgument.Raw(t"en"))))))

    suite(m"An+B"):
      def nth(text: Text): PseudoArgument =
        parse(text).selectors.head.head.parts.head match
          case Simple.PseudoClass(_, argument: PseudoArgument) => argument
          case _                                               => PseudoArgument.Raw(t"")

      val ofList = sl(cx(cpd(cls(t"x"))))

      test(m"odd keyword"):
        nth(t":nth-child(odd)")
      . assert(_ == PseudoArgument.Nth(2, 1, Unset))

      test(m"even keyword"):
        nth(t":nth-child(even)")
      . assert(_ == PseudoArgument.Nth(2, 0, Unset))

      test(m"a bare integer"):
        nth(t":nth-child(5)")
      . assert(_ == PseudoArgument.Nth(0, 5, Unset))

      test(m"an an+b expression"):
        nth(t":nth-child(2n+1)")
      . assert(_ == PseudoArgument.Nth(2, 1, Unset))

      test(m"an an+b expression with spaces"):
        nth(t":nth-child(2n + 1)")
      . assert(_ == PseudoArgument.Nth(2, 1, Unset))

      test(m"a negative coefficient"):
        nth(t":nth-child(-n+3)")
      . assert(_ == PseudoArgument.Nth(-1, 3, Unset))

      test(m"an of-clause"):
        nth(t":nth-child(2n of .x)")
      . assert(_ == PseudoArgument.Nth(2, 0, ofList))

    suite(m"Selector errors"):
      test(m"an empty selector is rejected"):
        capture[CssError](parse(t"")).reason
      . assert(_ == CssError.Reason.EmptySelector)

      test(m"unexpected trailing input is rejected"):
        capture[CssError](parse(t"a!")).reason
      . assert(_ == CssError.Reason.UnexpectedChar('!'))

    suite(m"Property validation"):
      test(m"a known property is accepted"):
        t"a { color: red }".read[Css].rules
      . assert(_ == List(rule(t"a", decl(t"color", t"red"))))

      test(m"an unknown property is rejected"):
        capture[CssError](t"a { colour: red }".read[Css]).reason
      . assert(_ == CssError.Reason.UnknownProperty(t"colour"))

      test(m"a custom property is accepted"):
        t"a { --my-color: red }".read[Css].rules
      . assert(_ == List(rule(t"a", decl(t"--my-color", t"red"))))

      test(m"a known property's value grammar is loaded"):
        PropertyDef.of(t"color").vouch.syntax
      . assert(_ == t"<color>")

      test(m"an unknown property has no definition"):
        PropertyDef.of(t"colour").absent
      . assert(_ == true)

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
