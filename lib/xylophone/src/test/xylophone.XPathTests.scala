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
package xylophone

import soundness.*

import strategies.throwUnsafely

object XPathTests extends Suite(m"Xylophone XPath evaluation tests"):
  def run(): Unit =
    given XmlSchema = XmlSchema.Freeform
    import XPath.Value

    val document: Xml =
      t"""<app><!-- note --><?render fast?><div class="container active" id="main"><button data-test="submit">Submit</button><button data-test="cancel">Cancel</button><span id="x" class="badge">7</span></div><div class="container"><ul class="list"><li n="1">one</li><li n="2">two</li><li n="3">three</li></ul><![CDATA[raw]]></div><footer xml:lang="en-GB"><p>done</p></footer></app>"""
      . read[Xml]

    suite(m"Browser-automation locators"):
      test(m"a text predicate finds a button by its label"):
        document.selectText(xp"//button[text()='Submit']/@data-test")
      . assert(_ == t"submit")

      test(m"an attribute predicate finds an element anywhere"):
        document.selectText(xp"//*[@data-test='cancel']")
      . assert(_ == t"Cancel")

      test(m"a contains predicate matches a partial attribute value"):
        document.selectText(xp"//*[contains(@class,'active')]/@id")
      . assert(_ == t"main")

      test(m"a parent step reaches the enclosing element"):
        document.select(xp"//span[@id='x']/..") match
          case Fragment(element: Element) => element.attributes.fetch(t"id")
          case _                          => Unset
      . assert(_ == t"main")

      test(m"an attribute-presence predicate selects only carriers"):
        document.evaluate(xp"count(//*[@data-test])")
      . assert(_ == Value.Numeric(2))

    suite(m"Axes"):
      test(m"descendant-or-self finds elements at any depth"):
        document.evaluate(xp"count(//li)")
      . assert(_ == Value.Numeric(3))

      test(m"a positional path descends by ordinal"):
        document.selectText(xp"/app/div[2]/ul/li[2]")
      . assert(_ == t"two")

      test(m"following-sibling in proximity order"):
        document.selectText(xp"//li[1]/following-sibling::li[1]")
      . assert(_ == t"two")

      test(m"preceding-sibling in proximity order"):
        document.selectText(xp"//li[3]/preceding-sibling::li[1]")
      . assert(_ == t"two")

      test(m"the nearest ancestor is position one on the ancestor axis"):
        document.selectText(xp"//li/ancestor::*[1]/@class")
      . assert(_ == t"list")

      test(m"the attribute axis with a wildcard yields every attribute"):
        document.evaluate(xp"count(//span/@*)")
      . assert(_ == Value.Numeric(2))

      test(m"following excludes descendants"):
        document.evaluate(xp"count(//ul/following::*)")
      . assert(_ == Value.Numeric(2))

      test(m"a comment node test finds comments"):
        document.evaluate(xp"count(//comment())")
      . assert(_ == Value.Numeric(1))

      test(m"a processing-instruction node test matches by target"):
        document.evaluate(xp"count(//processing-instruction('render'))")
      . assert(_ == Value.Numeric(1))

      test(m"a text node test selects character data"):
        document.selectText(xp"//li[1]/text()")
      . assert(_ == t"one")

    suite(m"Node-set semantics"):
      test(m"union results arrive in document order"):
        document.select(xp"//span | //button") match
          case Fragment(nodes*) =>
            nodes.map:
              case element: Element => element.label.s
              case _                => "?"
            . mkString(",").tt
      . assert(_ == t"button,button,span")

      test(m"revisited nodes deduplicate"):
        document.evaluate(xp"count(//li/../li)")
      . assert(_ == Value.Numeric(3))

      test(m"a predicate of position()=last() selects the final node"):
        document.selectText(xp"//li[position()=last()]")
      . assert(_ == t"three")

      test(m"an arithmetic predicate resolves to a position"):
        document.selectText(xp"//li[last() - 1]")
      . assert(_ == t"two")

      test(m"an element's string-value concatenates its descendants"):
        document.selectText(xp"//footer")
      . assert(_ == t"done")

      test(m"CDATA contributes to the string-value"):
        document.evaluate(xp"contains(/app/div[2], 'raw')")
      . assert(_ == Value.Truth(true))

    suite(m"Coercions and comparisons"):
      test(m"a number compares against a numeric string"):
        document.evaluate(xp"1 = '1'")
      . assert(_ == Value.Truth(true))

      test(m"NaN is unequal to itself"):
        document.evaluate(xp"number('x') != number('x')")
      . assert(_ == Value.Truth(true))

      test(m"an integral number renders without a decimal point"):
        document.evaluate(xp"string(2 div 2)")
      . assert(_ == Value.Textual(t"1"))

      test(m"a fractional number renders with its decimal part"):
        document.evaluate(xp"string(0.5)")
      . assert(_ == Value.Textual(t"0.5"))

      test(m"a node-set compares existentially"):
        document.evaluate(xp"//li = 'two'")
      . assert(_ == Value.Truth(true))

      test(m"relational comparison over attribute numbers"):
        document.evaluate(xp"count(//li[@n > 1])")
      . assert(_ == Value.Numeric(2))

    suite(m"Function library"):
      test(m"starts-with tests a prefix"):
        document.evaluate(xp"starts-with('hello','he')")
      . assert(_ == Value.Truth(true))

      test(m"substring rounds its bounds"):
        document.evaluate(xp"substring('12345', 1.5, 2.6)")
      . assert(_ == Value.Textual(t"234"))

      test(m"substring-before and substring-after split at the match"):
        document.evaluate(xp"concat(substring-before('a-b','-'), substring-after('a-b','-'))")
      . assert(_ == Value.Textual(t"ab"))

      test(m"normalize-space collapses interior runs"):
        document.evaluate(xp"normalize-space('  a   b ')")
      . assert(_ == Value.Textual(t"a b"))

      test(m"translate maps and deletes characters"):
        document.evaluate(xp"translate('--abc--','abc-','ABC')")
      . assert(_ == Value.Textual(t"ABC"))

      test(m"sum totals a node-set numerically"):
        document.evaluate(xp"sum(//li/@n)")
      . assert(_ == Value.Numeric(6))

      test(m"round is floor of x plus a half"):
        document.evaluate(xp"round(2.5) + round(-2.5)")
      . assert(_ == Value.Numeric(1))

      test(m"string-length defaults to the context node"):
        document.evaluate(xp"string-length('four')")
      . assert(_ == Value.Numeric(4))

      test(m"not negates a boolean"):
        document.evaluate(xp"not(false())")
      . assert(_ == Value.Truth(true))

      test(m"lang matches a language subtag from an ancestor"):
        document.selectText(xp"//p[lang('en')]")
      . assert(_ == t"done")

      test(m"name reports an element's label"):
        document.evaluate(xp"name(//span)")
      . assert(_ == Value.Textual(t"span"))

    suite(m"Variables and errors"):
      test(m"variables resolve from the given bindings"):
        document.evaluate(XPath(XPath.variable(t"n") + 1), Map(t"n" -> Value.Numeric(2)))
      . assert(_ == Value.Numeric(3))

      test(m"an unbound variable raises an error"):
        try
          document.evaluate(XPath(XPath.variable(t"missing")))
          t"evaluated"
        catch case error: XPath.EvaluationError => t"unbound"
      . assert(_ == t"unbound")

      test(m"an unknown function raises an error"):
        try
          document.evaluate(xp"frobnicate()")
          t"evaluated"
        catch case error: XPath.EvaluationError => t"unknown"
      . assert(_ == t"unknown")

      test(m"the namespace axis is unsupported"):
        try
          document.select(xp"//namespace::x")
          t"selected"
        catch case error: XPath.EvaluationError => t"unsupported"
      . assert(_ == t"unsupported")

      test(m"the id function is unsupported"):
        try
          document.evaluate(xp"id('main')")
          t"evaluated"
        catch case error: XPath.EvaluationError => t"unsupported"
      . assert(_ == t"unsupported")
