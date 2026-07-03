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
package archimedes

import soundness.*

import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import xylophone.XmlSchema

object Tests extends Suite(m"Archimedes tests"):
  def run(): Unit =
    given XmlSchema = XmlSchema.Freeform

    suite(m"Rendering to XML"):
      test(m"Render a superscript"):
        Msup(Mi(t"x"), Mn(t"2")).xml.show
      .assert(_ == t"<msup><mi>x</mi><mn>2</mn></msup>")

      test(m"Render a fraction"):
        Mfrac(Mn(t"1"), Mn(t"2")).xml.show
      .assert(_ == t"<mfrac><mn>1</mn><mn>2</mn></mfrac>")

      test(m"Render a token with an attribute"):
        Mi(t"x", List(t"mathvariant" -> t"italic")).xml.show
      .assert(_ == t"""<mi mathvariant="italic">x</mi>""")

      test(m"Render an empty mspace"):
        Mspace(List(t"width" -> t"1em")).xml.show
      .assert(_ == t"""<mspace width="1em"/>""")

      test(m"Render the root math element with namespace"):
        Math(Msup(Mi(t"x"), Mn(t"2"))).xml.show
      .assert(_ == t"""<math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>x</mi><mn>2"""
          + t"</mn></msup></math>")

      test(m"Render the display attribute"):
        Math(List(Mn(t"1")), Display.Block).xml.show
      .assert(_.contains(t"""display="block""""))

    suite(m"Parsing from XML"):
      val nested = Math(Mrow(Mi(t"a"), Mo(t"+"), Mfrac(Mn(t"1"), Mi(t"b"))))
      val stretchy = Math(Mo(t"=", List(t"stretchy" -> t"false")))

      test(m"Parse a superscript from MathML text"):
        val source = t"""<math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>x</mi>"""
            + t"<mn>2</mn></msup></math>"

        source.read[Math].contents
      .assert(_ == List(Msup(Mi(t"x"), Mn(t"2"))))

      test(m"Round-trip a nested expression"):
        nested.xml.show.read[Math].xml.show
      .assert(_ == nested.xml.show)

      test(m"Preserve unknown attributes on round-trip"):
        stretchy.xml.show.read[Math]
      .assert(_ == stretchy)

      test(m"Reject a non-math root element"):
        capture[MathmlError](t"<svg><rect/></svg>".read[Math]).reason
      .assert(_ == MathmlError.Reason.NotMathml(t"svg"))

    suite(m"Embedding in and extracting from HTML"):
      val expression = Math(Mrow(Mi(t"a"), Mo(t"+"), Mn(t"1")))

      test(m"Render MathML as an HTML foreign element"):
        Math(Msup(Mi(t"x"), Mn(t"2"))).html.show
      .assert(_.contains(t"<math"))

      test(m"Extract MathML back out of the HTML tree"):
        MathmlReader.read(expression.html)
      .assert(_ == expression)
