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
┃    Soundness, version 0.63.0.                                                                    ┃
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

    def body(ergo: Text): Text =
      Ergo.parse(ergo).xml.show.sub(t"""<math xmlns="http://www.w3.org/1998/Math/MathML">""", t"")
      . sub(t"</math>", t"")

    suite(m"Scripts and fractions"):
      test(m"superscript"):
        body(t"(x↗y)")
      .assert(_ == t"<msup><mi>x</mi><mi>y</mi></msup>")

      test(m"superscript of a group"):
        body(t"(x↗(y + 1))")
      .assert(_ == t"<msup><mi>x</mi><mrow><mi>y</mi><mo>+</mo><mn>1</mn></mrow></msup>")

      test(m"combined sub and superscript merge into msubsup"):
        body(t"(x↘i↗2)")
      .assert(_ == t"<msubsup><mi>x</mi><mi>i</mi><mn>2</mn></msubsup>")

      test(m"fraction binds looser than scripts"):
        body(t"(a/b↗2)")
      .assert(_ == t"<mfrac><mi>a</mi><msup><mi>b</mi><mn>2</mn></msup></mfrac>")

      test(m"multi-digit number is one mn"):
        body(t"(x↗123)")
      .assert(_ == t"<msup><mi>x</mi><mn>123</mn></msup>")

      test(m"multi-letter run is one identifier"):
        body(t"(sin)")
      .assert(_ == t"<mi>sin</mi>")

    suite(m"Roots"):
      test(m"square root"):
        body(t"(√x)")
      .assert(_ == t"<msqrt><mi>x</mi></msqrt>")

      test(m"nth root from an adjacent index"):
        body(t"(3√x)")
      .assert(_ == t"<mroot><mi>x</mi><mn>3</mn></mroot>")

    suite(m"Big operators via under/over"):
      test(m"sum with limits"):
        body(t"(∑↓(i = 1)↑n x)")
      .assert: result =>
        result == t"<munderover><mo>∑</mo><mrow><mi>i</mi><mo>=</mo><mn>1</mn></mrow>"
            + t"<mi>n</mi></munderover><mi>x</mi>"

    suite(m"Accents"):
      test(m"an mo over a base is an accent"):
        body(t"(x↑^)")
      .assert(_ == t"""<mover accent="true"><mi>x</mi><mo>^</mo></mover>""")

      test(m"an identifier over a base is an ordinary limit"):
        body(t"(x↑n)")
      .assert(_ == t"<mover><mi>x</mi><mi>n</mi></mover>")

    suite(m"Vectors and matrices (self-delimiting)"):
      test(m"row vector"):
        body(t"(⋯((1)(2)(3)))")
      .assert(_ == t"<mtable><mtr><mtd><mn>1</mn></mtd><mtd><mn>2</mn></mtd>"
          + t"<mtd><mn>3</mn></mtd></mtr></mtable>")

      test(m"column vector"):
        body(t"(⋮((a)(b)))")
      .assert(_ == t"<mtable><mtr><mtd><mi>a</mi></mtd></mtr><mtr><mtd><mi>b</mi></mtd></mtr>"
          + t"</mtable>")

      test(m"2x2 matrix"):
        body(t"(⋱(((1)(2))((3)(4))))")
      .assert(_ == t"<mtable><mtr><mtd><mn>1</mn></mtd><mtd><mn>2</mn></mtd></mtr><mtr>"
          + t"<mtd><mn>3</mn></mtd><mtd><mn>4</mn></mtd></mtr></mtable>")

      test(m"a single-cell row keeps a multi-token expression together"):
        body(t"(⋱(((a b))((c))))")
      .assert(_ == t"<mtable><mtr><mtd><mrow><mi>a</mi><mi>b</mi></mrow></mtd></mtr><mtr>"
          + t"<mtd><mi>c</mi></mtd></mtr></mtable>")

    suite(m"A whole formula"):
      test(m"the quadratic formula"):
        body(t"(x = (-b ± √(b↗2 - 4 a c))/(2 a))")
      .assert: result =>
        result == t"<mi>x</mi><mo>=</mo><mfrac><mrow><mo>-</mo><mi>b</mi><mo>±</mo><msqrt>"
            + t"<mrow><msup><mi>b</mi><mn>2</mn></msup><mo>-</mo><mn>4</mn><mi>a</mi><mi>c</mi>"
            + t"</mrow></msqrt></mrow><mrow><mn>2</mn><mi>a</mi></mrow></mfrac>"

    suite(m"Escaping"):
      test(m"a lone operator glyph is a literal mo"):
        body(t"((↗))")
      .assert(_ == t"<mo>↗</mo>")

    suite(m"Attribute directives"):
      test(m"a parameterised colour directive"):
        body(t"(x●(red))")
      .assert(_ == t"""<mi mathcolor="red">x</mi>""")

      test(m"a boolean true glyph"):
        body(t"(=⇿)")
      .assert(_ == t"""<mo stretchy="true">=</mo>""")

      test(m"a boolean false glyph"):
        body(t"(=↮)")
      .assert(_ == t"""<mo stretchy="false">=</mo>""")

      test(m"an enumerated value glyph"):
        body(t"(+⊰)")
      .assert(_ == t"""<mo form="prefix">+</mo>""")

      test(m"multiple directives juxtapose"):
        body(t"(=◆⇿)")
      .assert(_ == t"""<mo largeop="true" stretchy="true">=</mo>""")

      test(m"a fixed directive"):
        body(t"(x⦱)")
      .assert(_ == t"""<mi mathvariant="normal">x</mi>""")

      test(m"a directive binds to a grouped unit"):
        body(t"((a/b)═(0))")
      .assert(_ == t"""<mfrac linethickness="0"><mi>a</mi><mi>b</mi></mfrac>""")

      test(m"a boolean directive does not swallow a following group"):
        body(t"(=◆(a))")
      .assert(_ == t"""<mo largeop="true">=</mo><mi>a</mi>""")

    suite(m"Serialising MathML to ergo"):
      def rt(ergo: Text): Text = Ergo.serialize(Ergo.parse(ergo))

      test(m"a superscript"):
        rt(t"(x↗2)")
      .assert(_ == t"(x↗2)")

      test(m"a fraction"):
        rt(t"(a/b)")
      .assert(_ == t"(a/b)")

      test(m"insignificant spaces are dropped"):
        rt(t"(x↗(y + 1))")
      .assert(_ == t"(x↗(y+1))")

      test(m"a colour directive"):
        rt(t"(x●(red))")
      .assert(_ == t"(x●(red))")

      test(m"a boolean directive"):
        rt(t"(=⇿)")
      .assert(_ == t"(=⇿)")

      test(m"an enumerated directive value round-trips to its glyph"):
        rt(t"(+⊰)")
      .assert(_ == t"(+⊰)")

      test(m"an accent omits the implied attribute"):
        rt(t"(x↑^)")
      .assert(_ == t"(x↑^)")

      test(m"a matrix"):
        rt(t"(⋱(((1)(2))((3)(4))))")
      .assert(_ == t"(⋱(((1)(2))((3)(4))))")

      test(m"a row vector"):
        rt(t"(⋯((1)(2)(3)))")
      .assert(_ == t"(⋯((1)(2)(3)))")

      test(m"the quadratic formula round-trips to an equal tree"):
        val quadratic = t"(x = (-b ± √(b↗2 - 4 a c))/(2 a))"
        Ergo.parse(rt(quadratic))
      .assert(_ == Ergo.parse(t"(x = (-b ± √(b↗2 - 4 a c))/(2 a))"))

      test(m"an element outside the ergo subset is rejected"):
        capture[ErgoError](Ergo.serialize(Math(List(Mtext(t"hi"))))).reason
      .assert(_ == ErgoError.Reason.Unsupported(t"mtext"))

    suite(m"The ergo interpolator"):
      test(m"a literal is parsed at compile time to a Math value"):
        ergo"(x↗(y + 1))"
      .assert(_ == Ergo.parse(t"(x↗(y + 1))"))

      test(m"an interpolated matrix renders"):
        ergo"(⋱(((1)(2))((3)(4))))".xml.show.contains(t"<mtable>")
      .assert(_ == true)

    suite(m"Interpolator substitutions"):
      test(m"an integer substitution is a single atom"):
        val exponent = 2
        ergo"(x↗$exponent)".xml.show
      .assert(_ == t"""<math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>x</mi>"""
          + t"<mn>2</mn></msup></math>")

      test(m"two substitutions in sequence"):
        val left = 1
        val right = 2
        ergo"($left + $right)".xml.show.contains(t"<mn>1</mn><mo>+</mo><mn>2</mn>")
      .assert(_ == true)

      test(m"a directive binds to a substituted atom"):
        val value = 3
        ergo"($value●(red))".xml.show.contains(t"""<mn mathcolor="red">3</mn>""")
      .assert(_ == true)

      test(m"an Encodable value is embedded as an atom"):
        val vector = Vector(1, 2, 3)
        ergo"($vector)".xml.show.contains(t"<mtable>")
      .assert(_ == true)

    suite(m"Encoding values as MathML"):
      test(m"an integer becomes a math root"):
        42.math.xml.show
      .assert(_ == t"""<math xmlns="http://www.w3.org/1998/Math/MathML"><mn>42</mn></math>""")

      test(m"a double becomes mn"):
        3.14.mathml.xml.show
      .assert(_ == t"<mn>3.14</mn>")

      test(m"text becomes mtext"):
        t"speed".mathml.xml.show
      .assert(_ == t"<mtext>speed</mtext>")

      test(m"a complex number"):
        Complex(3, 4).mathml.xml.show
      .assert(_ == t"<mrow><mn>3</mn><mo>+</mo><mn>4</mn><mi>i</mi></mrow>")

      test(m"a quantity renders its units"):
        (5*Metre/Second).mathml.xml.show
      .assert: result =>
        result.contains(t"<mn>5.0</mn>") && result.contains(t"<mi>m</mi>")
            && result.contains(t"<msup><mi>s</mi><mn>-1</mn></msup>")

      test(m"a vector is a bracketed column"):
        Vector(1, 2, 3).mathml.xml.show
      .assert: result =>
        result.contains(t"<mtable>") && result.contains(t"<mtr><mtd><mn>1</mn></mtd></mtr>")

      test(m"a matrix is a bracketed table"):
        Matrix[2, 2]((1, 2), (3, 4)).mathml.xml.show
      .assert: result =>
        result.contains(t"<mtable>")
            && result.contains(t"<mtr><mtd><mn>1</mn></mtd><mtd><mn>2</mn></mtd></mtr>")

    suite(m"Rendering to the terminal"):
      test(m"a superscript stacks the exponent above the base"):
        Msup(Mi(t"x"), Mn(t"2")).draw
      .assert(_ == t" 2\nx ")

      test(m"a subscript stacks the index below the base"):
        Msub(Mi(t"x"), Mn(t"i")).draw
      .assert(_ == t"x \n i")

      test(m"a subsup places both scripts around the base"):
        Msubsup(Mi(t"x"), Mn(t"i"), Mn(t"2")).draw
      .assert(_ == t" 2\nx \n i")

      test(m"a fraction stacks over a rule"):
        Mfrac(Mn(t"1"), Mn(t"2")).draw
      .assert(_ == t" 1 \n───\n 2 ")

      test(m"binary operators are spaced in a row"):
        Mrow(Mi(t"x"), Mo(t"+"), Mn(t"1")).draw
      .assert(_ == t"x + 1")

      test(m"a square root draws a vinculum over the radicand"):
        Msqrt(Mrow(Mi(t"x"), Mo(t"+"), Mn(t"1"))).draw
      .assert(_ == t"┌─────\n√x + 1")

      test(m"a tall square root draws a foot and stem up to the corner"):
        Msqrt(Mfrac(Mn(t"1"), Mn(t"2"))).draw
      .assert(_ == t" ┌───\n │ 1 \n │───\n╲│ 2 ")

      test(m"an nth root writes its index one line above the sign"):
        Mroot(Mn(t"8"), Mn(t"3")).draw
      .assert(_ == t"3 ┌─\n ╲│8")

      test(m"an integral sign stretches to the height of its operand"):
        Mrow(Mo(t"∫"), Mfrac(Mn(t"1"), Mn(t"2"))).draw
      .assert(_ == t"╭  1 \n│ ───\n╯  2 ")

      test(m"a contour integral overlays a circle on the axis"):
        Mrow(Mo(t"∮"), Mfrac(Mn(t"1"), Mn(t"2"))).draw
      .assert(_ == t"╭  1 \n○ ───\n╯  2 ")

      test(m"a double integral repeats the stroke"):
        Mrow(Mo(t"∬"), Mfrac(Mn(t"1"), Mn(t"2"))).draw
      .assert(_ == t"╭╭  1 \n││ ───\n╯╯  2 ")

      test(m"a one-line summand keeps the plain sigma glyph"):
        Mrow(Mo(t"∑"), Mi(t"x")).draw
      .assert(_ == t"∑x")

      test(m"a one-line factor keeps the plain pi glyph"):
        Mrow(Mo(t"∏"), Mi(t"x")).draw
      .assert(_ == t"∏x")

      test(m"a big sigma stretches to a taller summand"):
        Mrow(Mo(t"∑"), Mfrac(Mn(t"1"), Mi(t"n"))).draw
      .assert(_ == t"▁▁▁   \n╲   1 \n╱  ───\n▔▔▔ n ")

      test(m"a big sigma only takes even heights, rounding up"):
        Mrow(Mo(t"∑"), Msup(Mi(t"x"), Mn(t"2"))).draw
      .assert(_ == t"▁▁▁  \n╲   2\n╱  x \n▔▔▔  ")

      test(m"a big pi adapts to any height"):
        Mrow(Mo(t"∏"), Mfrac(Mn(t"1"), Mi(t"n"))).draw
      .assert(_ == t"┬──┬ 1 \n│  │───\n│  │ n ")

      test(m"a summation carries its limits above and below"):
        Munderover(Mo(t"∑"), Mrow(Mi(t"i"), Mo(t"="), Mn(t"0")), Mi(t"n")).draw
      .assert(_ == t"  n  \n  ∑  \ni = 0")

      test(m"parentheses stretch around a taller cell"):
        Mfenced(Mfrac(Mn(t"1"), Mn(t"2"))).draw
      .assert(_ == t"⎛ 1 ⎞\n⎜───⎟\n⎝ 2 ⎠")

      test(m"the fraction bar aligns with the surrounding baseline"):
        Mrow(Mi(t"y"), Mo(t"="), Mfrac(Mn(t"1"), Mn(t"2"))).draw
      .assert(_ == t"     1 \ny = ───\n     2 ")
