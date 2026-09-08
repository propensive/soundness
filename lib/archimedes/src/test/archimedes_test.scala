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
package archimedes

import soundness.*
import Mathml.*

import errorDiagnostics.stackTracesDiagnostics
import strategies.throwUnsafely
import xylophone.XmlSchema

object Tests extends Suite(m"Archimedes tests"):
  def run(): Unit =
    given XmlSchema = XmlSchema.Freeform

    suite(m"Rendering to XML"):
      test(m"Render a superscript"):
        Msup(Mi("x"), Mn("2")).xml.show
      .assert(_ == "<msup><mi>x</mi><mn>2</mn></msup>")

      test(m"Render a fraction"):
        Mfrac(Mn("1"), Mn("2")).xml.show
      .assert(_ == "<mfrac><mn>1</mn><mn>2</mn></mfrac>")

      test(m"Render a rational as a fraction"):
        Q64(-3, 4).math.xml.show
      .assert(_ == """<math xmlns="http://www.w3.org/1998/Math/MathML"><mrow><mo>−</mo>"""
          + "<mfrac><mn>3</mn><mn>4</mn></mfrac></mrow></math>")

      test(m"Render a whole rational as a number"):
        Q32(7).math.xml.show
      .assert(_ == """<math xmlns="http://www.w3.org/1998/Math/MathML"><mn>7</mn></math>""")

      test(m"Render a token with an attribute"):
        Mi("x", List(t"mathvariant" -> t"italic")).xml.show
      .assert(_ == """<mi mathvariant="italic">x</mi>""")

      test(m"Render an empty mspace"):
        Mspace(List(t"width" -> t"1em")).xml.show
      .assert(_ == """<mspace width="1em"/>""")

      test(m"Render the root math element with namespace"):
        Math(Msup(Mi("x"), Mn("2"))).xml.show
      .assert(_ == """<math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>x</mi><mn>2"""
          + "</mn></msup></math>")

      test(m"Render the display attribute"):
        Math(List(Mn(t"1")), Display.Block).xml.show
      .assert(_.contains("""display="block""""))

    suite(m"Parsing from XML"):
      val nested = Math(Mrow(Mi("a"), Mo("+"), Mfrac(Mn("1"), Mi("b"))))
      val stretchy = Math(Mo("=", List(t"stretchy" -> t"false")))

      test(m"Parse a superscript from MathML text"):
        val source = """<math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>x</mi>"""
            + "<mn>2</mn></msup></math>"

        source.read[Math].contents
      .assert(_ == List(Msup(Mi(t"x"), Mn(t"2"))))

      test(m"Round-trip a nested expression"):
        nested.xml.show.read[Math].xml.show
      .assert(_ == nested.xml.show)

      test(m"Preserve unknown attributes on round-trip"):
        stretchy.xml.show.read[Math]
      .assert(_ == stretchy)

      test(m"Reject a non-math root element"):
        capture[Mathml.Error]("<svg><rect/></svg>".read[Math]).reason
      .assert(_ == Mathml.Error.Reason.NotMathml("svg"))

    suite(m"Embedding in and extracting from HTML"):
      val expression = Math(Mrow(Mi("a"), Mo("+"), Mn("1")))

      test(m"Render MathML as an HTML foreign element"):
        Math(Msup(Mi("x"), Mn("2"))).html.show
      .assert(_.contains("<math"))

      test(m"Extract MathML back out of the HTML tree"):
        Mathml.Reader.read(expression.html)
      .assert(_ == expression)

    def body(ergo: Text): Text =
      Ergo.parse(ergo).xml.show.sub("""<math xmlns="http://www.w3.org/1998/Math/MathML">""", "")
      . sub("</math>", "")

    suite(m"Scripts and fractions"):
      test(m"superscript"):
        body("(x↗y)")
      .assert(_ == "<msup><mi>x</mi><mi>y</mi></msup>")

      test(m"superscript of a group"):
        body("(x↗(y + 1))")
      .assert(_ == "<msup><mi>x</mi><mrow><mi>y</mi><mo>+</mo><mn>1</mn></mrow></msup>")

      test(m"combined sub and superscript merge into msubsup"):
        body("(x↘i↗2)")
      .assert(_ == "<msubsup><mi>x</mi><mi>i</mi><mn>2</mn></msubsup>")

      test(m"fraction binds looser than scripts"):
        body("(a/b↗2)")
      .assert(_ == "<mfrac><mi>a</mi><msup><mi>b</mi><mn>2</mn></msup></mfrac>")

      test(m"multi-digit number is one mn"):
        body("(x↗123)")
      .assert(_ == "<msup><mi>x</mi><mn>123</mn></msup>")

      test(m"multi-letter run is one identifier"):
        body("(sin)")
      .assert(_ == "<mi>sin</mi>")

    suite(m"Roots"):
      test(m"square root"):
        body("(√x)")
      .assert(_ == "<msqrt><mi>x</mi></msqrt>")

      test(m"nth root from an adjacent index"):
        body("(3√x)")
      .assert(_ == "<mroot><mi>x</mi><mn>3</mn></mroot>")

    suite(m"Big operators via under/over"):
      test(m"sum with limits"):
        body("(∑↓(i = 1)↑n x)")
      .assert: result =>
        result == "<munderover><mo>∑</mo><mrow><mi>i</mi><mo>=</mo><mn>1</mn></mrow>"
            + "<mi>n</mi></munderover><mi>x</mi>"

    suite(m"Accents"):
      test(m"an mo over a base is an accent"):
        body("(x↑^)")
      .assert(_ == """<mover accent="true"><mi>x</mi><mo>^</mo></mover>""")

      test(m"an identifier over a base is an ordinary limit"):
        body("(x↑n)")
      .assert(_ == "<mover><mi>x</mi><mi>n</mi></mover>")

    suite(m"Vectors and matrices (self-delimiting)"):
      test(m"row vector"):
        body("(⋯((1)(2)(3)))")
      .assert(_ == "<mtable><mtr><mtd><mn>1</mn></mtd><mtd><mn>2</mn></mtd>"
          + "<mtd><mn>3</mn></mtd></mtr></mtable>")

      test(m"column vector"):
        body("(⋮((a)(b)))")
      .assert(_ == "<mtable><mtr><mtd><mi>a</mi></mtd></mtr><mtr><mtd><mi>b</mi></mtd></mtr>"
          + "</mtable>")

      test(m"2x2 matrix"):
        body("(⋱(((1)(2))((3)(4))))")
      .assert(_ == "<mtable><mtr><mtd><mn>1</mn></mtd><mtd><mn>2</mn></mtd></mtr><mtr>"
          + "<mtd><mn>3</mn></mtd><mtd><mn>4</mn></mtd></mtr></mtable>")

      test(m"a single-cell row keeps a multi-token expression together"):
        body("(⋱(((a b))((c))))")
      .assert(_ == "<mtable><mtr><mtd><mrow><mi>a</mi><mi>b</mi></mrow></mtd></mtr><mtr>"
          + "<mtd><mi>c</mi></mtd></mtr></mtable>")

    suite(m"A whole formula"):
      test(m"the quadratic formula"):
        body("(x = (-b ± √(b↗2 - 4 a c))/(2 a))")
      .assert: result =>
        result == "<mi>x</mi><mo>=</mo><mfrac><mrow><mo>-</mo><mi>b</mi><mo>±</mo><msqrt>"
            + "<mrow><msup><mi>b</mi><mn>2</mn></msup><mo>-</mo><mn>4</mn><mi>a</mi><mi>c</mi>"
            + "</mrow></msqrt></mrow><mrow><mn>2</mn><mi>a</mi></mrow></mfrac>"

    suite(m"Escaping"):
      test(m"a lone operator glyph is a literal mo"):
        body("((↗))")
      .assert(_ == "<mo>↗</mo>")

    suite(m"Attribute directives"):
      test(m"a parameterised colour directive"):
        body("(x●(red))")
      .assert(_ == """<mi mathcolor="red">x</mi>""")

      test(m"a boolean true glyph"):
        body("(=⇿)")
      .assert(_ == """<mo stretchy="true">=</mo>""")

      test(m"a boolean false glyph"):
        body("(=↮)")
      .assert(_ == """<mo stretchy="false">=</mo>""")

      test(m"an enumerated value glyph"):
        body("(+⊰)")
      .assert(_ == """<mo form="prefix">+</mo>""")

      test(m"multiple directives juxtapose"):
        body("(=◆⇿)")
      .assert(_ == """<mo largeop="true" stretchy="true">=</mo>""")

      test(m"a fixed directive"):
        body("(x⦱)")
      .assert(_ == """<mi mathvariant="normal">x</mi>""")

      test(m"a directive binds to a grouped unit"):
        body("((a/b)═(0))")
      .assert(_ == """<mfrac linethickness="0"><mi>a</mi><mi>b</mi></mfrac>""")

      test(m"a boolean directive does not swallow a following group"):
        body("(=◆(a))")
      .assert(_ == """<mo largeop="true">=</mo><mi>a</mi>""")

    suite(m"Serialising MathML to ergo"):
      def rt(ergo: Text): Text = Ergo.serialize(Ergo.parse(ergo))

      test(m"a superscript"):
        rt("(x↗2)")
      .assert(_ == "(x↗2)")

      test(m"a fraction"):
        rt("(a/b)")
      .assert(_ == "(a/b)")

      test(m"insignificant spaces are dropped"):
        rt("(x↗(y + 1))")
      .assert(_ == "(x↗(y+1))")

      test(m"a colour directive"):
        rt("(x●(red))")
      .assert(_ == "(x●(red))")

      test(m"a boolean directive"):
        rt("(=⇿)")
      .assert(_ == "(=⇿)")

      test(m"an enumerated directive value round-trips to its glyph"):
        rt("(+⊰)")
      .assert(_ == "(+⊰)")

      test(m"an accent omits the implied attribute"):
        rt("(x↑^)")
      .assert(_ == "(x↑^)")

      test(m"a matrix"):
        rt("(⋱(((1)(2))((3)(4))))")
      .assert(_ == "(⋱(((1)(2))((3)(4))))")

      test(m"a row vector"):
        rt("(⋯((1)(2)(3)))")
      .assert(_ == "(⋯((1)(2)(3)))")

      test(m"the quadratic formula round-trips to an equal tree"):
        val quadratic = "(x = (-b ± √(b↗2 - 4 a c))/(2 a))"
        Ergo.parse(rt(quadratic))
      .assert(_ == Ergo.parse("(x = (-b ± √(b↗2 - 4 a c))/(2 a))"))

      test(m"an element outside the ergo subset is rejected"):
        capture[Ergo.Error](Ergo.serialize(Math(List(Mtext(t"hi"))))).reason
      .assert(_ == Ergo.Error.Reason.Unsupported("mtext"))

    suite(m"The ergo interpolator"):
      test(m"a literal is parsed at compile time to a Math value"):
        ergo"(x↗(y + 1))"
      .assert(_ == Ergo.parse("(x↗(y + 1))"))

      test(m"an interpolated matrix renders"):
        ergo"(⋱(((1)(2))((3)(4))))".xml.show.contains("<mtable>")
      .assert(_ == true)

    suite(m"Interpolator substitutions"):
      test(m"an integer substitution is a single atom"):
        val exponent = 2
        ergo"(x↗$exponent)".xml.show
      .assert(_ == """<math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>x</mi>"""
          + "<mn>2</mn></msup></math>")

      test(m"two substitutions in sequence"):
        val left = 1
        val right = 2
        ergo"($left + $right)".xml.show.contains("<mn>1</mn><mo>+</mo><mn>2</mn>")
      .assert(_ == true)

      test(m"a directive binds to a substituted atom"):
        val value = 3
        ergo"($value●(red))".xml.show.contains("""<mn mathcolor="red">3</mn>""")
      .assert(_ == true)

      test(m"an Encodable value is embedded as an atom"):
        val vector = Vector(1, 2, 3)
        ergo"($vector)".xml.show.contains("<mtable>")
      .assert(_ == true)

    suite(m"Encoding values as MathML"):
      test(m"an integer becomes a math root"):
        42.math.xml.show
      .assert(_ == """<math xmlns="http://www.w3.org/1998/Math/MathML"><mn>42</mn></math>""")

      test(m"a double becomes mn"):
        3.14.mathml.xml.show
      .assert(_ == "<mn>3.14</mn>")

      test(m"text becomes mtext"):
        "speed".mathml.xml.show
      .assert(_ == "<mtext>speed</mtext>")

      test(m"a complex number"):
        Complex(3, 4).mathml.xml.show
      .assert(_ == "<mrow><mn>3</mn><mo>+</mo><mn>4</mn><mi>i</mi></mrow>")

      test(m"a quantity renders its units"):
        (5*Metre/Second).mathml.xml.show
      .assert: result =>
        result.contains("<mn>5.0</mn>") && result.contains("<mi>m</mi>")
            && result.contains("<msup><mi>s</mi><mn>-1</mn></msup>")

      test(m"a vector is a bracketed column"):
        Vector(1, 2, 3).mathml.xml.show
      .assert: result =>
        result.contains("<mtable>") && result.contains("<mtr><mtd><mn>1</mn></mtd></mtr>")

      test(m"a matrix is a bracketed table"):
        Matrix[2, 2]((1, 2), (3, 4)).mathml.xml.show
      .assert: result =>
        result.contains("<mtable>")
            && result.contains("<mtr><mtd><mn>1</mn></mtd><mtd><mn>2</mn></mtd></mtr>")

    suite(m"Rendering to the terminal"):
      test(m"a simple superscript is set inline with Unicode glyphs"):
        Msup(Mi("x"), Mn("2")).draw
      .assert(_ == "x²")

      test(m"a simple subscript is set inline with Unicode glyphs"):
        Msub(Mi("x"), Mn("i")).draw
      .assert(_ == "xᵢ")

      test(m"an inline subsup sets the subscript before the superscript"):
        Msubsup(Mi("x"), Mn("i"), Mn("2")).draw
      .assert(_ == "xᵢ²")

      test(m"a multi-token superscript inlines if every glyph exists"):
        Msup(Mi("e"), Mrow(Mo("−"), Mi("x"))).draw
      .assert(_ == "e⁻ˣ")

      test(m"a superscript with no glyph stacks above the base"):
        Msup(Mi("x"), Mi("ρ")).draw
      .assert(_ == " ρ\nx ")

      test(m"a subscript with no glyph stacks below the base"):
        Msub(Mi("x"), Mi("q")).draw
      .assert(_ == "x \n q")

      test(m"a structured superscript stacks above the base"):
        Msup(Mi("x"), Mfrac(Mn("1"), Mn("2"))).draw
      .assert(_ == "  1 \n ───\n  2 \nx   ")

      test(m"a superscript on a tall base stacks above it"):
        Msup(Mfenced(Mfrac(Mn("1"), Mn("2"))), Mn("2")).draw
      .assert(_ == "     2\n⎛ 1 ⎞ \n⎜───⎟ \n⎝ 2 ⎠ ")

      test(m"a subsup inlines only when both scripts have glyphs"):
        Msubsup(Mi("x"), Mn("i"), Mi("ρ")).draw
      .assert(_ == " ρ\nx \n i")

      test(m"a fraction stacks over a rule"):
        Mfrac(Mn("1"), Mn("2")).draw
      .assert(_ == " 1 \n───\n 2 ")

      test(m"binary operators are spaced in a row"):
        Mrow(Mi("x"), Mo("+"), Mn("1")).draw
      .assert(_ == "x + 1")

      test(m"a square root draws a vinculum over the radicand"):
        Msqrt(Mrow(Mi("x"), Mo("+"), Mn("1"))).draw
      .assert(_ == "┌─────\n√x + 1")

      test(m"a tall square root draws a foot and stem up to the corner"):
        Msqrt(Mfrac(Mn("1"), Mn("2"))).draw
      .assert(_ == " ┌───\n │ 1 \n │───\n╲│ 2 ")

      test(m"an nth root writes its index one line above the sign"):
        Mroot(Mn("8"), Mn("3")).draw
      .assert(_ == "3 ┌─\n ╲│8")

      test(m"an integral sign stretches to the height of its operand"):
        Mrow(Mo("∫"), Mfrac(Mn("1"), Mn("2"))).draw
      .assert(_ == "╭  1 \n│ ───\n╯  2 ")

      test(m"a contour integral overlays a circle on the axis"):
        Mrow(Mo("∮"), Mfrac(Mn("1"), Mn("2"))).draw
      .assert(_ == "╭  1 \n○ ───\n╯  2 ")

      test(m"a double integral repeats the stroke"):
        Mrow(Mo("∬"), Mfrac(Mn("1"), Mn("2"))).draw
      .assert(_ == "╭╭  1 \n││ ───\n╯╯  2 ")

      test(m"a one-line summand keeps the plain sigma glyph"):
        Mrow(Mo("∑"), Mi("x")).draw
      .assert(_ == "∑x")

      test(m"a one-line factor keeps the plain pi glyph"):
        Mrow(Mo("∏"), Mi("x")).draw
      .assert(_ == "∏x")

      test(m"a big sigma stretches to a taller summand"):
        Mrow(Mo("∑"), Mfrac(Mn("1"), Mi("n"))).draw
      .assert(_ == "▁▁▁   \n╲   1 \n╱  ───\n▔▔▔ n ")

      test(m"a big sigma only takes even heights, rounding up"):
        Mrow(Mo("∑"), Msup(Mi("x"), Mi("ρ"))).draw
      .assert(_ == "▁▁▁  \n╲   ρ\n╱  x \n▔▔▔  ")

      test(m"a big pi adapts to any height"):
        Mrow(Mo("∏"), Mfrac(Mn("1"), Mi("n"))).draw
      .assert(_ == "┬──┬ 1 \n│  │───\n│  │ n ")

      test(m"a summation carries its limits above and below"):
        Munderover(Mo("∑"), Mrow(Mi("i"), Mo("="), Mn("0")), Mi("n")).draw
      .assert(_ == "  n  \n  ∑  \ni = 0")

      test(m"parentheses stretch around a taller cell"):
        Mfenced(Mfrac(Mn("1"), Mn("2"))).draw
      .assert(_ == "⎛ 1 ⎞\n⎜───⎟\n⎝ 2 ⎠")

      test(m"the fraction bar aligns with the surrounding baseline"):
        Mrow(Mi("y"), Mo("="), Mfrac(Mn("1"), Mn("2"))).draw
      .assert(_ == "     1 \ny = ───\n     2 ")

    suite(m"Compile-time error positioning"):
      test(m"an unclosed group's focus falls on the last character"):
        demilitarize:
          ergo"(x + (y + 1)"
        . map(_.focus)
      . assert(_ == List(")"))

      test(m"a bad opener's focus is the first character"):
        demilitarize:
          ergo"x + y"
        . map(_.focus)
      . assert(_ == List("x"))

    suite(m"Native-rendering coverage"):
      test(m"archimedes' types inspect natively"):
        Inspectable.fallbacks
         ( Math(Mfrac(Mn("1"), Mn("2"))).inspect,
           Cell.of(Msup(Mi("x"), Mn("2"))).inspect )

      . assert(_ == Nil)

      test(m"a math value inspects as its node tree"):
        Math(Mn("2")).inspect
      . assert(_ == """Math(○ ⟨mn t"2"⟩)""")

      test(m"a cell inspects with its dimensions and lines"):
        Cell.line("xy").inspect
      . assert(_ == """Cell(2×1@0 t"xy")""")
