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
package savagery

import soundness.*

import autopsies.contrastExpectations
import errorDiagnostics.stackTraces
import iridescence.WebColors.{Red, Blue, Green, Black, White}
import strategies.throwUnsafely
import xylophone.XmlSchema

object Tests extends Suite(m"Savagery tests"):
  def run(): Unit =
    suite(m"Basic shapes"):
      test(m"Rectangle at origin"):
        Rectangle(0!0, 10, 5).xml.show
      .assert(_ == t"""<rect x="0.0" y="0.0" width="10.0" height="5.0"/>""")

      test(m"Rectangle offset"):
        Rectangle(2!3, 8, 4).xml.show
      .assert(_ == t"""<rect x="2.0" y="3.0" width="8.0" height="4.0"/>""")

      test(m"Rectangle with negative position"):
        Rectangle((-1f)!(-2f), 5, 5).xml.show
      .assert(_ == t"""<rect x="-1.0" y="-2.0" width="5.0" height="5.0"/>""")

      test(m"Circle at origin"):
        Circle(0!0, 5).xml.show
      .assert(_ == t"""<circle cx="0.0" cy="0.0" r="5.0"/>""")

      test(m"Circle at offset"):
        Circle(10!20, 3).xml.show
      .assert(_ == t"""<circle cx="10.0" cy="20.0" r="3.0"/>""")

      test(m"Ellipse with different radii"):
        Ellipse(1!2, 3, 4, Angle(0)).xml.show
      .assert(_ == t"""<ellipse cx="1.0" cy="2.0" rx="3.0" ry="4.0"/>""")

      test(m"Ellipse with equal radii renders as circle"):
        Ellipse(0!0, 5, 5, Angle(0)).xml.show
      .assert(_ == t"""<circle cx="0.0" cy="0.0" r="5.0"/>""")

    suite(m"Path output"):
      test(m"Empty path"):
        Outline().xml.show
      .assert(_ == t"""<path d=""/>""")

      test(m"Move and close"):
        Outline().moveTo(0!0).closed.xml.show
      .assert(_ == t"""<path d="M 0.0 0.0 Z"/>""")

      test(m"Move and absolute line"):
        Outline().moveTo(0!0).lineTo(3!4).xml.show
      .assert(_ == t"""<path d="M 0.0 0.0 L 3.0 4.0"/>""")

      test(m"Move and cubic curve"):
        Outline().moveTo(0!0).curveTo(1!1, 2!1, 3!0).xml.show
      .assert(_ == t"""<path d="M 0.0 0.0 C 1.0 1.0, 2.0 1.0, 3.0 0.0"/>""")

      test(m"Plus sign path"):
        Outline().moveTo(0!0).lineUp(2).lineLeft(2).lineUp(1).lineRight(2).lineUp(2).lineRight(1)
            .lineDown(2).lineRight(2).lineDown(1).lineLeft(2).lineDown(2).closed.xml.show
      .assert(_ == t"""<path d="M 0.0 0.0 h 2.0 v -2.0 h 1.0 v 2.0 h 2.0 v 1.0 h -2.0 v 2.0 h -1.0 v -2.0 h -2.0 Z"/>""")

    suite(m"Transform encoding"):
      test(m"Translate"):
        (Transform.Translate(Shift(10, 20)): Transform).encode
      .assert(_ == t"translate(10.0,20.0)")

      test(m"Translate with negative offset"):
        (Transform.Translate(Shift(-5, -10)): Transform).encode
      .assert(_ == t"translate(-5.0,-10.0)")

      test(m"Scale uniform"):
        (Transform.Scale(2.0f, Unset): Transform).encode
      .assert(_ == t"scale(2.0)")

      test(m"Scale non-uniform"):
        (Transform.Scale(2.0f, 3.0f): Transform).encode
      .assert(_ == t"scale(2.0,3.0)")

      test(m"Rotate"):
        (Transform.Rotate(Angle.degrees(45)): Transform).encode
      .assert(_ == t"rotate(45.0)")

      test(m"Rotate by 90 degrees"):
        (Transform.Rotate(Angle.degrees(90)): Transform).encode
      .assert(_ == t"rotate(90.0)")

      test(m"SkewX"):
        (Transform.SkewX(Angle.degrees(45)): Transform).encode
      .assert(_ == t"skewX(45.0)")

      test(m"SkewY"):
        (Transform.SkewY(Angle.degrees(45)): Transform).encode
      .assert(_ == t"skewY(45.0)")

      test(m"Matrix identity"):
        (Transform.Matrix(1.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f): Transform).encode
      .assert(_ == t"matrix(1.0,0.0,0.0,1.0,0.0,0.0)")

      test(m"Matrix translation"):
        (Transform.Matrix(1.0f, 0.0f, 0.0f, 1.0f, 5.0f, 10.0f): Transform).encode
      .assert(_ == t"matrix(1.0,0.0,0.0,1.0,5.0,10.0)")

    suite(m"Outline with attributes"):
      test(m"Outline with id"):
        Outline(id = SvgId(t"plus")).moveTo(0!0).closed.xml.show
      .assert(_ == t"""<path d="M 0.0 0.0 Z" id="plus"/>""")

      test(m"Outline with single transform"):
        Outline(transforms = List(Transform.Translate(Shift(10, 20))))
            .moveTo(0!0).closed.xml.show
      .assert(_ == t"""<path d="M 0.0 0.0 Z" transform="translate(10.0,20.0)"/>""")

      test(m"Outline with multiple transforms"):
        Outline
         (transforms =
            List(Transform.Translate(Shift(1, 2)), Transform.Rotate(Angle.degrees(45))))
        . moveTo(0!0).closed.xml.show
      .assert(_ == t"""<path d="M 0.0 0.0 Z" transform="translate(1.0,2.0) rotate(45.0)"/>""")

      test(m"Outline with id and transform"):
        Outline
         (id         = SvgId(t"shape1"),
          transforms = List(Transform.Translate(Shift(5, 5))))
        . moveTo(0!0).closed.xml.show
      .assert(_ == t"""<path d="M 0.0 0.0 Z" id="shape1" transform="translate(5.0,5.0)"/>""")

    suite(m"Gradient stops"):
      test(m"Stop with red at offset 0"):
        Stop(0.0, Red).xml.show
      .assert(_ == t"""<stop offset="0.0" stop-color="#ff0000"/>""")

      test(m"Stop with blue at offset 1"):
        Stop(1.0, Blue).xml.show
      .assert(_ == t"""<stop offset="1.0" stop-color="#0000ff"/>""")

      test(m"Stop with black at offset 0.5"):
        Stop(0.5, Black).xml.show
      .assert(_ == t"""<stop offset="0.5" stop-color="#000000"/>""")

      test(m"Stop with white at offset 0.25"):
        Stop(0.25, White).xml.show
      .assert(_ == t"""<stop offset="0.25" stop-color="#ffffff"/>""")

    suite(m"Linear gradient"):
      test(m"Single-stop gradient"):
        LinearGradient(SvgId(t"grad1"), Stop(0.0, Red)).xml.show
      .assert: result =>
          result == t"""<linearGradient id="grad1"><stop offset="0.0" stop-color="#ff0000"/></linearGradient>"""

      test(m"Two-stop gradient"):
        LinearGradient(SvgId(t"grad2"), Stop(0.0, Red), Stop(1.0, Blue)).xml.show
      .assert: result =>
          result == t"""<linearGradient id="grad2"><stop offset="0.0" stop-color="#ff0000"/><stop offset="1.0" stop-color="#0000ff"/></linearGradient>"""

      test(m"Three-stop gradient"):
        LinearGradient
         (SvgId(t"rainbow"),
          Stop(0.0, Red),
          Stop(0.5, Green),
          Stop(1.0, Blue))
        . xml.show
      .assert: result =>
          result == t"""<linearGradient id="rainbow"><stop offset="0.0" stop-color="#ff0000"/><stop offset="0.5" stop-color="#008000"/><stop offset="1.0" stop-color="#0000ff"/></linearGradient>"""

    suite(m"SVG document"):
      test(m"Empty SVG"):
        Svg(100, 100).xml.show
      .assert(_ == t"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100.0 100.0" width="100.0" height="100.0"/>""")

      test(m"SVG with single rectangle"):
        Svg(50, 50, figures = List(Rectangle(0!0, 10, 10))).xml.show
      .assert: result =>
          result == t"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 50.0 50.0" width="50.0" height="50.0"><rect x="0.0" y="0.0" width="10.0" height="10.0"/></svg>"""

      test(m"SVG with two figures"):
        Svg
         (100,
          100,
          figures = List(Rectangle(0!0, 10, 10), Circle(50!50, 5)))
        . xml.show
      .assert: result =>
          result == t"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100.0 100.0" width="100.0" height="100.0"><rect x="0.0" y="0.0" width="10.0" height="10.0"/><circle cx="50.0" cy="50.0" r="5.0"/></svg>"""

      test(m"SVG with defs"):
        Svg
         (100,
          100,
          defs = List(LinearGradient(SvgId(t"g1"), Stop(0.0, Red), Stop(1.0, Blue))))
        . xml.show
      .assert: result =>
          result == t"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100.0 100.0" width="100.0" height="100.0"><defs><linearGradient id="g1"><stop offset="0.0" stop-color="#ff0000"/><stop offset="1.0" stop-color="#0000ff"/></linearGradient></defs></svg>"""

      test(m"SVG with defs and figures"):
        Svg
         (100,
          100,
          defs    = List(LinearGradient(SvgId(t"g1"), Stop(0.0, Red), Stop(1.0, Blue))),
          figures = List(Rectangle(0!0, 50, 50)))
        . xml.show
      .assert: result =>
          result == t"""<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100.0 100.0" width="100.0" height="100.0"><defs><linearGradient id="g1"><stop offset="0.0" stop-color="#ff0000"/><stop offset="1.0" stop-color="#0000ff"/></linearGradient></defs><rect x="0.0" y="0.0" width="50.0" height="50.0"/></svg>"""

    suite(m"SVG document with header"):
      test(m"Document[Svg] with UTF-8 includes XML declaration"):
        Document(Svg(10, 10), enc"UTF-8").show
      .assert: result =>
          result == t"""<?xml version="1.0" encoding="UTF-8"?><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 10.0 10.0" width="10.0" height="10.0"/>"""

      test(m"Document[Svg] with content"):
        Document(Svg(50, 50, figures = List(Circle(25!25, 10))), enc"UTF-8").show
      .assert: result =>
          result == t"""<?xml version="1.0" encoding="UTF-8"?><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 50.0 50.0" width="50.0" height="50.0"><circle cx="25.0" cy="25.0" r="10.0"/></svg>"""

    suite(m"SVG parsing"):
      given XmlSchema = XmlSchema.Freeform

      test(m"Parse empty SVG"):
        val svg = t"""<svg width="100" height="100"/>""".read[Svg]
        (svg.width, svg.height, svg.figures.length, svg.defs.length)
      .assert(_ == (100.0f, 100.0f, 0, 0))

      test(m"Parse SVG with rectangle"):
        val svg =
          t"""<svg width="50" height="50"><rect x="0" y="0" width="10" height="10"/></svg>"""
        . read[Svg]

        svg.figures
      .assert(_ == List(Rectangle(Point(0, 0), 10, 10)))

      test(m"Parse SVG with circle"):
        val svg = t"""<svg width="100" height="100"><circle cx="10" cy="20" r="5"/></svg>"""
                . read[Svg]

        svg.figures
      .assert(_ == List(Ellipse(Point(10, 20), 5, 5, Angle(0))))

      test(m"Parse SVG with ellipse"):
        val svg =
          t"""<svg width="100" height="100"><ellipse cx="1" cy="2" rx="3" ry="4"/></svg>"""
        . read[Svg]

        svg.figures
      .assert(_ == List(Ellipse(Point(1, 2), 3, 4, Angle(0))))

      test(m"Parse SVG with simple path"):
        val svg = t"""<svg width="10" height="10"><path d="M 0 0 L 1 1 Z"/></svg>""".read[Svg]
        svg.figures.length
      .assert(_ == 1)

      test(m"Parse SVG with path and check ops"):
        val svg = t"""<svg width="10" height="10"><path d="M 0 0 L 1 1 Z"/></svg>""".read[Svg]
        svg.figures.head
      .assert:
          case Outline(ops, Unset, Unset, Nil) =>
            ops.reverse == List(Stroke.Move(Point(0, 0)), Stroke.Draw(Point(1, 1)), Stroke.Close)
          case _ => false

      test(m"Parse path with relative h and v"):
        val svg = t"""<svg width="10" height="10"><path d="M 0 0 h 2 v -2 Z"/></svg>""".read[Svg]
        svg.figures.head
      .assert:
          case Outline(ops, _, _, _) =>
            ops.reverse == List
             (Stroke.Move(Point(0, 0)),
              Stroke.Draw(Shift(2, 0)),
              Stroke.Draw(Shift(0, -2)),
              Stroke.Close)
          case _ => false

      test(m"Parse path with id"):
        val svg = t"""<svg width="10" height="10"><path id="x" d="M 0 0 Z"/></svg>""".read[Svg]
        svg.figures.head
      .assert:
          case Outline(_, _, id, _) => id == SvgId(t"x")
          case _                    => false

      test(m"Parse path with single transform"):
        val svg = t"""<svg width="10" height="10"><path d="M 0 0 Z" transform="translate(5,10)"/></svg>"""
                . read[Svg]

        svg.figures.head
      .assert:
          case Outline(_, _, _, transforms) =>
            transforms == List(Transform.Translate(Shift(5, 10)))
          case _ => false

      test(m"Parse path with multiple transforms"):
        val svg = t"""<svg width="10" height="10"><path d="M 0 0 Z" transform="translate(1,2) rotate(45)"/></svg>"""
                . read[Svg]

        svg.figures.head
      .assert:
          case Outline(_, _, _, transforms) =>
            transforms == List(Transform.Translate(Shift(1, 2)), Transform.Rotate(Angle.degrees(45)))
          case _ => false

      test(m"Parse rgb hex color #ff0000"):
        val svg =
          t"""<svg width="10" height="10"><defs><linearGradient id="g"><stop offset="0" stop-color="#ff0000"/></linearGradient></defs></svg>"""
        . read[Svg]

        svg.defs.head
      .assert:
          case lg: LinearGradient[?] =>
            lg.stops.length == 1 && lg.stops.head.color == Srgb(1.0, 0.0, 0.0)
          case _ => false

      test(m"Parse short hex #f00"):
        val svg =
          t"""<svg width="10" height="10"><defs><linearGradient id="g"><stop offset="0" stop-color="#f00"/></linearGradient></defs></svg>"""
        . read[Svg]

        svg.defs.head

      . assert(_.stops.head.color == Srgb(1.0, 0.0, 0.0))

      test(m"Parse rgb function"):
        val svg =
          t"""<svg width="10" height="10"><defs><linearGradient id="g"><stop offset="0.5" stop-color="rgb(255,0,0)"/></linearGradient></defs></svg>"""
        . read[Svg]

        svg.defs.head
      . assert(_.stops.head.color == Srgb(1.0, 0.0, 0.0))

      test(m"Parse named color red"):
        val svg =
          t"""<svg width="10" height="10"><defs><linearGradient id="g"><stop offset="0" stop-color="red"/></linearGradient></defs></svg>"""
        . read[Svg]

        svg.defs.head

      . assert(_.stops.head.color == Srgb(1.0, 0.0, 0.0))

      test(m"Parse linear gradient with id"):
        val svg =
          t"""<svg width="10" height="10"><defs><linearGradient id="myGrad"><stop offset="0" stop-color="#ff0000"/></linearGradient></defs></svg>"""

        . read[Svg]

        svg.defs.head
      .assert:
          case lg: LinearGradient[?] => lg.id == SvgId(t"myGrad")
          case _                     => false

      test(m"Skip unknown element"):
        val svg =
          t"""<svg width="10" height="10"><text x="0" y="0">Hello</text><rect x="0" y="0" width="5" height="5"/></svg>"""
        . read[Svg]

        svg.figures.length
      .assert(_ == 1)

      test(m"Flatten group"):
        val svg =
          t"""<svg width="10" height="10"><g><rect x="0" y="0" width="5" height="5"/><circle cx="0" cy="0" r="3"/></g></svg>"""
        . read[Svg]

        svg.figures.length
      .assert(_ == 2)

      test(m"Ignore unknown attributes"):
        val svg = t"""<svg width="10" height="10"><rect x="0" y="0" width="5" height="5" foo="bar"/></svg>"""
                . read[Svg]

        svg.figures.head
      .assert(_ == Rectangle(Point(0, 0), 5, 5))

      test(m"Default missing attributes to zero"):
        val svg = t"""<svg width="10" height="10"><rect width="5" height="5"/></svg>""".read[Svg]
        svg.figures.head
      .assert(_ == Rectangle(Point(0, 0), 5, 5))

      test(m"Round-trip: rectangle"):
        val encoded = Svg(100, 100, figures = List(Rectangle(2!3, 8, 4))).xml.show
        encoded.read[Svg].xml.show == encoded
      .assert(_ == true)

      test(m"Round-trip: circle"):
        val encoded = Svg(100, 100, figures = List(Circle(50!50, 10))).xml.show
        encoded.read[Svg].xml.show == encoded
      .assert(_ == true)

      test(m"Round-trip: ellipse"):
        val encoded = Svg(100, 100, figures = List(Ellipse(0!0, 5, 3, Angle(0)))).xml.show
        encoded.read[Svg].xml.show == encoded
      .assert(_ == true)

      test(m"Round-trip: path"):
        val encoded =
          Svg(100, 100, figures = List(Outline().moveTo(0!0).lineTo(1!1).closed)).xml.show

        encoded.read[Svg].xml.show == encoded
      .assert(_ == true)

      test(m"Round-trip: path with id and transform"):
        val encoded = Svg
         (100,
          100,
          figures = List
           (Outline
             (id         = SvgId(t"shape1"),
              transforms = List(Transform.Translate(Shift(5, 10))))
            . moveTo(0!0).closed))
        . xml.show

        encoded.read[Svg].xml.show == encoded
      .assert(_ == true)

      test(m"Round-trip: SVG with multiple figures"):
        val encoded = Svg
         (100, 100, figures = List(Rectangle(0!0, 10, 10), Circle(50!50, 5)))
        . xml.show

        encoded.read[Svg].xml.show == encoded
      .assert(_ == true)

      test(m"Round-trip: Document[Svg]"):
        val encoded =
          Document(Svg(50, 50, figures = List(Circle(25!25, 10))), enc"UTF-8").show

        encoded.load[Svg].show == encoded
      .assert(_ == true)

      test(m"load[Svg] preserves encoding metadata"):
        val original = Document(Svg(10, 10), enc"UTF-8")
        val parsed: Document[Svg] = original.show.load[Svg]
        parsed.metadata.name
      .assert(_ == t"UTF-8")

      test(m"Non-SVG root raises NotAnSvg"):
        capture[SvgError](t"""<html/>""".read[Svg])
      .assert:
          case SvgError(SvgError.Reason.NotAnSvg(_)) => true
          case _                                     => false
