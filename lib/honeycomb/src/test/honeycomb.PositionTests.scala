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
package honeycomb

import soundness.*

import htmlDoms.whatwg
import strategies.throwUnsafely
import parsing.trackPositions

object PositionTests extends Suite(m"Honeycomb position-index tests"):

  private def line(p: Optional[Html.Position]): Optional[Int] = p.let(_.line.n1)
  private def col(p: Optional[Html.Position]): Optional[Int] = p.let(_.column.n1)
  private def len(p: Optional[Html.Position]): Optional[Int] = p.let(_.length)

  private def html: Html.Path = Html.Path().element(t"html")
  private def body: Html.Path = html.element(t"body")

  def run(): Unit =
    // `import parsing.trackPositions` (above) turns on position tracking, so a plain
    // `.load[Html]` records source positions on the `Document[Html]`; `document.locate(path)`
    // then resolves an `Html.Path` to its `Position`.
    def trackedDoc(source: Text): Document[Html] = source.load[Html]

    suite(m"Root element"):
      test(m"Root element line"):
        line(trackedDoc(t"<html></html>").locate(html))
      . assert(_ == 1)

      test(m"Root element column"):
        col(trackedDoc(t"<html></html>").locate(html))
      . assert(_ == 1)

      test(m"Root element source length spans the open / close"):
        len(trackedDoc(t"<html></html>").locate(html))
      . assert(_ == 13)

      test(m"A doctype shifts the root's column"):
        col(trackedDoc(t"<!DOCTYPE html><html></html>").locate(html))
      . assert(_ == 16)

    suite(m"Nested elements"):
      val source = t"<html><head></head><body><p>Hi</p></body></html>"

      test(m"Nested child column"):
        col(trackedDoc(source).locate(body.element(t"p")))
      . assert(_ == 26)

      test(m"Nested child length"):
        len(trackedDoc(source).locate(body.element(t"p")))
      . assert(_ == 9)

      test(m"Second child with the same label uses ordinal 2"):
        col(trackedDoc(t"<html><body><p>a</p><p>b</p></body></html>").locate(body.element(t"p", 2)))
      . assert(_ == 21)

      test(m"A void element is located"):
        col(trackedDoc(t"<html><body><p>a</p><br></body></html>").locate(body.element(t"br")))
      . assert(_ == 21)

      test(m"Text and comments do not count towards ordinals"):
        col(trackedDoc(t"<html><body>hi<!-- x --><p>a</p></body></html>").locate(body.element(t"p")))
      . assert(_ == 25)

      test(m"Missing child returns Unset"):
        trackedDoc(source).locate(body.element(t"missing"))
      . assert(_ == Unset)

      test(m"A wrong root label returns Unset"):
        trackedDoc(source).locate(Html.Path().element(t"body"))
      . assert(_ == Unset)

    suite(m"Attributes"):
      val source = t"""<html lang="en"><body class="x" id='y'></body></html>"""

      test(m"Root attribute column"):
        col(trackedDoc(source).locate(html.attribute(t"lang")))
      . assert(_ == 7)

      test(m"Attribute length spans name=\"value\""):
        len(trackedDoc(source).locate(html.attribute(t"lang")))
      . assert(_ == 9)

      test(m"Nested attribute column"):
        col(trackedDoc(source).locate(body.attribute(t"class")))
      . assert(_ == 23)

      test(m"Second attribute column"):
        col(trackedDoc(source).locate(body.attribute(t"id")))
      . assert(_ == 33)

      test(m"Missing attribute returns Unset"):
        trackedDoc(source).locate(body.attribute(t"missing"))
      . assert(_ == Unset)

    suite(m"Multi-line input"):
      val source = t"<html>\n  <body>\n    <p>Hi</p>\n  </body>\n</html>"

      test(m"Third-line child is on line 3"):
        line(trackedDoc(source).locate(body.element(t"p")))
      . assert(_ == 3)

      test(m"Third-line child column reflects indent"):
        col(trackedDoc(source).locate(body.element(t"p")))
      . assert(_ == 5)

      test(m"Root length spans every line"):
        len(trackedDoc(source).locate(html))
      . assert(_ == source.length)

    suite(m"Non-ASCII content"):
      val source = t"<html><body><p>é😀</p><p>x</p></body></html>"

      test(m"Columns count code points, not chars"):
        col(trackedDoc(source).locate(body.element(t"p", 2)))
      . assert(_ == 22)

      test(m"Lengths count chars"):
        len(trackedDoc(source).locate(body.element(t"p")))
      . assert(_ == 10)

      test(m"A newline after non-ASCII content resets the column"):
        col(trackedDoc(t"<html><body><p>😀</p>\n <p>x</p></body></html>").locate(body.element(t"p", 2)))
      . assert(_ == 2)

    suite(m"Inferred and recovered elements"):
      test(m"An inferred root starts at the token that produced it"):
        val document = trackedDoc(t"<p>Hi</p>")
        (col(document.locate(html)), len(document.locate(html)))
      . assert(_ == (1, 9))

      test(m"A child of inferred ancestors is located"):
        col(trackedDoc(t"<p>Hi</p>").locate(body.element(t"p")))
      . assert(_ == 1)

      test(m"An inferred ancestor carries no attribute positions"):
        trackedDoc(t"""<p class="x">Hi</p>""").locate(body.attribute(t"class"))
      . assert(_ == Unset)

      test(m"A foster-parented element keeps its own position"):
        val source = t"<html><body><table><p>f</p><tr><td>a</td></tr></table></body></html>"
        col(trackedDoc(source).locate(body.element(t"p")))
      . assert(_ == 20)

      test(m"The table a child was fostered out of keeps its position"):
        val source = t"<html><body><table><p>f</p><tr><td>a</td></tr></table></body></html>"
        col(trackedDoc(source).locate(body.element(t"table")))
      . assert(_ == 13)

      test(m"An element closed by a parent's end tag ends at that tag"):
        val document = trackedDoc(t"<html><body><p>a<p>b</body></html>")
        (col(document.locate(body.element(t"p"))), len(document.locate(body.element(t"p"))))
      . assert(_ == (13, 4))

      test(m"A permissive load is tracked too"):
        import recoveries.permissiveRecovery
        col(t"<html><body><p>a</b></p></body></html>".load[Html].locate(body.element(t"p")))
      . assert(_ == 13)

    suite(m"Tracking off"):
      test(m"Without tracking, the document has no index"):
        given PositionTracking = PositionTracking.Off
        t"<html></html>".load[Html].metadata.positionIndex
      . assert(_ == Unset)

      test(m"Without tracking, locate returns Unset"):
        given PositionTracking = PositionTracking.Off
        t"<html></html>".load[Html].locate(html)
      . assert(_ == Unset)

      test(m"A tracked and an untracked load compare equal"):
        val untracked = locally:
          given PositionTracking = PositionTracking.Off
          t"<html></html>".load[Html]

        t"<html></html>".load[Html] == untracked
      . assert(_ == true)
