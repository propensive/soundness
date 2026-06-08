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
package ultimatum

import java.io as ji

import soundness.*

object Tests extends Suite(m"Ultimatum Tests"):
  def run(): Unit =
    suite(m"TerminalSurface"):
      // Capture everything a surface writes into an in-memory buffer.
      def captured(block: Stdio ?=> Unit): Text =
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basic)
        block
        String(bytes.toByteArray.nn, "UTF-8").tt

      test(m"move emits an absolute CSI cursor-position sequence"):
        captured: stdio ?=>
          TerminalSurface(80, 24).move(10.z, 5.z)
      . assert(_ == t"\e[6;11H")

      test(m"move then put places text at the position"):
        captured: stdio ?=>
          val surface = TerminalSurface(80, 24)
          surface.move(10.z, 5.z)
          surface.put(t"X")
      . assert(_ == t"\e[6;11HX")

      test(m"clear erases the whole display"):
        captured(TerminalSurface(80, 24).clear())
      . assert(_ == t"\e[2J")

      test(m"hiding the cursor emits the DECTCEM reset"):
        captured(TerminalSurface(80, 24).cursor(false))
      . assert(_ == t"\e[?25l")

    suite(m"FlowExtent"):
      // A standalone extent over a muted parent; mutation tests never flush, so
      // the parent surface is unused.
      def extent(width: Int, height: Int): FlowExtent =
        given Stdio = Stdio(null, null, null, termcapDefinitions.basic)
        FlowExtent(TerminalSurface(width, height), Rect(0, 0, width, height))

      test(m"text wraps at the rectangle's width"):
        val flow = extent(3, 2)
        flow.put(t"abcdef")
        flow.render
      . assert(_ == t"abc\ndef")

      test(m"a newline moves to the start of the next row"):
        val flow = extent(5, 3)
        flow.put(t"ab\ncd")
        flow.render
      . assert(_ == t"ab   \ncd   \n     ")

      test(m"the grid scrolls up when the last row overflows"):
        val flow = extent(3, 2)
        flow.put(t"abcdefghi")
        flow.render
      . assert(_ == t"def\nghi")

      test(m"clear blanks the whole grid"):
        val flow = extent(3, 2)
        flow.put(t"abcdef")
        flow.clear()
        flow.render
      . assert(_ == t"   \n   ")

      test(m"Out output through the extent (an Stdio) flows into the grid"):
        val flow = extent(5, 1)
        given Stdio = flow
        Out.print(t"hi")
        flow.render
      . assert(_ == t"hi   ")

      test(m"flush paints the grid onto the parent at the rect's offset"):
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basic)
        val flow = FlowExtent(TerminalSurface(80, 24), Rect(2, 1, 3, 1))
        flow.put(t"xy")
        flow.flush()
        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[2;3Hxy ")

    suite(m"Layout solver"):
      def cell(sizing: Sizing): Frame = Frame.Cell(sizing)
      def file(children: Frame*): Frame = Frame.Split(Sizing(), Axis.File, children.to(List))
      def rank(children: Frame*): Frame = Frame.Split(Sizing(), Axis.Rank, children.to(List))

      test(m"fractions divide the axis proportionally"):
        val frame = file(cell(Sizing(2.0)), cell(Sizing(3.0)), cell(Sizing(4.0)))
        frame.arrange(Rect(0, 0, 90, 10)).cells
      . assert(_ == List(Rect(0, 0, 20, 10), Rect(20, 0, 30, 10), Rect(50, 0, 40, 10)))

      test(m"largest-remainder rounding fills the axis exactly"):
        val frame = file(cell(Sizing(2.0)), cell(Sizing(3.0)), cell(Sizing(4.0)))
        frame.arrange(Rect(0, 0, 100, 1)).cells.map(_.width)
      . assert(_ == List(22, 33, 45))

      test(m"the rounded sizes always sum to the available space"):
        val frame = file(cell(Sizing(2.0)), cell(Sizing(3.0)), cell(Sizing(4.0)))
        frame.arrange(Rect(0, 0, 100, 1)).cells.map(_.width).foldLeft(0)(_ + _)
      . assert(_ == 100)

      test(m"a child whose minimum exceeds its share is fixed at the minimum"):
        val frame = file(cell(Sizing(1.0, minWidth = 8)), cell(Sizing(1.0)))
        frame.arrange(Rect(0, 0, 10, 1)).cells.map(_.width)
      . assert(_ == List(8, 2))

      test(m"a child whose maximum is below its share is capped at the maximum"):
        val frame = file(cell(Sizing(1.0, maxWidth = 3)), cell(Sizing(1.0)))
        frame.arrange(Rect(0, 0, 10, 1)).cells.map(_.width)
      . assert(_ == List(3, 7))

      test(m"a container's minimum is forced up to the sum of its children's"):
        val frame = file(cell(Sizing(1.0, minWidth = 5)), cell(Sizing(1.0, minWidth = 5)))
        frame.measure(Axis.File)
      . assert(_ == Limits(10, Unset))

      test(m"file children fill the cross axis (full height)"):
        val frame = file(cell(Sizing(1.0)), cell(Sizing(1.0)))
        frame.arrange(Rect(0, 0, 8, 4)).cells
      . assert(_ == List(Rect(0, 0, 4, 4), Rect(4, 0, 4, 4)))

      test(m"rank splits distribute height and fill width"):
        val frame = rank(cell(Sizing(1.0)), cell(Sizing(1.0)))
        frame.arrange(Rect(0, 0, 8, 4)).cells
      . assert(_ == List(Rect(0, 0, 8, 2), Rect(0, 2, 8, 2)))

      test(m"nested ranks within files place rectangles correctly"):
        val frame = file(cell(Sizing(1.0)), rank(cell(Sizing(1.0)), cell(Sizing(1.0))))
        frame.arrange(Rect(0, 0, 10, 4)).cells
      . assert(_ == List(Rect(0, 0, 5, 4), Rect(5, 0, 5, 2), Rect(5, 2, 5, 2)))

    suite(m"layout / panel DSL"):
      test(m"side-by-side panels paint at their own offsets, no bleed"):
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basic)

        paint(TerminalSurface(4, 1), fullScreen = true):
          file(panel()(Out.print(t"AA")), panel()(Out.print(t"BB")))

        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[1;1HAA\e[1;3HBB")

      test(m"a panel's output wraps and scrolls within its own rectangle, no bleed"):
        val bytes = ji.ByteArrayOutputStream()
        given Stdio = Stdio(ji.PrintStream(bytes, true), null, null, termcapDefinitions.basic)

        // "HELLO" in a 2x1 panel wraps and scrolls until only "O" remains; the
        // sibling panel's "X" is unaffected, so neither bleeds past column 2.
        paint(TerminalSurface(4, 1), fullScreen = true):
          file(panel()(Out.print(t"HELLO")), panel()(Out.print(t"X")))

        String(bytes.toByteArray.nn, "UTF-8").tt
      . assert(_ == t"\e[1;1HO \e[1;3HX ")

      test(m"a fixed-minimum panel squeezes its fractional sibling"):
        val frame = file(panel(minWidth = 8)(()), panel()(())).frame
        frame.arrange(Rect(0, 0, 10, 1)).cells.map(_.width)
      . assert(_ == List(8, 2))
