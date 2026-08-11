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
package ultimatum

import anticipation.*
import escapade.*
import vacuous.*

object Captioned:
  // Derived from whatever design the underlying status already has, rather than choosing one: the
  // caption's placement is the only style decision here, and that comes from `CaptionLayout`. So
  // this is structural, lives in the companion, and needs no import.
  given gaugeable: [status: Gaugeable as design]
  =>  ( layout: CaptionLayout, gauging: Gauging )
  =>  Captioned[status] is Gaugeable =

    new Gaugeable:
      type Self = Captioned[status]
      override def period: Optional[Int] = design.period
      override def minWidth(status: Self): Int = design.minWidth(status.status)

      override def columns(status: Self): Int =
        design.columns(status.status) + layout.gap + gauging.cells(status.caption)

      override def height(status: Self, width: Int): Int =
        design.height(status.status, width)

      def rows(status: Self, tick: Tick, width: Int): List[Teletype] =
        val gaugeWidth =
          layout.gaugeWidth(design.columns(status.status), status.caption, width, gauging)

        val drawn = design.rows(status.status, tick, gaugeWidth)

        // Only the first row carries the caption; a multi-row design keeps its shape below it.
        val captioned = drawn.stdlib.zipWithIndex.map: (row, index) =>
          if index > 0 then row else layout.compose(row, gaugeWidth, status.caption, width, gauging)

        List.of(captioned.toList)

// Any status with a label beside it: `⠹ resolving dependencies`, `████░░ copying`. Generic over
// what it labels, so one design serves every status type — and its given derives from the
// underlying design rather than choosing one, so it is structural, not a style, and needs no
// import.
case class Captioned[status](status: status, caption: Text)
