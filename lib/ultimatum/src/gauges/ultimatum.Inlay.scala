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
import gossamer.*
import parasite.*
import quantitative.*
import symbolism.*
import turbulence.*
import vacuous.*

// A gauge drawn live at the cursor, with no form and no layout: the shape a command-line tool
// wants when it has one thing to report. Each frame is redrawn in place, the animation is one
// background task snoozing the design's own period, and `finish` erases the block.
// Every design works here unchanged, because a design is a pure function of status, tick and width
// — this class supplies the three and does the writing.
class Inlay[status: Gaugeable as design]
  ( reading: Reading[status], width: Optional[Int] = Unset )
  ( using stdio: Stdio, monitor: Monitor, probate: Probate ):

  @scala.caps.unsafe.untrackedCaptures
  private val started: Long = System.nanoTime

  // How many rows the last frame occupied, so the next one knows how far to move back up.
  @scala.caps.unsafe.untrackedCaptures
  private var drawn: Int = 0

  @scala.caps.unsafe.untrackedCaptures
  private var running: Boolean = false

  private def columns: Int = width.or(stdio.termcap.width.max(1))

  private def tick: Tick =
    Tick.at((System.nanoTime - started)/1000000L, design.period.or(1000))

  // Draw one frame over the last one. Each row is erased to the end of the line as it is written,
  // so a frame that is shorter than its predecessor leaves no residue.
  private def paint(): Unit =
    // The stdlib view is taken once, indexed and counted by the paint loop below.
    val rows = design.rows(reading(), tick, columns).stdlib
    rewind()
    var index = 0

    while index < rows.length do
      Out.print(e"${rows(index)}${csi.el()}")
      if index < rows.length - 1 then Out.print(t"\n")
      index += 1

    drawn = rows.length

  // Back to the first column of the block's first row.
  private def rewind(): Unit =
    if drawn > 1 then Out.print(csi.cuu(drawn - 1))
    Out.print(t"\r")

  // Paint once, and — if the design animates — keep painting until `finish`. A design with no
  // period is drawn once here and thereafter only when its `Reading` changes and calls back.
  def start(): Unit =
    reading.bindWake: () => paint()
    paint()

    design.period.let: period =>
      running = true

      // The repaint task captures this `Inlay`, which also owns the `Monitor` the task is spawned
      // against, and separation checking rejects the overlap. They are the same single-owner
      // session: the task is started here, stopped by `finish`, and touches nothing else — so the
      // assertion is local and its scope is the object's own lifetime. (`form` makes the same
      // assertion, for the same reason, around the alternate-screen session.)
      scala.caps.unsafe.unsafeAssumeSeparate:
        async:
          while running do
            snooze(period.toDouble*Milli(Second))
            if running then paint()

        ()

  // Erase the block and leave the cursor where it started, so whatever the caller prints next
  // begins on a clean line.
  def finish(): Unit =
    running = false
    reading.bindWake: () => ()
    rewind()
    var index = 0

    while index < drawn do
      Out.print(csi.el())
      if index < drawn - 1 then Out.print(t"\n")
      index += 1

    if drawn > 1 then Out.print(csi.cuu(drawn - 1))
    Out.print(t"\r")
    drawn = 0
