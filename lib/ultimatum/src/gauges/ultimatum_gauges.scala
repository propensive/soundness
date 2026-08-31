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
import rudiments.prim
import gossamer.*
import parasite.*
import symbolism.*
import turbulence.*
import vacuous.*

// The status types, whose opaque declarations live in `internal` so that their abstraction does not
// leak; see the note there.
export ultimatum.internal.{Countdown, Fraction}

// A leaf panel hosting a live gauge. The design is the `Gaugeable` given for `status` in scope
// here, so which one is used is decided by the caller's imports at this line, not by the driver.
// The gauge reports its design's intrinsic size on every solve, so it grows and shrinks with the
// layout and re-negotiates on each resize; a gauge whose design animates also drives the form's
// frame clock.
def gauge[status: Gaugeable as design]
  ( reading:   Reading[status],
    fraction:  Double        = 1.0,
    minWidth:  Int           = 0,
    maxWidth:  Optional[Int] = Unset,
    minHeight: Int           = 0,
    maxHeight: Optional[Int] = Unset )
:   Pane =

  val preferred = design.columns(reading())

  // An inelastic design (a spinner, a status glyph) is pinned to its own width, so it does not
  // stretch across the terminal; an elastic one takes whatever the solver gives it.
  val width: Optional[Int] = if design.elastic then maxWidth else preferred

  val sizing =
    Sizing(fraction, minWidth.max(design.minWidth(reading())), width, minHeight, maxHeight)

  Pane.Widget(sizing, Gaugeable.Fixture(reading))

// One frame of `status` as styled rows — the pure entry point, for a caller doing its own drawing.
def gaugeRows[status: Gaugeable as design]
  ( status: status, width: Int, tick: Tick = Tick.zero )
:   List[Teletype] =

  design.rows(status, tick, width)

// One frame as a single line, which is what a bar, a spinner or a counter renders to. A multi-row
// design yields only its first row here; use `gaugeRows` for those.
def gaugeLine[status: Gaugeable as design]
  ( status: status, width: Int, tick: Tick = Tick.zero )
:   Teletype =

  design.rows(status, tick, width).prim.or(Teletype(t" "*width.max(0)))

// Show a gauge at the cursor for the duration of `block`, then erase it. The animation runs in one
// background task that redraws in place at the design's own period; a design with no period is
// drawn once and then only when its `Reading` changes.
// This is the standalone path — no form, no layout — and it shares every design with the embedded
// one.
def whilst[status: Gaugeable as design, result]
  ( reading: Reading[status], width: Optional[Int] = Unset )
  ( block: => result )
  ( using Stdio, Monitor, Probate )
:   result =

  val inlay = Inlay(reading, width)
  inlay.start()
  try block finally inlay.finish()
