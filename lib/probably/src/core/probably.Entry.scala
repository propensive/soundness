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
package probably

import scala.collection.mutable as scm


import anticipation.*
import rudiments.*
import vacuous.*
import denominative.dysasymptotics.linearSize

object Entry:
  enum Kind:
    case Check, Bench, Stress, Profile

object Run:
  // Structure that a numeric metric cannot carry: a profile's hot frames, or preformatted
  // operation-size and operation-rate units from a benchmark.
  enum Payload:
    case Frames(hotspots: Hotspots)
    case Sizing(operationSize: Optional[Text], operationRate: Optional[Text])

// One execution of one cell: a verdict for unit tests (absent for measurements), a map of
// recorded metrics (whose insertion order is their presentation order), failure details,
// and any structural payload. In a capacity search, `sustained` marks the winning run.
case class Run
  ( verdict:   Optional[Verdict]       = Unset,
    metrics:   Ledger[Metric, Double]  = Ledger(),
    details:   List[Verdict.Detail]    = Nil,
    payload:   Optional[Run.Payload]   = Unset,
    sustained: Boolean                 = false )

// The accumulated runs at one coordinate of a test: repeated executions of the same cell
// gather here, generalizing repeated-verdict accumulation to every test kind.
final class Tally():
  private val mutex: Mutex = Mutex()
  private val runs0: scm.ArrayBuffer[Run] = scm.ArrayBuffer()

  def record(run: Run): Unit = mutex(runs0.append(run))
  def runs: List[Run] = mutex(runs0.to(List))

// One named test and its results: an ordered list of axes (which may grow during the run,
// as emergent axes acquire coordinates), and a sparse map of cells keyed by coordinates in
// axis space. A test with no axes has exactly one cell, at `Nil`; combinations of axis
// values without a cell are gaps, rendered as empty positions in a grid.
final class Entry(val id: Test.Id, val kind: Entry.Kind):
  private val mutex: Mutex = Mutex()
  @scala.caps.unsafe.untrackedCaptures
  private var axes0: List[Axis.Spec] = Nil
  @scala.caps.unsafe.untrackedCaptures
  private var ticks0: Map[Axis.Spec, List[Value]] = Map()
  @scala.caps.unsafe.untrackedCaptures
  private var cells0: Ledger[List[Value], Tally] = Ledger()

  @scala.caps.unsafe.untrackedCaptures
  var headline: Optional[Metric] = Unset
  @scala.caps.unsafe.untrackedCaptures
  var anchor: Optional[Anchor] = Unset

  def axes: List[Axis.Spec] = mutex(axes0)
  def cells: List[(List[Value], Tally)] = mutex(cells0.to[List])

  // Returns the cell at the given coordinates, creating it if absent. Appends any
  // not-yet-seen axes (emergent axes extend the axis list as their coordinates arrive) and
  // registers each axis's values in first-appearance order.
  def cell(coordinates: List[(Axis.Spec, Value)]): Tally = mutex:
    coordinates.each: (axis, value) =>
      if !axes0.has(axis) then axes0 = axes0 :+ axis
      val seen = ticks0(axis).or(Nil)
      if !seen.has(value) then ticks0 = ticks0.define(axis, seen :+ value)

    val address = coordinates.map(_(1))

    cells0(address).or:
      val tally = Tally()
      cells0 = cells0.define(address, tally)
      tally

  // The coordinate values of one axis in presentation order: first-appearance order for
  // discrete axes, numeric order for integral and decimal axes.
  def values(axis: Axis.Spec): List[Value] =
    val seen = mutex(ticks0(axis).or(Nil))

    axis.domain match
      case Axis.Domain.Discrete => seen
      case _                    => List.of(seen.stdlib.sortBy(_.numeric.or(0.0)))
