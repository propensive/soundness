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
    metrics:   ListMap[Metric, Double] = ListMap(),
    details:   List[Verdict.Detail]    = Nil,
    payload:   Optional[Run.Payload]   = Unset,
    sustained: Boolean                 = false )

// The accumulated runs at one coordinate of a test: repeated executions of the same cell
// gather here, generalizing repeated-verdict accumulation to every test kind.
final class Cell():
  private val mutex: Mutex = Mutex()
  private val runs0: scm.ArrayBuffer[Run] = scm.ArrayBuffer()

  def record(run: Run): Unit = mutex(runs0.append(run))
  def runs: List[Run] = mutex(runs0.to(List))

// One named test and its results: an ordered list of axes (which may grow during the run,
// as emergent axes acquire coordinates), and a sparse map of cells keyed by coordinates in
// axis space. A test with no axes has exactly one cell, at `Nil`; combinations of axis
// values without a cell are gaps, rendered as empty positions in a grid.
final class Entry(val id: TestId, val kind: Entry.Kind):
  private val mutex: Mutex = Mutex()
  private var axes0: List[Axis.Spec] = Nil
  private var ticks0: Map[Axis.Spec, List[Value]] = Map()
  private var cells0: ListMap[List[Value], Cell] = ListMap()

  var headline: Optional[Metric] = Unset
  var anchor: Optional[Anchor] = Unset

  def axes: List[Axis.Spec] = mutex(axes0)
  def cells: List[(List[Value], Cell)] = mutex(cells0.to(List))

  // Returns the cell at the given coordinates, creating it if absent. Appends any
  // not-yet-seen axes (emergent axes extend the axis list as their coordinates arrive) and
  // registers each axis's values in first-appearance order.
  def cell(coordinates: List[(Axis.Spec, Value)]): Cell = mutex:
    coordinates.each: (axis, value) =>
      if !axes0.contains(axis) then axes0 = axes0 :+ axis
      val seen = ticks0.at(axis).or(Nil)
      if !seen.contains(value) then ticks0 = ticks0.updated(axis, seen :+ value)

    val address = coordinates.map(_(1))
    if !cells0.defines(address) then cells0 = cells0.updated(address, Cell())
    cells0(address)

  // The coordinate values of one axis in presentation order: first-appearance order for
  // discrete axes, numeric order for integral and decimal axes.
  def values(axis: Axis.Spec): List[Value] =
    val seen = mutex(ticks0.at(axis).or(Nil))

    axis.domain match
      case Axis.Domain.Discrete => seen
      case _                    => seen.sortBy(_.numeric.or(0.0))
