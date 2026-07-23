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

import anticipation.*
import gossamer.*

object Metric:
  // The physical interpretation of a metric's value, used to choose formatting (time,
  // memory or rate units) uniformly across test kinds.
  enum Dimension:
    case Time, Memory, Rate, Count, Fraction

  // Whether a smaller or a larger value is an improvement, used for baseline comparisons
  // and chart orientation; `Neutral` metrics are informational only.
  enum Sense:
    case LowerIsBetter, HigherIsBetter, Neutral

import Metric.{Dimension, Sense}

// The closed set of quantities a test run can record. Each cell of a test stores a map of
// these to values; one of them (or, for unit tests, the pass/fail status) is the test's
// headline, used for grids and comparisons. Times are in nanoseconds, memory in bytes,
// rates in operations per second and fractions in the unit interval.
enum Metric(val dimension: Dimension, val sense: Sense, val label: Text):
  case Duration   extends Metric(Dimension.Time,     Sense.LowerIsBetter,  t"Time")
  case Mean       extends Metric(Dimension.Time,     Sense.LowerIsBetter,  t"μ")
  case Least      extends Metric(Dimension.Time,     Sense.LowerIsBetter,  t"Min")
  case Most       extends Metric(Dimension.Time,     Sense.LowerIsBetter,  t"Max")
  case Deviation  extends Metric(Dimension.Time,     Sense.Neutral,        t"σ")
  case Confidence extends Metric(Dimension.Fraction, Sense.Neutral,        t"Confidence")
  case Iterations extends Metric(Dimension.Count,    Sense.Neutral,        t"n")
  case Throughput extends Metric(Dimension.Rate,     Sense.HigherIsBetter, t"Throughput")
  case Operations extends Metric(Dimension.Count,    Sense.Neutral,        t"Ops")
  case Allocation extends Metric(Dimension.Memory,   Sense.LowerIsBetter,  t"Alloc·op¯¹")
  case PeakHeap   extends Metric(Dimension.Memory,   Sense.LowerIsBetter,  t"Peak")
  case Retained   extends Metric(Dimension.Memory,   Sense.LowerIsBetter,  t"Retained")
  case GcCount    extends Metric(Dimension.Count,    Sense.LowerIsBetter,  t"GC n")
  case GcTime     extends Metric(Dimension.Time,     Sense.LowerIsBetter,  t"GC t")
  case P50        extends Metric(Dimension.Time,     Sense.LowerIsBetter,  t"p50")
  case P90        extends Metric(Dimension.Time,     Sense.LowerIsBetter,  t"p90")
  case P99        extends Metric(Dimension.Time,     Sense.LowerIsBetter,  t"p99")
  case P999       extends Metric(Dimension.Time,     Sense.LowerIsBetter,  t"p999")
  case Compliance extends Metric(Dimension.Fraction, Sense.HigherIsBetter, t"SLO")
  case Samples    extends Metric(Dimension.Count,    Sense.Neutral,        t"Samples")
