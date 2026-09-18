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
package tasseomancy

import murmuration.fold
import prepositional.*
import vacuous.*

// A Kalman smoother for a run of measurements in order: a random-walk model, in which each
// value is the last plus process noise of unit variance, observed with measurement noise of
// variance `ratio`, so a larger ratio trusts each measurement less and smooths more. The
// forward filter is followed by the Rauch–Tung–Striebel backward pass, so the estimate at each
// point draws on the points after it as well as those before, and the smoothed line neither
// lags a rising curve nor overshoots a plateau.
object Kalman:
  def smooth(values: List[Double], ratio: Double): List[Double] = values match
    case Nil => Nil

    case first :: rest =>
      // Forward, latest first: each step's estimate, its variance, and the variance predicted
      // for it before its measurement, which the backward pass needs.
      type Step = (Double, Double, Double)
      val start: List[Step] = List((first, ratio, ratio + 1.0))

      val forward: List[Step] =
        rest.fold(start): (acc, measured) =>
          acc match
            case previous :: _ =>
              val predicted = previous(1) + 1.0
              val gain = predicted/(predicted + ratio)
              val estimate = previous(0) + gain*(measured - previous(0))
              (estimate, (1.0 - gain)*predicted, predicted) :: acc

            case _ =>
              acc

      // Backward, from the latest: each estimate is corrected towards the smoothed estimate
      // after it, in proportion to how much of that later prediction's variance was its own.
      forward match
        case latest :: earlier =>
          val begin: (List[Double], Double, Double) = (List(latest(0)), latest(0), latest(2))

          val smoothed: (List[Double], Double, Double) =
            earlier.fold(begin): (acc, step) =>
              val laterSmoothed = acc(1)
              val laterPredicted = acc(2)
              val correction = if laterPredicted == 0.0 then 0.0 else step(1)/laterPredicted
              val estimate = step(0) + correction*(laterSmoothed - step(0))
              (estimate :: acc(0), estimate, step(2))

          smoothed(0)

        case _ =>
          Nil
