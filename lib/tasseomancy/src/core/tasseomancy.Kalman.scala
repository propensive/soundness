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

// A Kalman smoother for a run of measurements in order. The model is a constant velocity:
// each value is the last plus its rate of change, both perturbed by process noise of unit
// variance, and observed with measurement noise of variance `ratio`, so a larger ratio trusts
// each measurement less and smooths more. The forward filter is followed by the
// Rauch–Tung–Striebel backward pass, so the estimate at each point draws on the points after it
// as well as those before, and a smoothed line follows a rise without lagging it and settles
// on a plateau without overshooting.
object Kalman:
  // A two-by-two matrix and a two-vector, as tuples: the state is a value and its velocity.
  private type Matrix = (Double, Double, Double, Double)
  private type Vector = (Double, Double)

  private def multiply(m: Matrix, n: Matrix): Matrix =
    ( m(0)*n(0) + m(1)*n(2), m(0)*n(1) + m(1)*n(3),
      m(2)*n(0) + m(3)*n(2), m(2)*n(1) + m(3)*n(3) )

  private def apply(m: Matrix, v: Vector): Vector = (m(0)*v(0) + m(1)*v(1), m(2)*v(0) + m(3)*v(1))
  private def transpose(m: Matrix): Matrix = (m(0), m(2), m(1), m(3))
  private def add(m: Matrix, n: Matrix): Matrix = (m(0) + n(0), m(1) + n(1), m(2) + n(2), m(3) + n(3))

  private def invert(m: Matrix): Matrix =
    val determinant = m(0)*m(3) - m(1)*m(2)
    if determinant == 0.0 then (0.0, 0.0, 0.0, 0.0)
    else (m(3)/determinant, -m(1)/determinant, -m(2)/determinant, m(0)/determinant)

  // The transition over one step, and the process noise of a unit white-noise acceleration.
  private val transition: Matrix = (1.0, 1.0, 0.0, 1.0)
  private val noise: Matrix = (0.25, 0.5, 0.5, 1.0)

  // One step of the forward filter: the estimate after the measurement, its covariance, and
  // the prediction and its covariance from before it, which the backward pass needs.
  private case class Step(estimate: Vector, covariance: Matrix, predicted: Vector, predictedCovariance: Matrix)

  def smooth(values: List[Double], ratio: Double): List[Double] = values match
    case Nil => Nil

    case first :: rest =>
      val initial: Step = Step((first, 0.0), (ratio, 0.0, 0.0, ratio), (first, 0.0), (ratio, 0.0, 0.0, ratio))
      val start: List[Step] = List(initial)

      // Forward, latest first.
      val forward: List[Step] =
        rest.fold(start): (acc, measured) =>
          acc match
            case previous :: _ =>
              val predicted = apply(transition, previous.estimate)
              val predictedCovariance =
                add(multiply(multiply(transition, previous.covariance), transpose(transition)), noise)

              val innovation = predictedCovariance(0) + ratio
              val gain: Vector = (predictedCovariance(0)/innovation, predictedCovariance(2)/innovation)
              val residual = measured - predicted(0)
              val estimate: Vector = (predicted(0) + gain(0)*residual, predicted(1) + gain(1)*residual)

              val covariance: Matrix =
                ( (1.0 - gain(0))*predictedCovariance(0), (1.0 - gain(0))*predictedCovariance(1),
                  predictedCovariance(2) - gain(1)*predictedCovariance(0),
                  predictedCovariance(3) - gain(1)*predictedCovariance(1) )

              Step(estimate, covariance, predicted, predictedCovariance) :: acc

            case _ =>
              acc

      // Backward, from the latest: each estimate is corrected towards the smoothed estimate
      // after it, through the gain that relates its covariance to the later prediction's.
      forward match
        case latest :: earlier =>
          val begin: (List[Double], Vector, Step) = (List(latest.estimate(0)), latest.estimate, latest)

          val smoothed: (List[Double], Vector, Step) =
            earlier.fold(begin): (acc, step) =>
              val later = acc(2)
              val laterSmoothed = acc(1)
              val gain = multiply(multiply(step.covariance, transpose(transition)), invert(later.predictedCovariance))
              val difference: Vector = (laterSmoothed(0) - later.predicted(0), laterSmoothed(1) - later.predicted(1))
              val correction = apply(gain, difference)
              val estimate: Vector = (step.estimate(0) + correction(0), step.estimate(1) + correction(1))
              (estimate(0) :: acc(0), estimate, step)

          smoothed(0)

        case _ =>
          Nil
