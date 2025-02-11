/*
    Capricious, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package capricious

import hypotenuse.*

import language.experimental.genericNumberLiterals

object Gamma:
  def approximate(mean: Double, variance: Double): Gamma =
    val scale: Double = variance/mean
    val shape: Int = (mean/scale + 0.5).toInt
    Gamma(shape, scale)

case class Gamma(shape: Int, scale: Double) extends Distribution:
  def mean: F64 = F64(shape*scale)
  def variance: F64 = mean*scale
  def variationCoefficient: F64 = F64(shape) ** -0.5
  def skewness: F64 = variationCoefficient*2.0

  def transform(random: Random): Double =
    def accumulate(sum: Double, count: Int): Double =
      if count == 0 then sum*scale else
        val gaussian = randomDistributions.gaussian.transform(random)
        accumulate(sum + gaussian*gaussian, count - 1)

    accumulate(0.0, shape)
