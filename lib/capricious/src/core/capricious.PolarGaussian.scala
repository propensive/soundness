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

case class PolarGaussian(mean: Double = 0.0, standardDeviation: Double = 1.0) extends Distribution:
  def transform(random: Random): Double =
    @annotation.tailrec
    def recur(): F64 =
      val u0: F64 = F64(randomDistributions.uniformSymmetricUnitInterval.transform(random))
      val u1: F64 = F64(randomDistributions.uniformSymmetricUnitInterval.transform(random))
      val s: F64 = hyp(u0, u1)
      if s >= 1 || s == 0 then recur() else F64(standardDeviation)*u0*(-ln(s)/s).sqrt*2 + mean

    recur().double
