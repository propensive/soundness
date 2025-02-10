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

import scala.util as su

import language.experimental.genericNumberLiterals

object Random:
  lazy val global: Random = new Random(randomization.unseeded.make())

  def apply(seed: Seed)(using randomization: Randomization): Random =
    new Random(randomization.make())

class Random(private val generator: su.Random):
  def long(): Long = generator.nextLong()
  def gaussian(): Double = generator.nextGaussian()
  def unitInterval(): Double = generator.nextDouble()
  def apply[ValueType: Randomizable](): ValueType = ValueType.from(this)

  transparent inline def shuffle[ElementType](seq: Seq[ElementType]): Seq[ElementType] =
    generator.shuffle(seq)
