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

import anticipation.*
import hypotenuse.*

import scala.util as su

import java.security as js
import java.util as ju

import language.experimental.genericNumberLiterals

package randomization:
  package text:
    given bigListOfNaughtyStrings: Text is Randomizable:
      val resource = getClass.getResourceAsStream("/capricious/blns.txt").nn
      val blns = IArray.from(scala.io.Source.fromInputStream(resource).getLines().map(_.tt))

      def from(random: Random) = blns(random.long().toInt.abs%blns.length)

  package sizes:
    given uniformUpto10: RandomSize = _.long().toInt.abs%10
    given uniformUpto100: RandomSize = _.long().toInt.abs%100
    given uniformUpto1000: RandomSize = _.long().toInt.abs%1000
    given uniformUpto10000: RandomSize = _.long().toInt.abs%10000
    given uniformUpto100000: RandomSize = _.long().toInt.abs%100000

  given unseeded: Randomization = () => su.Random(java.util.Random())
  given secureUnseeded: Randomization = () => su.Random(js.SecureRandom())

  given stronglySecure: Randomization = () =>
    su.Random(js.SecureRandom.getInstanceStrong().nn)

  given seeded: (seed: Seed) => Randomization = () =>
    su.Random(ju.Random(seed.long))

  given secureSeeded: (seed: Seed) => Randomization = () =>
    su.Random(js.SecureRandom(seed.value.to(Array)))

def stochastic[ResultType](using randomization: Randomization)(block: Random ?=> ResultType)
:     ResultType =
  block(using new Random(randomization.make()))

def arbitrary[ValueType: Randomizable]()(using Random): ValueType = ValueType()

def random[ValueType: Randomizable](): ValueType =
  given Random = Random.global
  ValueType()

def toss()(using Random): Boolean = math.random < 0.5

package randomDistributions:
  given gaussian: Distribution = Gaussian()
  given uniformUnitInterval: Distribution = UniformDistribution(0, 1)
  given uniformSymmetricUnitInterval: Distribution = UniformDistribution(-1, 1)
  given binary: Distribution = random => Double(random.long())
