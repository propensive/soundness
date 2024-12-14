/*
    Capricious, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import rudiments.*

import scala.util as su

import java.security as js
import java.util as ju

import language.experimental.genericNumberLiterals

package randomization:
  package text:
    given Text is Randomizable as bigListOfNaughtyStrings:
      val resource = getClass.getResourceAsStream("/capricious/blns.txt").nn
      val blns = IArray.from(scala.io.Source.fromInputStream(resource).getLines().map(_.tt))

      def from(random: Random) = blns(random.long().toInt.abs%blns.length)

  package sizes:
    given RandomSize as uniformUpto10 = _.long().toInt.abs%10
    given RandomSize as uniformUpto100 = _.long().toInt.abs%100
    given RandomSize as uniformUpto1000 = _.long().toInt.abs%1000
    given RandomSize as uniformUpto10000 = _.long().toInt.abs%10000
    given RandomSize as uniformUpto100000 = _.long().toInt.abs%100000

  given Randomization as unseeded = () => su.Random(java.util.Random())
  given Randomization as secureUnseeded = () => su.Random(js.SecureRandom())

  given Randomization as stronglySecure = () =>
    su.Random(js.SecureRandom.getInstanceStrong().nn)

  given (using seed: Seed) => Randomization as seeded = () =>
    su.Random(ju.Random(seed.long))

  given (using seed: Seed) => Randomization as secureSeeded = () =>
    su.Random(js.SecureRandom(seed.value.to(Array)))

def stochastic[ResultType](using randomization: Randomization)(block: Random ?=> ResultType)
        : ResultType =
  block(using new Random(randomization.make()))

def arbitrary[ValueType: Randomizable]()(using Random): ValueType = ValueType()

def random[ValueType: Randomizable](): ValueType =
  given Random = Random.global
  ValueType()

def toss()(using Random): Boolean = math.random < 0.5

package randomDistributions:
  given Distribution as gaussian = Gaussian()
  given Distribution as uniformUnitInterval = UniformDistribution(0, 1)
  given Distribution as uniformSymmetricUnitInterval = UniformDistribution(-1, 1)
  given Distribution as binary = random => Double(random.long())
