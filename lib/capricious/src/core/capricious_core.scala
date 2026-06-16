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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package capricious

import language.experimental.genericNumberLiterals

import java.security as js
import java.util as ju

import scala.util as su

import anticipation.*
import hypotenuse.*

package randomization:
  package text:
    given bigListOfNaughtyStrings: Text is Randomizable:
      val resource = getClass.getResourceAsStream("/capricious/blns.txt").nn
      val blns = IArray.from(scala.io.Source.fromInputStream(resource).getLines().map(_.tt))

      def randomize(random: Random) = blns(random.long().toInt.abs%blns.length)

  package sizes:
    given uniformUpto10: RandomSize = _.long().toInt.abs%10
    given uniformUpto100: RandomSize = _.long().toInt.abs%100
    given uniformUpto1000: RandomSize = _.long().toInt.abs%1000
    given uniformUpto10000: RandomSize = _.long().toInt.abs%10000
    given uniformUpto100000: RandomSize = _.long().toInt.abs%100000

  given unseededRandomization: Randomization = () => su.Random(java.util.Random())
  given secureUnseededRandomization: Randomization = () => su.Random(js.SecureRandom())

  given stronglySecureRandomization: Randomization = () =>
    su.Random(js.SecureRandom.getInstanceStrong().nn)

  given seededRandomization: (seed: Seed) => Randomization = () =>
    su.Random(ju.Random(seed.long))

  given secureSeededRandomization: (seed: Seed) => Randomization = () =>
    su.Random(js.SecureRandom(seed.value.to(Array)))

def stochastic[result](using randomization: Randomization)(block: Random ?=> result): result =
  block(using new Random(randomization.initialize()))

def arbitrary[value: Randomizable]()(using Random): value = value()

def random[value: Randomizable](): value =
  given globalRandom: Random = Random.global
  value()

def toss()(using Random): Boolean = math.random() < 0.5

package randomDistributions:
  given gaussianDistribution: Distribution = Gaussian()
  given uniformUnitIntervalDistribution: Distribution = UniformDistribution(0, 1)
  given uniformSymmetricUnitIntervalDistribution: Distribution = UniformDistribution(-1, 1)
  given binaryDistribution: Distribution = random => Double(random.long())
