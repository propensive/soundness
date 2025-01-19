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
import rudiments.*
import wisteria.*

import scala.compiletime.*

import language.experimental.genericNumberLiterals

object Randomizable extends Derivation[[DerivationType] =>> DerivationType is Randomizable]:
  given byte: Byte is Randomizable = _.long().toByte
  given short: Short is Randomizable = _.long().toShort
  given int: Int is Randomizable = _.long().toInt
  given long: Long is Randomizable = _.long()
  given char: Char is Randomizable = _.long().toChar
  given seed: Seed is Randomizable = _.long().pipe(Seed(_))
  given boolean: Boolean is Randomizable = _.long() < 0L

  given [ElementType: Randomizable] => (size: RandomSize) => List[ElementType] is Randomizable =
    random =>
      given Random = random
      List.fill(size.generate(random))(arbitrary[ElementType]())

  given [ElementType: Randomizable] => (size: RandomSize) => Set[ElementType] is Randomizable =
    random =>
      given Random = random
      Set.fill(size.generate(random))(arbitrary[ElementType]())

  given [ElementType: {Randomizable, ClassTag}] => (size: RandomSize)
      => IArray[ElementType] is Randomizable =
    random =>
      given Random = random
      IArray.fill(size.generate(random))(arbitrary[ElementType]())

  given Distribution => Double is Randomizable = summon[Distribution].transform(_)

  inline def join[DerivationType <: Product: ProductReflection]: DerivationType is Randomizable =
    random =>
      stochastic(using summonInline[Randomization]):
        construct { [FieldType] => _.from(summon[Random]) }

  inline def split[DerivationType: SumReflection]: DerivationType is Randomizable = random =>
    stochastic(using summonInline[Randomization]):
      delegate(variantLabels(random.long().abs.toInt%variantLabels.length)):
        [VariantType <: DerivationType] => _.from(summon[Random])

trait Randomizable:
  type Self
  def apply()(using random: Random): Self = from(random)
  def from(random: Random): Self
