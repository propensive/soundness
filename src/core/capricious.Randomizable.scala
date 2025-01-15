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
  given Byte is Randomizable as byte = _.long().toByte
  given Short is Randomizable as short = _.long().toShort
  given Int is Randomizable as int = _.long().toInt
  given Long is Randomizable as long = _.long()
  given Char is Randomizable as char = _.long().toChar
  given Seed is Randomizable as seed = _.long().pipe(Seed(_))
  given Boolean is Randomizable as boolean = _.long() < 0L

  given [ElementType: Randomizable](using size: RandomSize)
      => List[ElementType] is Randomizable =
    random =>
      given Random = random
      List.fill(size.generate(random))(arbitrary[ElementType]())

  given [ElementType: Randomizable](using size: RandomSize)
      => Set[ElementType] is Randomizable =
    random =>
      given Random = random
      Set.fill(size.generate(random))(arbitrary[ElementType]())

  given [ElementType: {Randomizable, ClassTag}](using size: RandomSize)
      => IArray[ElementType] is Randomizable =
    random =>
      given Random = random
      IArray.fill(size.generate(random))(arbitrary[ElementType]())

  given (using Distribution) => Double is Randomizable = summon[Distribution].transform(_)

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
