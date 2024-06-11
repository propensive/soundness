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

import wisteria.*
import hypotenuse.*
import rudiments.*

import scala.compiletime.*

import language.experimental.genericNumberLiterals

object Arbitrary extends Derivation[[DerivationType] =>> DerivationType is Arbitrary]:
  given Int is Arbitrary as int = _.long().toInt
  given Long is Arbitrary as long = _.long()
  given Char is Arbitrary as char = _.long().toChar
  given Seed is Arbitrary as seed = _.long().pipe(Seed(_))
  given Boolean is Arbitrary as boolean = _.long() < 0L
  given (using Distribution) => Double is Arbitrary = summon[Distribution].transform(_)

  inline def join[DerivationType <: Product: ProductReflection]: DerivationType is Arbitrary = random =>
    stochastic(using summonInline[RandomNumberGenerator]):
      construct { [FieldType] => arbitrary => arbitrary.from(summon[Random]) }

  inline def split[DerivationType: SumReflection]: DerivationType is Arbitrary = random =>
    stochastic(using summonInline[RandomNumberGenerator]):
      delegate(variantLabels(random.long().abs.toInt%variantLabels.length)):
        [VariantType <: DerivationType] => arbitrary => arbitrary.from(summon[Random])

trait Arbitrary:
  type Self
  def apply()(using random: Random): Self = from(random)
  def from(random: Random): Self
