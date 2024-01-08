/*
    Wisteria, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package wisteria

import fulminate.*
import rudiments.*
import anticipation.*

import scala.deriving.*
import scala.compiletime.*

object VariantError:
  inline def apply[DerivationType](variant: Text)(using reflection: SumReflection[DerivationType]): VariantError =
    val variants = constValueTuple[reflection.MirroredElemLabels].toList.map(_.toString.tt)
    val sum = constValue[reflection.MirroredLabel].tt
    VariantError(variant, sum, variants)

case class VariantError(variant: Text, sum: Text, validVariants: List[Text])
extends Error(msg"""the specified $variant is not one of the valid variants (${validVariants.mkString(", ").tt})
                    of sum $sum""")

trait ProductDerivation[TypeclassType[_]] extends ProductDerivationMethods[TypeclassType]:
  inline given derived[DerivationType](using Reflection[DerivationType]): TypeclassType[DerivationType] =

    inline summon[Reflection[DerivationType]] match
      case reflection: ProductReflection[derivationType] =>
        join[derivationType](using reflection).asMatchable match
          case typeclass: TypeclassType[DerivationType] => typeclass

trait Derivation[TypeclassType[_]]
extends ProductDerivationMethods[TypeclassType], SumDerivationMethods[TypeclassType]:
  
  inline given derived[DerivationType](using Reflection[DerivationType]): TypeclassType[DerivationType] =
    inline summon[Reflection[DerivationType]] match
      case reflection: ProductReflection[derivationType] =>
        join[derivationType](using reflection).asMatchable match
          case typeclass: TypeclassType[DerivationType] => typeclass

      case given SumReflection[DerivationType] =>
        split

type Reflection[DerivationType] = Mirror.Of[DerivationType]
type ProductReflection[DerivationType <: Product] = Mirror.ProductOf[DerivationType]
type SumReflection[DerivationType] = Mirror.SumOf[DerivationType]