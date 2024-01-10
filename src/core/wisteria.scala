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
import vacuous.*
import anticipation.*

import scala.deriving.*
import scala.quoted.*
import scala.compiletime.*

object VariantError:
  inline def apply[DerivationType]()(using reflection: SumReflection[DerivationType]): VariantError =
    val variants = constValueTuple[reflection.MirroredElemLabels].toList.map(_.toString.tt)
    val sum = constValue[reflection.MirroredLabel].tt
    VariantError(sum, variants)

case class VariantError(sum: Text, validVariants: List[Text])
extends Error(msg"""the specified variant is not one of the valid variants (${validVariants.mkString(", ").tt})
                    of sum type $sum""")

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

      case reflection: SumReflection[derivationType] =>
        split[derivationType](using reflection).asMatchable match
          case typeclass: TypeclassType[DerivationType] => typeclass

type Reflection[DerivationType] = Mirror.Of[DerivationType]
type ProductReflection[DerivationType <: Product] = Mirror.ProductOf[DerivationType]
type SumReflection[DerivationType] = Mirror.SumOf[DerivationType]

object Wisteria:

  inline def default[ProductType, FieldType](index: Int): Optional[FieldType] =
    ${getDefault[ProductType, FieldType]('index)}

  def getDefault
      [ProductType: Type, FieldType: Type]
      (index: Expr[Int])
      (using Quotes)
      : Expr[Optional[FieldType]] =
    import quotes.reflect.*

    val methodName: String = "$lessinit$greater$default$"+(index.valueOrAbort + 1)
    val productSymbol = TypeRepr.of[ProductType].typeSymbol
    
    productSymbol.companionClass.declaredMethod(methodName).headOption.map: method =>
      Ref(productSymbol.companionModule).select(method)
    .map: selection =>
      TypeRepr.of[ProductType].typeArgs match
        case Nil  => selection
        case args => selection.appliedToTypes(args)
    .map(_.asExprOf[FieldType]).getOrElse('{Unset})
    
    