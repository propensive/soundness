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
import perforate.*
import rudiments.*
import vacuous.*
import anticipation.*

import scala.deriving.*
import scala.compiletime.*

case class VariantError(variant: Text, sum: Text, validVariants: List[Text])
extends Error(msg"""the specified $variant is not one of the valid variants (${validVariants.mkString(", ").tt})
                    of sum $sum""")

trait SumDerivationMethods[TypeclassType[_]]:

  transparent inline def sum
      [DerivationType]
      (variant: Text)
      (inline lambda: [VariantType <: DerivationType] => TypeclassType[VariantType] => (label: Text, ordinal: Int) ?=> VariantType)
      (using reflective: Reflection[DerivationType])
      : DerivationType =

    inline reflective match
      case given SumReflection[DerivationType] =>
        sumRecur[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels](variant, 0)(lambda)

  private transparent inline def sumRecur
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (variant: Text, index: Int)
      (inline lambda: [VariantType <: DerivationType] => TypeclassType[VariantType] => (label: Text, ordinal: Int) ?=> VariantType)
      (using reflective: SumReflection[DerivationType])
      : DerivationType =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
          case label: String =>
            if label.tt == variant
            then lambda[variantType & DerivationType](summonInline[TypeclassType[variantType & DerivationType]])(using variant, index)
            else sumRecur[DerivationType, variantsType, moreLabelsType](variant, index + 1)(lambda)

        case EmptyTuple => throw Mistake(msg"unreachable")
      case EmptyTuple =>
        val raises = summonInline[Raises[VariantError]]
        val variants = constValueTuple[reflective.MirroredElemLabels].toList.map(_.toString.tt)
        abort(VariantError(variant, constValue[reflective.MirroredLabel].tt, variants))(using raises)
  
  transparent inline def variant
      [DerivationType]
      (sum: DerivationType)
      [ResultType]
      (inline lambda: [VariantType <: DerivationType] => VariantType => (typeclass: TypeclassType[VariantType], label: Text, ordinal: Int) ?=> ResultType)
      (using reflective: SumReflection[DerivationType])
      : ResultType =

    inline reflective match
      case reflective: SumReflection[DerivationType] =>
        val ordinal = reflective.ordinal(sum)
        
        findVariant[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels](sum, ordinal, 0)(using reflective)(lambda)

  private transparent inline def findVariant
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (sum: DerivationType, ordinal: Int, index: Int)
      [ResultType]
      (using reflective: SumReflection[DerivationType])
      (inline lambda: [VariantType <: DerivationType] => VariantType => (typeclass: TypeclassType[VariantType], label: Text, ordinal: Int) ?=> ResultType)
      : ResultType =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) =>
          if ordinal == 0 then inline valueOf[labelType].asMatchable match
            case label: String =>
              val typeclass = summonInline[TypeclassType[variantType & DerivationType]]
              lambda[variantType & DerivationType ](sum.asInstanceOf[variantType & DerivationType])(using typeclass, label.tt, index)
          else findVariant[DerivationType, variantsType, moreLabelsType](sum, ordinal, index + 1)(lambda)
        case EmptyTuple => throw Mistake(msg"unreachable")
      case EmptyTuple => throw Mistake(msg"unreachable")

  inline def split[DerivationType: SumReflection]: TypeclassType[DerivationType]
  

trait ProductDerivationMethods[TypeclassType[_]]:

  transparent inline def product
      [DerivationType]
      (using reflective: ProductReflection[DerivationType])
      (inline lambda: [FieldType] => TypeclassType[FieldType] => (label: Text, ordinal: Int) ?=> FieldType) =
    
    reflective.fromProduct:
      productRecur[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels](0)(lambda)

  inline def typeName[DerivationType](using reflective: Reflection[DerivationType]): Text =
    valueOf[reflective.MirroredLabel].tt
  
  inline def tuple[DerivationType](using reflective: Reflection[DerivationType]): Boolean =
    compiletime.summonFrom:
      case given (reflective.MirroredMonoType <:< Tuple) => true
      case _                                             => false

  private transparent inline def productRecur
      [DerivationType, FieldsType <: Tuple, LabelsType <: Tuple]
      (index: Int)
      (inline lambda: [FieldType] => TypeclassType[FieldType] => (label: Text, ordinal: Int) ?=> FieldType)
      : Tuple =

    inline erasedValue[FieldsType] match
      case _: (fieldType *: moreFieldsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
          case label: String =>
            val typeclass = summonInline[TypeclassType[fieldType]]
            val field = lambda[fieldType](typeclass)(using label.tt, index)
          
            field *: productRecur[DerivationType, moreFieldsType, moreLabelsType](index + 1)(lambda)

        case EmptyTuple => EmptyTuple
      case EmptyTuple => EmptyTuple

  transparent inline def params
      [DerivationType]
      (inline product: DerivationType)
      (using reflective: ProductReflection[DerivationType])
      [ResultType: ClassTag]
      (inline lambda: [FieldType] => FieldType => (typeclass: TypeclassType[FieldType], label: Text, ordinal: Int) ?=> ResultType)
      : IArray[ResultType] =
    
    val array: Array[ResultType] = new Array(valueOf[Tuple.Size[reflective.MirroredElemTypes]])
    
    inline product.asMatchable match
      case product: Product => inline reflective match
        case given ProductReflection[DerivationType & Product] =>
          paramsRecur[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels, ResultType]
              (Tuple.fromProductTyped(product), array, 0)(lambda)
    
    array.immutable(using Unsafe)
    
  private transparent inline def paramsRecur
      [DerivationType, FieldsType <: Tuple, LabelsType <: Tuple, ResultType]
      (tuple: Tuple, array: Array[ResultType], index: Int)
      (inline lambda: [FieldType] => FieldType => (typeclass: TypeclassType[FieldType], label: Text, ordinal: Int) ?=> ResultType)
      : Unit =

    inline tuple match
      case EmptyTuple => ()
      case cons: (fieldType *: moreFieldsType) => cons match
        case param *: params => inline erasedValue[LabelsType] match
          case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
            case label: String =>
              val typeclass = summonInline[TypeclassType[fieldType]]
              array(index) = lambda[fieldType](param)(using typeclass, label.tt, index)
              
              paramsRecur[DerivationType, moreFieldsType, moreLabelsType, ResultType](params, array, index + 1)(lambda)

  inline def join[DerivationType: ProductReflection]: TypeclassType[DerivationType]

trait ProductDerivation[TypeclassType[_]] extends ProductDerivationMethods[TypeclassType]:
  inline given derived
      [DerivationType]
      (using reflective: Reflection[DerivationType])
      : TypeclassType[DerivationType] =

    inline reflective match
      case reflective: ProductReflection[DerivationType] => join[DerivationType](using reflective)

trait Derivation[TypeclassType[_]]
extends ProductDerivationMethods[TypeclassType], SumDerivationMethods[TypeclassType]:
  inline given derived
      [DerivationType]
      (using reflective: Reflection[DerivationType])
      : TypeclassType[DerivationType] =

    inline reflective match
      case reflective: ProductReflection[DerivationType] => join[DerivationType](using reflective)
      case reflective: SumReflection[DerivationType]     => split[DerivationType](using reflective)

type Reflection[DerivationType] = Mirror.Of[DerivationType]
type ProductReflection[DerivationType] = Mirror.ProductOf[DerivationType]
type SumReflection[DerivationType] = Mirror.SumOf[DerivationType]