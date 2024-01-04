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

package wisteria2

import fulminate.*
import gossamer.*
import perforate.*
import rudiments.*
import vacuous.*
import anticipation.*

import scala.deriving.*
import scala.compiletime.*

case class VariantError(variant: Text, sum: Text, validVariants: List[Text])
extends Error(msg"""the specified $variant is not one of the valid variants (${validVariants.join(t", ")}) of
                    sum $sum""")

trait SumDerivationMethods[TypeclassType[_]]:
  transparent inline def sum
      [DerivationType]
      (variant: Text)
      [ResultType]
      (inline split: (typeclass: TypeclassType[DerivationType]) ?=> ResultType)
      (using reflective: Reflection[DerivationType])
      : ResultType =

    inline reflective match
      case given SumReflection[DerivationType] =>
        sumRecur[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels](variant):
          typeclass => split(using typeclass)

  transparent inline def variants
      [DerivationType]
      (sum: DerivationType)
      [ResultType]
      (inline split: (typeclass: TypeclassType[DerivationType], label: Text, variant: DerivationType,
          ordinal: Int) ?=> ResultType)
      (using reflective: SumReflection[DerivationType])
      : ResultType =

    inline reflective match
      case reflective: SumReflection[DerivationType] =>
        val ordinal = reflective.ordinal(sum)
        
        sumRecur[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels](ordinal)
            (using reflective):
          label => typeclass => split(using typeclass, label, sum, ordinal)

  private transparent inline def sumRecur
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (ordinal: Int)
      [ResultType]
      (using reflective: SumReflection[DerivationType])
      (inline split: Text => TypeclassType[DerivationType] => ResultType)
      : ResultType =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: labelsType) =>
          if ordinal == 0 then inline valueOf[labelType].asMatchable match
            case label: String => split(label.tt):
              summonInline[TypeclassType[variantType]].asInstanceOf[TypeclassType[DerivationType]]
          else sumRecur[DerivationType, variantsType, labelsType](ordinal - 1)(split)
        case EmptyTuple => throw Mistake(msg"unreachable")
      case EmptyTuple => throw Mistake(msg"unreachable")

  private transparent inline def sumRecur
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (variant: Text)
      [ResultType]
      (inline split: TypeclassType[DerivationType] => ResultType)
      (using reflective: SumReflection[DerivationType])
      : ResultType =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: labelsType) => inline valueOf[labelType].asMatchable match
          case label: String =>
            if label.tt == variant
            then split(summonInline[TypeclassType[variantType]].asInstanceOf[TypeclassType[DerivationType]])
            else sumRecur[DerivationType, variantsType, labelsType](variant)(split)

        case EmptyTuple => throw Mistake(msg"unreachable")
      case EmptyTuple =>
        val raises = summonInline[Raises[VariantError]]
        val variants = constValueTuple[reflective.MirroredElemLabels].toList.map(_.toString.tt)
        abort(VariantError(variant, constValue[reflective.MirroredLabel].tt, variants))(using raises)
  
  inline def split[DerivationType: SumReflection]: TypeclassType[DerivationType]
  

trait ProductDerivationMethods[TypeclassType[_]]:
  transparent inline def product
      [DerivationType]
      (using reflective: ProductReflection[DerivationType])
      (inline join: (typeclass: TypeclassType[Any], label: Text, ordinal: Int) ?=> Any) =
    
    reflective.fromProduct:
      productRecur[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels](0)
          (typeclass => label => ordinal => join(using typeclass, label, ordinal))

  private transparent inline def productRecur
      [DerivationType, ParamsType <: Tuple, LabelsType <: Tuple]
      (index: Int)
      (inline join: TypeclassType[Any] => Text => Int => Any)
      : Tuple =

    inline erasedValue[ParamsType] match
      case _: (paramType *: paramsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: labelsType) => inline valueOf[labelType].asMatchable match
          case label: String =>
            val typeclass = summonInline[TypeclassType[paramType]].asInstanceOf[TypeclassType[Any]]
            join(typeclass)(label.tt)(index) *:
                productRecur[DerivationType, paramsType, labelsType](index + 1)(join)
        case EmptyTuple => EmptyTuple
      case EmptyTuple => EmptyTuple

  transparent inline def params
      [DerivationType]
      (inline product: DerivationType)
      (using reflective: ProductReflection[DerivationType])
      [ResultType: ClassTag]
      (inline join: (typeclass: TypeclassType[Any], label: Text, ordinal: Int, param: Any) ?=> ResultType)
      : IArray[ResultType] =
    
    val array: Array[ResultType] = new Array(valueOf[Tuple.Size[reflective.MirroredElemTypes]])
    
    inline product.asMatchable match
      case product: Product => inline reflective match
        case given ProductReflection[DerivationType & Product] =>
          paramsRecur[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels, ResultType]
              (Tuple.fromProductTyped(product), array, 0):
            (typeclass, label, ordinal, param) => join(using typeclass, label, ordinal, param)
    
    array.immutable(using Unsafe)
    
  transparent inline def oneParam
      [DerivationType]
      (inline product: DerivationType)
      (index: Int)
      (using reflective: ProductReflection[DerivationType])
      [ResultType]
      (inline join: (typeclass: TypeclassType[Any], label: Text, ordinal: Int, param: Any) ?=> ResultType)
      : ResultType =
    
    inline product.asMatchable match
      case product: Product => inline reflective match
        case given ProductReflection[DerivationType & Product] =>
          paramRecur[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels, ResultType]
              (Tuple.fromProductTyped(product), 0, index):
            (typeclass, label, ordinal, param) => join(using typeclass, label, ordinal, param)
    
  private transparent inline def paramRecur
      [DerivationType, ParamsType <: Tuple, LabelsType <: Tuple, ResultType]
      (tuple: Tuple, index: Int, target: Int)
      (inline join: (TypeclassType[Any], Text, Int, Any) => ResultType)
      : ResultType =

    inline tuple match
      case EmptyTuple => throw Mistake(msg"should be unreachable")
      case cons: (paramType *: paramsType) => cons match
        case param *: params => inline erasedValue[LabelsType] match
          case _: (labelType *: labelsType) => inline valueOf[labelType].asMatchable match
            case label: String =>
              if index == target
              then join(summonInline[TypeclassType[paramType]].asInstanceOf[TypeclassType[Any]], label.tt, index, param)
              else paramRecur[DerivationType, paramsType, labelsType, ResultType](params, index + 1, target)(join)
  
  private transparent inline def paramsRecur
      [DerivationType, ParamsType <: Tuple, LabelsType <: Tuple, ResultType]
      (tuple: Tuple, array: Array[ResultType], index: Int)
      (inline join: (TypeclassType[Any], Text, Int, Any) => ResultType)
      : Unit =

    inline tuple match
      case EmptyTuple => ()
      case cons: (paramType *: paramsType) => cons match
        case param *: params => inline erasedValue[LabelsType] match
          case _: (labelType *: labelsType) => inline valueOf[labelType].asMatchable match
            case label: String =>
              array(index) = join(summonInline[TypeclassType[paramType]].asInstanceOf[TypeclassType[Any]],
                  label.tt, index, param)
              
              paramsRecur[DerivationType, paramsType, labelsType, ResultType](params, array, index + 1)(join)

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