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
  type SplitContext[VariantType, ResultType] =
    (typeclass: TypeclassType[VariantType], label: Text, ordinal: Int) ?=> ResultType
  
  type SplitContext2[VariantType, ResultType] = (label: Text, ordinal: Int) ?=> ResultType

  transparent inline def sum
      [DerivationType]
      (variant: Text)
      [ResultType]
      (inline split: [VariantType] => TypeclassType[VariantType] => SplitContext2[VariantType, ResultType])
      (using reflective: Reflection[DerivationType])
      : ResultType =

    inline reflective match
      case given SumReflection[DerivationType] =>
        sumRecur[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels](variant, 0)(split)

  private transparent inline def sumRecur
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (variant: Text, index: Int)
      [ResultType]
      (inline split: [VariantType] => TypeclassType[VariantType] => SplitContext2[VariantType, ResultType])
      (using reflective: SumReflection[DerivationType])
      : ResultType =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
          case label: String =>
            if label.tt == variant
            then split[variantType](summonInline[TypeclassType[variantType]])(using variant, index)
            else sumRecur[DerivationType, variantsType, moreLabelsType](variant, index + 1)(split)

        case EmptyTuple => throw Mistake(msg"unreachable")
      case EmptyTuple =>
        val raises = summonInline[Raises[VariantError]]
        val variants = constValueTuple[reflective.MirroredElemLabels].toList.map(_.toString.tt)
        abort(VariantError(variant, constValue[reflective.MirroredLabel].tt, variants))(using raises)
  
  transparent inline def variant
      [DerivationType]
      (sum: DerivationType)
      [ResultType]
      (inline split: [VariantType] => VariantType => SplitContext[VariantType, ResultType])
      (using reflective: SumReflection[DerivationType])
      : ResultType =

    inline reflective match
      case reflective: SumReflection[DerivationType] =>
        val ordinal = reflective.ordinal(sum)
        
        findVariant[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels](sum, ordinal, 0)(using reflective)(split)

  private transparent inline def findVariant
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (sum: DerivationType, ordinal: Int, index: Int)
      [ResultType]
      (using reflective: SumReflection[DerivationType])
      (inline split: [VariantType] => VariantType => SplitContext[VariantType, ResultType])
      : ResultType =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) =>
          if ordinal == 0 then inline valueOf[labelType].asMatchable match
            case label: String =>
              val typeclass = summonInline[TypeclassType[variantType]]
              split[variantType](sum.asInstanceOf[variantType])(using typeclass, label.tt, index)
          else findVariant[DerivationType, variantsType, moreLabelsType](sum, ordinal, index + 1)(split)
        case EmptyTuple => throw Mistake(msg"unreachable")
      case EmptyTuple => throw Mistake(msg"unreachable")

  inline def split[DerivationType: SumReflection]: TypeclassType[DerivationType]
  

trait ProductDerivationMethods[TypeclassType[_]]:

  type JoinContext[FieldType] = (label: Text, ordinal: Int) ?=> FieldType
  type JoinContext2[FieldType, ResultType] = (typeclass: TypeclassType[FieldType], label: Text, ordinal: Int) ?=> ResultType

  transparent inline def product
      [DerivationType]
      (using reflective: ProductReflection[DerivationType])
      (inline join: [FieldType] => TypeclassType[FieldType] => JoinContext[FieldType]) =
    
    reflective.fromProduct:
      productRecur[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels](0)(join)

  inline def typeName[DerivationType](using reflective: Reflection[DerivationType]): Text =
    valueOf[reflective.MirroredLabel].tt
  
  inline def tuple[DerivationType](using reflective: Reflection[DerivationType]): Boolean =
    compiletime.summonFrom:
      case given (reflective.MirroredMonoType <:< Tuple) => true
      case _                                             => false

  private transparent inline def productRecur
      [DerivationType, FieldsType <: Tuple, LabelsType <: Tuple]
      (index: Int)
      (inline join: [FieldType] => TypeclassType[FieldType] => JoinContext[FieldType])
      : Tuple =

    inline erasedValue[FieldsType] match
      case _: (fieldType *: moreFieldsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
          case label: String =>
            val typeclass = summonInline[TypeclassType[fieldType]]
            val field = join[fieldType](typeclass)(using label.tt, index)
          
            field *: productRecur[DerivationType, moreFieldsType, moreLabelsType](index + 1)(join)

        case EmptyTuple => EmptyTuple
      case EmptyTuple => EmptyTuple

  transparent inline def params
      [DerivationType]
      (inline product: DerivationType)
      (using reflective: ProductReflection[DerivationType])
      [ResultType: ClassTag]
      (inline join: [FieldType] => FieldType => JoinContext2[FieldType, ResultType])
      : IArray[ResultType] =
    
    val array: Array[ResultType] = new Array(valueOf[Tuple.Size[reflective.MirroredElemTypes]])
    
    inline product.asMatchable match
      case product: Product => inline reflective match
        case given ProductReflection[DerivationType & Product] =>
          paramsRecur[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels, ResultType]
              (Tuple.fromProductTyped(product), array, 0)(join)
    
    array.immutable(using Unsafe)
    
  private transparent inline def paramsRecur
      [DerivationType, FieldsType <: Tuple, LabelsType <: Tuple, ResultType]
      (tuple: Tuple, array: Array[ResultType], index: Int)
      (inline join: [FieldType] => FieldType => JoinContext2[FieldType, ResultType])
      : Unit =

    inline tuple match
      case EmptyTuple => ()
      case cons: (fieldType *: moreFieldsType) => cons match
        case param *: params => inline erasedValue[LabelsType] match
          case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
            case label: String =>
              val typeclass = summonInline[TypeclassType[fieldType]]
              array(index) = join[fieldType](param)(using typeclass, label.tt, index)
              
              paramsRecur[DerivationType, moreFieldsType, moreLabelsType, ResultType](params, array, index + 1)(join)

  inline def join[DerivationType: ProductReflection]: TypeclassType[DerivationType]
  // transparent inline def oneParam
  //     [DerivationType]
  //     (inline product: DerivationType)
  //     (index: Int)
  //     (using reflective: ProductReflection[DerivationType])
  //     [ResultType]
  //     (inline join: [FieldType] => FieldType => JoinContext2[FieldType])
  //     : ResultType =
    
  //   inline product.asMatchable match
  //     case product: Product => inline reflective match
  //       case given ProductReflection[DerivationType & Product] =>
  //         oneParamRecur[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels, ResultType]
  //             (Tuple.fromProductTyped(product), 0, index)(join)
    
  // private transparent inline def oneParamRecur
  //     [DerivationType, FieldsType <: Tuple, LabelsType <: Tuple, ResultType]
  //     (tuple: Tuple, index: Int, target: Int)
  //     (inline join: [FieldType] => FieldType => JoinContext2[FieldType])
  //     : ResultType =

  //   inline tuple match
  //     case EmptyTuple => throw Mistake(msg"should be unreachable")
  //     case cons: (fieldType *: moreFieldsType) => cons match
  //       case param *: params => inline erasedValue[LabelsType] match
  //         case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
  //           case label: String =>
  //             if index == target
  //             then join[fieldType](param)(using summonInline[TypeclassType[fieldType]], label.tt, index)
  //             else oneParamRecur[DerivationType, moreFieldsType, moreLabelsType, ResultType](params, index + 1, target)(join)

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