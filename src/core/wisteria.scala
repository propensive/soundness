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

  protected transparent inline def sum
      [DerivationType]
      (variant: Text)
      (inline lambda: [VariantType <: DerivationType] => TypeclassType[VariantType] => (label: Text, ordinal: Int) ?=> VariantType)
      (using reflection: Reflection[DerivationType])
      : DerivationType =

    inline reflection match
      case given SumReflection[DerivationType] =>
        sumRecur[DerivationType, reflection.MirroredElemTypes, reflection.MirroredElemLabels](variant, 0)(lambda)

  private transparent inline def sumRecur
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (variant: Text, index: Int)
      (inline lambda: [VariantType <: DerivationType] => TypeclassType[VariantType] => (label: Text, ordinal: Int) ?=> VariantType)
      (using reflection: SumReflection[DerivationType])
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
        val variants = constValueTuple[reflection.MirroredElemLabels].toList.map(_.toString.tt)
        abort(VariantError(variant, constValue[reflection.MirroredLabel].tt, variants))(using raises)
  
  protected transparent inline def variant
      [DerivationType]
      (sum: DerivationType)
      [ResultType]
      (inline lambda: [VariantType <: DerivationType] => VariantType => (typeclass: TypeclassType[VariantType], label: Text, ordinal: Int) ?=> ResultType)
      (using reflection: SumReflection[DerivationType])
      : ResultType =

    inline reflection match
      case reflection: SumReflection[DerivationType] =>
        val ordinal = reflection.ordinal(sum)
        
        findVariant[DerivationType, reflection.MirroredElemTypes, reflection.MirroredElemLabels](sum, ordinal, 0)(using reflection)(lambda)

  private transparent inline def findVariant
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (sum: DerivationType, ordinal: Int, index: Int)
      [ResultType]
      (using reflection: SumReflection[DerivationType])
      (inline lambda: [VariantType <: DerivationType] => VariantType => (typeclass: TypeclassType[VariantType], label: Text, ordinal: Int) ?=> ResultType)
      : ResultType =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) =>
          type variantSubtype = variantType & DerivationType

          if ordinal == 0 then inline valueOf[labelType].asMatchable match
            case label: String =>
              val typeclass = summonInline[TypeclassType[variantSubtype]]
              
              lambda[variantType & DerivationType](sum.asInstanceOf[variantSubtype])
                  (using typeclass, label.tt, index)

          else findVariant[DerivationType, variantsType, moreLabelsType](sum, ordinal, index + 1)(lambda)
        case EmptyTuple => throw Mistake(msg"unreachable")
      case EmptyTuple => throw Mistake(msg"unreachable")

  inline def split[DerivationType: SumReflection]: TypeclassType[DerivationType]
  

trait ProductDerivationMethods[TypeclassType[_]]:
  protected transparent inline def product
      [DerivationType]
      (using reflection: ProductReflection[DerivationType])
      (inline lambda: [FieldType] => TypeclassType[FieldType] => (label: Text, index: Int) ?=> FieldType) =
    
    reflection.fromProduct:
      foldTuple[DerivationType, reflection.MirroredElemLabels, Tuple]
          (erasedValue[reflection.MirroredElemTypes], EmptyTuple, 0):
        accumulator => [FieldType] => field =>
          val typeclass = summonInline[TypeclassType[FieldType]]
          lambda[FieldType](typeclass)(using label, index) *: accumulator
      .reverse

  inline def typeName[DerivationType](using reflection: Reflection[DerivationType]): Text =
    valueOf[reflection.MirroredLabel].tt
  
  inline def tuple[DerivationType](using reflection: Reflection[DerivationType]): Boolean =
    compiletime.summonFrom:
      case given (reflection.MirroredMonoType <:< Tuple) => true
      case _                                             => false

  protected transparent inline def fields
      [DerivationType]
      (inline product: DerivationType)
      (using reflection: ProductReflection[DerivationType])
      [ResultType]
      (inline lambda: [FieldType] => FieldType =>
          (typeclass: TypeclassType[FieldType], label: Text, index: Int) ?=> ResultType)
      : IArray[ResultType] =
    
    summonInline[ClassTag[ResultType]].contextually:
      inline product.asMatchable match
        case product: Product => inline reflection match
          case given ProductReflection[DerivationType & Product] =>
            val array: Array[ResultType] = new Array(valueOf[Tuple.Size[reflection.MirroredElemTypes]])
            
            foldTuple[DerivationType, reflection.MirroredElemLabels, Array[ResultType]]
                (Tuple.fromProductTyped(product), array, 0):
              accumulator => [FieldType] => field =>

                accumulator(index) = lambda[FieldType](field)
                accumulator

            .immutable(using Unsafe)

  private transparent inline def foldTuple
      [DerivationType, LabelsType <: Tuple, AccumulatorType]
      (tuple: Tuple, accumulator: AccumulatorType, index: Int)
      (inline lambda: AccumulatorType => [FieldType] => FieldType =>
          (typeclass: TypeclassType[FieldType], label: Text, index: Int) ?=> AccumulatorType)
      : AccumulatorType =

    inline tuple match
      case EmptyTuple =>
        accumulator
      
      case cons: (fieldType *: moreFieldsType) => cons match
        case field *: fields => inline erasedValue[LabelsType] match
          case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
            case label: String =>
              val typeclass = summonInline[TypeclassType[fieldType]]
              val accumulator2 = lambda(accumulator)[fieldType](field)(using typeclass, label.tt, index)
              
              foldTuple[DerivationType, moreLabelsType, AccumulatorType]
                  (fields, accumulator2, index + 1)(lambda)

  inline def join[DerivationType: ProductReflection]: TypeclassType[DerivationType]

trait ProductDerivation[TypeclassType[_]] extends ProductDerivationMethods[TypeclassType]:
  inline given derived
      [DerivationType]
      (using reflection: Reflection[DerivationType])
      : TypeclassType[DerivationType] =

    inline reflection match
      case reflection: ProductReflection[DerivationType] => join[DerivationType](using reflection)

trait Derivation[TypeclassType[_]]
extends ProductDerivationMethods[TypeclassType], SumDerivationMethods[TypeclassType]:
  inline given derived
      [DerivationType]
      (using reflection: Reflection[DerivationType])
      : TypeclassType[DerivationType] =

    inline reflection match
      case reflection: ProductReflection[DerivationType] => join[DerivationType](using reflection)
      case reflection: SumReflection[DerivationType]     => split[DerivationType](using reflection)

type Reflection[DerivationType] = Mirror.Of[DerivationType]
type ProductReflection[DerivationType] = Mirror.ProductOf[DerivationType]
type SumReflection[DerivationType] = Mirror.SumOf[DerivationType]