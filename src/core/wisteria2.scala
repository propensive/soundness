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

trait Derivation[TypeclassType[_]] extends ProductDerivation[TypeclassType]:
  protected object sum:
    transparent inline def of
        [DerivationType]
        (variant: Text)
        [ResultType]
        (inline split: (typeclass: TypeclassType[DerivationType]) ?=> ResultType)
        (using reflective: Reflective[DerivationType])
        : ResultType =

      inline reflective match
        case given ReflectiveSum[DerivationType] =>
          sum[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels](variant):
            typeclass => split(using typeclass)

    transparent inline def from
        [DerivationType]
        (value: DerivationType)
        [ResultType]
        (inline split: (typeclass: TypeclassType[DerivationType], label: Text, variant: DerivationType,
            ordinal: Int) ?=> ResultType)
        (using reflective: ReflectiveSum[DerivationType])
        : ResultType =

      inline reflective match
        case reflective: ReflectiveSum[DerivationType] =>
          val ordinal = reflective.ordinal(value)
          
          sum[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels](ordinal)
              (using reflective):
            label => typeclass => split(using typeclass, label, value, ordinal)

    private transparent inline def sum
        [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
        (ordinal: Int)
        [ResultType]
        (using reflective: ReflectiveSum[DerivationType])
        (inline split: Text => TypeclassType[DerivationType] => ResultType)
        : ResultType =

      inline erasedValue[VariantsType] match
        case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
          case _: (labelType *: labelsType) =>
            if ordinal == 0 then inline valueOf[labelType].asMatchable match
              case label: String => split(label.tt):
                summonInline[TypeclassType[variantType]].asInstanceOf[TypeclassType[DerivationType]]
            else sum[DerivationType, variantsType, labelsType](ordinal - 1)(split)
          case EmptyTuple => throw Mistake(msg"unreachable")
        case EmptyTuple => throw Mistake(msg"unreachable")

    private transparent inline def sum
        [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
        (variant: Text)
        [ResultType]
        (inline split: TypeclassType[DerivationType] => ResultType)
        (using reflective: ReflectiveSum[DerivationType])
        : ResultType =

      inline erasedValue[VariantsType] match
        case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
          case _: (labelType *: labelsType) => inline valueOf[labelType].asMatchable match
            case label: String =>
              if label.tt == variant
              then split(summonInline[TypeclassType[variantType]].asInstanceOf[TypeclassType[DerivationType]])
              else sum[DerivationType, variantsType, labelsType](variant)(split)

          case EmptyTuple => throw Mistake(msg"unreachable")
        case EmptyTuple =>
          val raises = summonInline[Raises[VariantError]]
          val variants = constValueTuple[reflective.MirroredElemLabels].toList.map(_.toString.tt)
          abort(VariantError(variant, constValue[reflective.MirroredLabel].tt, variants))(using raises)
  
  inline def split[DerivationType: ReflectiveSum]: TypeclassType[DerivationType]
  

trait ProductDerivation[TypeclassType[_]]:
  protected object product:
    transparent inline def of
        [DerivationType]
        (using reflective: ReflectiveProduct[DerivationType])
        (inline join: (typeclass: TypeclassType[Any], label: Text) ?=> Any) =
      
      reflective.fromProduct:
        product[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels]
            (typeclass ?=> label => join(using typeclass, label))

    private transparent inline def product
        [DerivationType, ParamsType <: Tuple, LabelsType <: Tuple]
        (inline join: (typeclass: TypeclassType[Any]) ?=> Text => Any)
        : Tuple =

      inline erasedValue[ParamsType] match
        case _: (paramType *: paramsType) => inline erasedValue[LabelsType] match
          case _: (labelType *: labelsType) => inline valueOf[labelType].asMatchable match
            case label: String =>
              join(using summonInline[TypeclassType[paramType]].asInstanceOf[TypeclassType[Any]])(label.tt) *:
                  product[DerivationType, paramsType, labelsType](join)
          case EmptyTuple => EmptyTuple
        case EmptyTuple => EmptyTuple

    transparent inline def from
        [DerivationType]
        (inline value: DerivationType)
        (using reflective: ReflectiveProduct[DerivationType])
        [ResultType: ClassTag]
        (inline join: (typeclass: TypeclassType[Any], label: Text, param: Any) ?=> ResultType)
        : IArray[ResultType] =
      
      val array: Array[ResultType] = new Array(valueOf[Tuple.Size[reflective.MirroredElemTypes]])
      
      inline value.asMatchable match
        case value: Product => inline reflective match
          case given ReflectiveProduct[DerivationType & Product] =>
            product[DerivationType, reflective.MirroredElemTypes, reflective.MirroredElemLabels, ResultType]
                (Tuple.fromProductTyped(value), array, 0):
              (typeclass, label, param) => join(using typeclass, label, param)
      
      array.immutable(using Unsafe)
      

    private transparent inline def product
        [DerivationType, ParamsType <: Tuple, LabelsType <: Tuple, ResultType]
        (tuple: Tuple, array: Array[ResultType], index: Int)
        (inline join: (TypeclassType[Any], Text, Any) => ResultType)
        : Unit =

      inline tuple match
        case EmptyTuple => ()
        case cons: (paramType *: paramsType) => cons match
          case param *: params => inline erasedValue[LabelsType] match
            case _: (labelType *: labelsType) => inline valueOf[labelType].asMatchable match
              case label: String =>
                array(index) = join(summonInline[TypeclassType[paramType]].asInstanceOf[TypeclassType[Any]],
                    label.tt, param)
                
                product[DerivationType, paramsType, labelsType, ResultType](params, array, index + 1)(join)

  inline def join[DerivationType: ReflectiveProduct]: TypeclassType[DerivationType]

  inline given derived
      [DerivationType]
      (using reflective: Reflective[DerivationType])
      : TypeclassType[DerivationType] =

    inline this match
      case derivation: Derivation[TypeclassType] =>
        inline reflective match
          case reflective: ReflectiveProduct[DerivationType] =>
            join[DerivationType](using reflective)
          case reflective: ReflectiveSum[DerivationType] =>
            derivation.split[DerivationType](using reflective)
      
      case derivation: ProductDerivation[TypeclassType] =>
        inline reflective match
          case reflective: ReflectiveProduct[DerivationType] => join[DerivationType](using reflective)


type Reflective[DerivationType] = Mirror.Of[DerivationType]
type ReflectiveProduct[DerivationType] = Mirror.ProductOf[DerivationType]
type ReflectiveSum[DerivationType] = Mirror.SumOf[DerivationType]