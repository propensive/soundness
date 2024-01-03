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

case class CoproductError(variant: Text, coproduct: Text, validVariants: List[Text])
extends Error(msg"""the specified $variant is not one of the valid variants (${validVariants.join(t", ")}) of
                    coproduct $coproduct""")

object Derivation:
  object sum:
    transparent inline def of
        [DerivationType, TypeclassType[_]]
        (variant: Text)
        [ResultType]
        (inline split: (typeclass: TypeclassType[DerivationType]) ?=> ResultType)
        (using mirror: Mirror.Of[DerivationType])
        : ResultType =

      inline mirror match
        case given Mirror.SumOf[DerivationType] =>
          sum[DerivationType, TypeclassType, mirror.MirroredElemTypes, mirror.MirroredElemLabels](variant):
            typeclass => split(using typeclass)

    transparent inline def from
        [DerivationType, TypeclassType[_]]
        (value: DerivationType)
        [ResultType]
        (inline split: (typeclass: TypeclassType[DerivationType], label: Text, variant: DerivationType,
            ordinal: Int) ?=> ResultType)
        (using mirror: Mirror.Of[DerivationType])
        : ResultType =

      inline mirror match
        case mirror: Mirror.SumOf[DerivationType] =>
          val ordinal = mirror.ordinal(value)
          sum[DerivationType, TypeclassType, mirror.MirroredElemTypes, mirror.MirroredElemLabels](ordinal)
              (using mirror):
            label => typeclass => split(using typeclass, label, value, ordinal)

    private transparent inline def sum
        [DerivationType, TypeclassType[_], VariantsType <: Tuple, LabelsType <: Tuple]
        (ordinal: Int)
        [ResultType]
        (using mirror: Mirror.SumOf[DerivationType])
        (inline split: Text => TypeclassType[DerivationType] => ResultType)
        : ResultType =

      inline erasedValue[VariantsType] match
        case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
          case _: (labelType *: labelsType) =>
            if ordinal == 0 then inline valueOf[labelType].asMatchable match
              case label: String => split(label.tt):
                summonInline[TypeclassType[variantType]].asInstanceOf[TypeclassType[DerivationType]]
            else sum[DerivationType, TypeclassType, variantsType, labelsType](ordinal - 1)(split)
          case EmptyTuple => throw Mistake(msg"unreachable")
        case EmptyTuple => throw Mistake(msg"unreachable")

    private transparent inline def sum
        [DerivationType, TypeclassType[_], VariantsType <: Tuple, LabelsType <: Tuple]
        (variant: Text)
        [ResultType]
        (inline split: TypeclassType[DerivationType] => ResultType)
        (using mirror: Mirror.SumOf[DerivationType])
        : ResultType =

      inline erasedValue[VariantsType] match
        case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
          case _: (labelType *: labelsType) => inline valueOf[labelType].asMatchable match
            case label: String =>
              if label.tt == variant
              then split(summonInline[TypeclassType[variantType]].asInstanceOf[TypeclassType[DerivationType]])
              else sum[DerivationType, TypeclassType, variantsType, labelsType](variant)(split)

          case EmptyTuple => throw Mistake(msg"unreachable")
        case EmptyTuple =>
          val raises = summonInline[Raises[CoproductError]]
          val variants = constValueTuple[mirror.MirroredElemLabels].toList.map(_.toString.tt)
          abort(CoproductError(variant, constValue[mirror.MirroredLabel].tt, variants))(using raises)
  
  object product:
    transparent inline def of
        [DerivationType, TypeclassType[_]]
        (using mirror: Mirror.ProductOf[DerivationType])
        (inline join: (typeclass: TypeclassType[Any], label: Text) ?=> Any) =
      
      mirror.fromProduct:
        product[DerivationType, TypeclassType, mirror.MirroredElemTypes, mirror.MirroredElemLabels]
            (typeclass ?=> label => join(using typeclass, label))

    private transparent inline def product
        [DerivationType, TypeclassType[_], ParamsType <: Tuple, LabelsType <: Tuple]
        (inline join: (typeclass: TypeclassType[Any]) ?=> Text => Any)
        : Tuple =

      inline erasedValue[ParamsType] match
        case _: (paramType *: paramsType) => inline erasedValue[LabelsType] match
          case _: (labelType *: labelsType) => inline valueOf[labelType].asMatchable match
            case label: String =>
              join(using summonInline[TypeclassType[paramType]].asInstanceOf[TypeclassType[Any]])(label.tt) *:
                  product[DerivationType, TypeclassType, paramsType, labelsType](join)
          case EmptyTuple => EmptyTuple
        case EmptyTuple => EmptyTuple

    transparent inline def from
        [DerivationType, TypeclassType[_]]
        (inline value: DerivationType)
        (using mirror: Mirror.ProductOf[DerivationType])
        [ResultType: ClassTag]
        (inline join: (typeclass: TypeclassType[Any], label: Text, param: Any) ?=> ResultType)
        : IArray[ResultType] =
      
      val array: Array[ResultType] = new Array(valueOf[Tuple.Size[mirror.MirroredElemTypes]])
      
      inline value.asMatchable match
        case value: Product => inline mirror match
          case given Mirror.ProductOf[DerivationType & Product] =>
            product[DerivationType, TypeclassType, mirror.MirroredElemTypes, mirror.MirroredElemLabels, ResultType]
                (Tuple.fromProductTyped(value), array, 0):
              (typeclass, label, param) => join(using typeclass, label, param)
      
      array.immutable(using Unsafe)
      

    private transparent inline def product
        [DerivationType, TypeclassType[_], ParamsType <: Tuple, LabelsType <: Tuple, ResultType]
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
                
                product[DerivationType, TypeclassType, paramsType, labelsType, ResultType](params, array, index + 1)(join)


trait Derived[TypeclassType[_]]:

  inline def join[DerivationType: Mirror.ProductOf]: TypeclassType[DerivationType]
  inline def split[DerivationType: Mirror.SumOf]: TypeclassType[DerivationType]

  inline given derived[DerivationType](using mirror: Mirror.Of[DerivationType]): TypeclassType[DerivationType] =
    inline mirror match
      case mirror: Mirror.ProductOf[DerivationType] =>
        join[DerivationType](using mirror)
      
      case mirror: Mirror.SumOf[DerivationType] =>
        split[DerivationType](using mirror)
