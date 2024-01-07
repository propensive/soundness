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

import anticipation.*
import rudiments.*
import vacuous.*
import fulminate.*
import perforate.*

import scala.deriving.*
import scala.compiletime.*

trait SumDerivationMethods[TypeclassType[_]]:
  protected transparent inline def sum
      [DerivationType]
      (variant: Text)
      (inline lambda: [VariantType <: DerivationType] => TypeclassType[VariantType] => (label: Text,
          index: Int & VariantIndex[VariantType]) ?=> Optional[VariantType])
      (using reflection: SumReflection[DerivationType])
      : Optional[DerivationType] =

    foldErased[DerivationType, reflection.MirroredElemTypes, reflection.MirroredElemLabels]():
      [VariantType <: DerivationType] => typeclassOption =>
        val typeclass = summonInline[TypeclassType[VariantType]]
        if variant == label then lambda[VariantType](typeclass)(using label, index) else Unset

  protected transparent inline def correspondent
      [DerivationType]
      (sum: DerivationType)
      (using reflection: SumReflection[DerivationType])
      [VariantType <: DerivationType]
      (using variantIndex: Int & VariantIndex[VariantType])
      [ResultType]
      (inline lambda: VariantType =>
          (optionalTypeclass: Optional[TypeclassType[VariantType]], label: Text, index: Int & VariantIndex[VariantType]) ?=> ResultType)
      : Optional[ResultType] =

    type Labels = reflection.MirroredElemLabels
    type Variants = reflection.MirroredElemTypes

    fold[DerivationType, Variants, Labels](sum, 0, reflection.ordinal(sum)):
      [VariantType2 <: DerivationType] => variant =>
        val variant2 = variant.asInstanceOf[VariantType]
        val optionalTypeclass2 = optionalTypeclass.asInstanceOf[Optional[TypeclassType[VariantType]]]
        val index2: Int & VariantIndex[VariantType] = index.asInstanceOf[Int & VariantIndex[VariantType]]
        lambda(variant2)(using optionalTypeclass2, label, index2)

  protected transparent inline def variant
      [DerivationType]
      (sum: DerivationType)
      [ResultType]
      (inline lambda: [VariantType <: DerivationType] => VariantType =>
          (optionalTypeclass: Optional[TypeclassType[VariantType]], label: Text, index: Int & VariantIndex[VariantType]) ?=> ResultType)
      (using reflection: SumReflection[DerivationType])
      : ResultType =

    inline reflection match
      case reflection: SumReflection[DerivationType] =>
        val index = reflection.ordinal(sum)
        
        fold[DerivationType, reflection.MirroredElemTypes, reflection.MirroredElemLabels](sum, index, 0)(lambda)

  private transparent inline def foldErased
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (index: Int = 0)
      [ResultType]
      (inline lambda: [VariantType <: DerivationType] => Optional[TypeclassType[VariantType]] =>
          (label: Text, index: Int & VariantIndex[VariantType]) ?=> Optional[ResultType])
      : Optional[ResultType] =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
          case label: String =>
            val typeclass = summonInline[TypeclassType[variantType & DerivationType]]
            
            lambda[variantType & DerivationType](typeclass)(using label.tt, index.asInstanceOf[Int & VariantIndex[variantType & DerivationType]]) match
              case Unset => foldErased[DerivationType, variantsType, moreLabelsType](index + 1)(lambda)
              case result => result

        case EmptyTuple =>
          throw Mistake(msg"unreachable")

      case EmptyTuple =>
        throw Mistake(msg"unreachable")
  
  private transparent inline def fold
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (sum: DerivationType, index: Int, variantIndex: Int)
      [ResultType]
      (using reflection: SumReflection[DerivationType])
      (inline lambda: [VariantType <: DerivationType] => VariantType =>
          (optionalTypeclass: Optional[TypeclassType[VariantType]], label: Text,
          index: Int & VariantIndex[VariantType]) ?=> ResultType)
      : ResultType =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) =>
          type variantSubtype = variantType & DerivationType

          if index == 0 then inline valueOf[labelType].asMatchable match
            case label: String =>
              val typeclass = summonInline[TypeclassType[variantSubtype]]
              
              lambda[variantType & DerivationType](sum.asInstanceOf[variantSubtype])
                  (using typeclass, label.tt, variantIndex.asInstanceOf[Int & VariantIndex[variantType & DerivationType]])

          else fold[DerivationType, variantsType, moreLabelsType](sum, index, variantIndex + 1)(lambda)
        case EmptyTuple => throw Mistake(msg"unreachable")
      case EmptyTuple => throw Mistake(msg"unreachable")

  inline def split[DerivationType: SumReflection]: TypeclassType[DerivationType]

erased trait VariantIndex[VariantType]