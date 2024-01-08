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
  protected transparent inline def delegate
      [DerivationType]
      (variant: Text)
      (inline lambda: [VariantType <: DerivationType] => TypeclassType[VariantType] =>
          (context: TypeclassType[VariantType], label: Text, index: Int & VariantIndex[VariantType]) ?=>
          Optional[VariantType])
      (using reflection: SumReflection[DerivationType], variantError: Raises[VariantError])
      : Optional[DerivationType] =

    foldErased[DerivationType, reflection.MirroredElemTypes, reflection.MirroredElemLabels]():
      [VariantType <: DerivationType] => typeclassOption =>
        val typeclass = summonInline[TypeclassType[VariantType]]
        if variant == label then lambda[VariantType](typeclass)(using typeclass, label, index) else Unset

  protected transparent inline def complement
      [DerivationType]
      (sum: DerivationType)
      (using reflection: SumReflection[DerivationType])
      [VariantType <: DerivationType]
      (using variantIndex: Int & VariantIndex[VariantType])
      [ResultType]
      (inline lambda: VariantType => (context: Optional[TypeclassType[VariantType]], label: Text,
          index: Int & VariantIndex[VariantType]) ?=> ResultType)
      : Optional[ResultType] =

    type Labels = reflection.MirroredElemLabels
    type Variants = reflection.MirroredElemTypes

    fold[DerivationType, Variants, Labels](sum, 0, reflection.ordinal(sum)):
      [VariantType2 <: DerivationType] => variant =>
        val variant2 = variant.asInstanceOf[VariantType]
        val context2 = context.asInstanceOf[Optional[TypeclassType[VariantType]]]
        val index2: Int & VariantIndex[VariantType] = index.asInstanceOf[Int & VariantIndex[VariantType]]
        lambda(variant2)(using context2, label, index2)

  protected transparent inline def variant
      [DerivationType]
      (sum: DerivationType)
      (using reflection: SumReflection[DerivationType], requirement: ContextRequirement)
      [ResultType]
      (inline lambda: [VariantType <: DerivationType] => VariantType =>
          (context: requirement.Optionality[TypeclassType[VariantType]], label: Text,
          index: Int & VariantIndex[VariantType]) ?=> ResultType)
      : ResultType =

    fold[DerivationType, reflection.MirroredElemTypes, reflection.MirroredElemLabels](sum, reflection.ordinal(
        sum), 0):
      [VariantType <: DerivationType] => variant => lambda[VariantType](variant)

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
            type VariantType = variantType & DerivationType
            val typeclass = summonInline[TypeclassType[VariantType]]
            val variantIndex = index.asInstanceOf[Int & VariantIndex[VariantType]]

            lambda[variantType & DerivationType](typeclass)(using label.tt, variantIndex) match
              case Unset  => foldErased[DerivationType, variantsType, moreLabelsType](index + 1)(lambda)
              case result => result

        case EmptyTuple =>
          throw Mistake(msg"unreachable")

      case EmptyTuple =>
        throw Mistake(msg"unreachable")
  
  private transparent inline def fold
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (sum: DerivationType, index: Int, variantIndex: Int)
      [ResultType]
      (using reflection: SumReflection[DerivationType], requirement: ContextRequirement)
      (inline lambda: [VariantType <: DerivationType] => VariantType =>
          (context: requirement.Optionality[TypeclassType[VariantType]], label: Text,
          index: Int & VariantIndex[VariantType]) ?=> ResultType)
      : ResultType =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) =>
          type VariantType = variantType & DerivationType

          if index == 0 then inline valueOf[labelType].asMatchable match
            case label: String =>
              val context = requirement.wrap(summonInline[TypeclassType[VariantType]])
              
              lambda[variantType & DerivationType](sum.asInstanceOf[VariantType])
                  (using context, label.tt, variantIndex.asInstanceOf[Int & VariantIndex[VariantType]])

          else fold[DerivationType, variantsType, moreLabelsType](sum, index, variantIndex + 1)(lambda)
        case EmptyTuple => throw Mistake(msg"unreachable")
      case EmptyTuple => throw Mistake(msg"unreachable")

  inline def split[DerivationType: SumReflection]: TypeclassType[DerivationType]

erased trait VariantIndex[VariantType]