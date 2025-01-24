/*
    Wisteria, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import contingency.*
import fulminate.*
import rudiments.*
import vacuous.*

import scala.deriving.*
import scala.compiletime.*

trait SumDerivationMethods[TypeclassType[_]]:

  @deprecated("This method has been renamed to `choice`")
  transparent inline def allSingletons[DerivationType: SumReflection]: Boolean =
    choice[DerivationType]

  transparent inline def choice[DerivationType: SumReflection]: Boolean =
    inline erasedValue[DerivationType.MirroredElemTypes] match
      case _: (variantType *: variantTypes) => all[variantType, variantTypes]

  private transparent inline def all[VariantType, VariantTypes <: Tuple]: Boolean = summonFrom:
    case given (VariantType <:< Singleton) => inline erasedValue[VariantTypes] match
      case _: EmptyTuple                     => true
      case _: (variantType *: variantsType)  => all[variantType, variantsType]
    case _                                 => false

  protected transparent inline def complement[DerivationType, VariantType](sum: DerivationType)
     (using variantIndex: Int & VariantIndex[VariantType],
            reflection:   SumReflection[DerivationType])
  :     Optional[VariantType] =

    type Labels = reflection.MirroredElemLabels
    type Variants = reflection.MirroredElemTypes
    val size: Int = valueOf[Tuple.Size[reflection.MirroredElemTypes]]

    fold[DerivationType, Variants, Labels](sum, size, 0, false)(index == reflection.ordinal(sum)):
      [VariantType2 <: DerivationType] => field =>
        if index == variantIndex then field.asInstanceOf[VariantType] else Unset

  protected inline def variantLabels[DerivationType]
     (using reflection: SumReflection[DerivationType])
  :     List[Text] =

    constValueTuple[reflection.MirroredElemLabels].toList.map(_.toString.tt)

  protected transparent inline def singleton[DerivationType](input: Text)
     (using reflection: SumReflection[DerivationType])
  :     DerivationType =

    type Variants = reflection.MirroredElemTypes
    type Labels = reflection.MirroredElemLabels

    singletonFold[DerivationType, Variants, Labels](_ == input).or:
      summonInline[Tactic[VariantError]].give:
        abort(VariantError[DerivationType](input))

  private transparent inline def singletonFold
     [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
     (using reflection: SumReflection[DerivationType])
     (predicate: Text => Boolean)
  :     Optional[DerivationType] =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: labelsType) =>
          type VariantType = variantType & DerivationType

          if predicate(valueOf[labelType & String].tt)
          then summonInline[Mirror.ProductOf[VariantType]].fromProduct(EmptyTuple)
          else singletonFold[DerivationType, variantsType, labelsType](predicate)

      case _  =>
        Unset

  protected transparent inline def delegate[DerivationType](label: Text)
     (using reflection: SumReflection[DerivationType], requirement: ContextRequirement)
     [ResultType]
     (inline lambda: [VariantType <: DerivationType] =>
                         requirement.Optionality[TypeclassType[VariantType]] =>
                             (context: requirement.Optionality[TypeclassType[VariantType]],
                              label:   Text,
                              index:   Int & VariantIndex[VariantType]) ?=>
                                 ResultType)
  :     ResultType =

    type Labels = reflection.MirroredElemLabels
    type Variants = reflection.MirroredElemTypes

    val size: Int = valueOf[Tuple.Size[reflection.MirroredElemTypes]]
    val variantLabel = label

    // Here label comes from context of fold's predicate
    fold[DerivationType, Variants, Labels](variantLabel, size, 0, true)(label == variantLabel):
      [VariantType <: DerivationType] => context => lambda[VariantType](context)

    . vouch

  protected transparent inline def variant[DerivationType](sum: DerivationType)
     (using reflection: SumReflection[DerivationType], requirement: ContextRequirement)
     [ResultType]
     (inline lambda: [VariantType <: DerivationType] =>
                         VariantType =>
                             (context: requirement.Optionality[TypeclassType[VariantType]],
                              label:   Text,
                              index:   Int & VariantIndex[VariantType]) ?=>
                                 ResultType)
  :     ResultType =

    type Labels = reflection.MirroredElemLabels
    type Variants = reflection.MirroredElemTypes

    val size: Int = valueOf[Tuple.Size[reflection.MirroredElemTypes]]

    fold[DerivationType, Variants, Labels](sum, size, 0, false)(index == reflection.ordinal(sum)):
      [VariantType <: DerivationType] => variant => lambda[VariantType](variant)

    . vouch

  private transparent inline def fold[DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
     (inline inputLabel: Text, size: Int, index: Int, fallible: Boolean)
     (using reflection: SumReflection[DerivationType], requirement: ContextRequirement)
     (inline predicate: (label: Text, index: Int & VariantIndex[DerivationType]) ?=> Boolean)
     [ResultType]
     (inline lambda: [VariantType <: DerivationType] =>
                         requirement.Optionality[TypeclassType[VariantType]] =>
                             (context: requirement.Optionality[TypeclassType[VariantType]],
                              label:   Text,
                              index:   Int & VariantIndex[VariantType]) ?=>
                                 ResultType)
  :     Optional[ResultType] =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) =>
          type VariantType = variantType & DerivationType
          if index >= size then Unset else
            valueOf[labelType].asMatchable.absolve match
              case label: String =>
                val index2: Int & VariantIndex[DerivationType] = VariantIndex[DerivationType](index)

                if predicate(using label.tt, index2)
                then
                  val index3: Int & VariantIndex[VariantType] = VariantIndex[VariantType](index)
                  val context = requirement.wrap(summonInline[TypeclassType[VariantType]])
                  lambda[VariantType](context)(using context, label.tt, index3)
                else
                  fold
                    [DerivationType, variantsType, moreLabelsType]
                    (inputLabel, size, index + 1, fallible)
                    (predicate)
                    (lambda)

      case _ =>
        inline if fallible
        then raise(VariantError[DerivationType](inputLabel), Unset)
              (using summonInline[Tactic[VariantError]])
        else panic(m"Should be unreachable")

  private transparent inline def fold[DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
     (inline sum: DerivationType, size: Int, index: Int, fallible: Boolean)
     (using reflection: SumReflection[DerivationType], requirement: ContextRequirement)
     (inline predicate: (label: Text, index: Int & VariantIndex[DerivationType]) ?=> Boolean)
     [ResultType]
     (inline lambda: [VariantType <: DerivationType] =>
                         VariantType =>
                             (context: requirement.Optionality[TypeclassType[VariantType]],
                              label: Text,
                              index: Int & VariantIndex[VariantType]) ?=>
                                 ResultType)
  :     Optional[ResultType] =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) =>
          type VariantType = variantType & DerivationType
          if index >= size then Unset else
            valueOf[labelType].asMatchable.absolve match
              case label: String =>
                val index2: Int & VariantIndex[DerivationType] = VariantIndex[DerivationType](index)

                if predicate(using label.tt, index2)
                then
                  val index3: Int & VariantIndex[VariantType] = VariantIndex[VariantType](index)
                  val variant: VariantType = sum.asInstanceOf[VariantType]
                  val context = requirement.wrap(summonInline[TypeclassType[VariantType]])
                  lambda[VariantType](variant)(using context, label.tt, index3)
                else
                  fold
                    [DerivationType, variantsType, moreLabelsType]
                    (sum, size, index + 1, fallible)
                    (predicate)
                    (lambda)

      case _ =>
        inline if fallible
        then raise(VariantError[DerivationType]("".tt), Unset)
              (using summonInline[Tactic[VariantError]])
        else panic(m"Should be unreachable")

  inline def split[DerivationType: SumReflection]: TypeclassType[DerivationType]
