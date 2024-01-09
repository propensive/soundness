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
import perforate.*
import fulminate.*

import scala.deriving.*
import scala.compiletime.*

trait SumDerivationMethods[TypeclassType[_]]:
  protected transparent inline def complement
      [DerivationType, VariantType]
      (sum: DerivationType)
      (using variantIndex: Int & VariantIndex[VariantType], reflection: SumReflection[DerivationType])
      : Optional[VariantType] =
    
    type Labels = reflection.MirroredElemLabels
    type Variants = reflection.MirroredElemTypes
    val size: Int = valueOf[Tuple.Size[reflection.MirroredElemTypes]]
    
    fold[DerivationType, Variants, Labels](sum, size, 0, false)(index == reflection.ordinal(sum)):
      [VariantType2 <: DerivationType] => field =>
        if index == variantIndex then field.asInstanceOf[VariantType] else Unset

  protected inline def variants[DerivationType](using reflection: SumReflection[DerivationType]): List[Text] =
    constValueTuple[reflection.MirroredElemLabels].toList.map(_.toString.tt)

  protected transparent inline def delegate
      [DerivationType]
      (label: Text)
      (using reflection: SumReflection[DerivationType], requirement: ContextRequirement)
      [ResultType]
      (inline lambda: [VariantType <: DerivationType] => requirement.Optionality[TypeclassType[VariantType]] =>
          (context: requirement.Optionality[TypeclassType[VariantType]], label: Text,
          index: Int & VariantIndex[VariantType]) ?=> ResultType)
      : ResultType =

    type Labels = reflection.MirroredElemLabels
    type Variants = reflection.MirroredElemTypes
    
    val size: Int = valueOf[Tuple.Size[reflection.MirroredElemTypes]]
    val variantLabel = label
    
    fold[DerivationType, Variants, Labels](size, 0, true)(label == variantLabel):
      [VariantType <: DerivationType] => context => lambda[VariantType](context)
    .vouch(using Unsafe)

  protected transparent inline def variant
      [DerivationType]
      (sum: DerivationType)
      (using reflection: SumReflection[DerivationType], requirement: ContextRequirement)
      [ResultType]
      (inline lambda: [VariantType <: DerivationType] => VariantType =>
          (context: requirement.Optionality[TypeclassType[VariantType]], label: Text,
          index: Int & VariantIndex[VariantType]) ?=> ResultType)
      : ResultType =

    type Labels = reflection.MirroredElemLabels
    type Variants = reflection.MirroredElemTypes

    val size: Int = valueOf[Tuple.Size[reflection.MirroredElemTypes]]
    
    fold[DerivationType, Variants, Labels](sum, size, 0, false)(index == reflection.ordinal(sum)):
      [VariantType <: DerivationType] => variant => lambda[VariantType](variant)
    .vouch(using Unsafe)
  
  private transparent inline def fold
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (inline size: Int, index: Int, fallible: Boolean)
      (using reflection: SumReflection[DerivationType], requirement: ContextRequirement)
      (inline predicate: (label: Text, index: Int & VariantIndex[DerivationType]) ?=> Boolean)
      [ResultType]
      (inline lambda: [VariantType <: DerivationType] => requirement.Optionality[TypeclassType[VariantType]] =>
          (context: requirement.Optionality[TypeclassType[VariantType]], label: Text,
          index: Int & VariantIndex[VariantType]) ?=> ResultType)
      : Optional[ResultType] =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) =>
          type VariantType = variantType & DerivationType
          if index >= size then Unset else
            (valueOf[labelType].asMatchable: @unchecked) match
              case label: String =>
                val index2: Int & VariantIndex[DerivationType] = VariantIndex[DerivationType](index)
                
                if predicate(using label.tt, index2)
                then
                  val index3: Int & VariantIndex[VariantType] = VariantIndex[VariantType](index)
                  val context = requirement.wrap(summonInline[TypeclassType[VariantType]])
                  lambda[VariantType](context)(using context, label.tt, index3)
                else fold[DerivationType, variantsType, moreLabelsType](size, index + 1, fallible)(predicate)
                    (lambda)
        
      case _ =>
        inline if fallible
        then raise(VariantError[DerivationType]())(Unset)(using summonInline[Raises[VariantError]])
        else throw Mistake(msg"Should be unreachable")
    
  private transparent inline def fold
      [DerivationType, VariantsType <: Tuple, LabelsType <: Tuple]
      (inline sum: DerivationType, size: Int, index: Int, fallible: Boolean)
      (using reflection: SumReflection[DerivationType], requirement: ContextRequirement)
      (inline predicate: (label: Text, index: Int & VariantIndex[DerivationType]) ?=> Boolean)
      [ResultType]
      (inline lambda: [VariantType <: DerivationType] => VariantType =>
          (context: requirement.Optionality[TypeclassType[VariantType]], label: Text,
          index: Int & VariantIndex[VariantType]) ?=> ResultType)
      : Optional[ResultType] =

    inline erasedValue[VariantsType] match
      case _: (variantType *: variantsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) =>
          type VariantType = variantType & DerivationType
          if index >= size then Unset else
            (valueOf[labelType].asMatchable: @unchecked) match
              case label: String =>
                val index2: Int & VariantIndex[DerivationType] = VariantIndex[DerivationType](index)
                
                if predicate(using label.tt, index2)
                then
                  val index3: Int & VariantIndex[VariantType] = VariantIndex[VariantType](index)
                  val variant: VariantType = sum.asInstanceOf[VariantType]
                  val context = requirement.wrap(summonInline[TypeclassType[VariantType]])
                  lambda[VariantType](variant)(using context, label.tt, index3)
                else fold[DerivationType, variantsType, moreLabelsType](sum, size, index + 1, fallible)
                    (predicate)(lambda)
        
      case _ =>
        inline if fallible
        then raise(VariantError[DerivationType]())(Unset)(using summonInline[Raises[VariantError]])
        else throw Mistake(msg"Should be unreachable")
  
  inline def split[DerivationType: SumReflection]: TypeclassType[DerivationType]

object VariantIndex:
  inline def apply[VariantType](int: Int): Int & VariantIndex[VariantType] =
    int.asInstanceOf[Int & VariantIndex[VariantType]]

erased trait VariantIndex[VariantType]