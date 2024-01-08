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

import scala.deriving.*
import scala.compiletime.*

trait ProductDerivationMethods[TypeclassType[_]]:
  protected transparent inline def construct
      [DerivationType <: Product]
      (using reflection: ProductReflection[DerivationType])
      (inline lambda: [FieldType] => TypeclassType[FieldType] => (typeclass: TypeclassType[FieldType],
          label: Text, index: Int & FieldIndex[FieldType]) ?=> FieldType): DerivationType =
    
    reflection.fromProduct:
      foldErased[DerivationType, reflection.MirroredElemTypes, reflection.MirroredElemLabels, Tuple, true]
          (EmptyTuple, 0):
        accumulator => [FieldType] => context =>
          val context2 = context.asInstanceOf[TypeclassType[FieldType]]
          lambda[FieldType](context2)(using context2, label, index) *: accumulator
      .reverse

  protected transparent inline def fieldContexts
      [DerivationType <: Product]
      (using reflection: ProductReflection[DerivationType], requirement: ContextRequirement)
      [ResultType]
      (inline lambda: [FieldType] => requirement.Optionality[TypeclassType[FieldType]] =>
          (typeclass: requirement.Optionality[TypeclassType[FieldType]], label: Text,
          index: Int & FieldIndex[FieldType]) ?=> ResultType): IArray[ResultType] =
    
    summonInline[ClassTag[ResultType]].contextually:
      val array: Array[ResultType] = new Array(valueOf[Tuple.Size[reflection.MirroredElemTypes]])
      
      foldErased[DerivationType, reflection.MirroredElemTypes, reflection.MirroredElemLabels, Unit,
          requirement.RequiredType]((), 0):
        accumulator => [FieldType] => context =>
          val context2: requirement.Optionality[TypeclassType[FieldType]] = requirement.wrap(context)
          array(index) = lambda[FieldType](context2)(using context2, label, index)
      
      array.immutable(using Unsafe)
  
  inline def typeName[DerivationType](using reflection: Reflection[DerivationType]): Text =
    valueOf[reflection.MirroredLabel].tt
  
  inline def tuple[DerivationType](using reflection: Reflection[DerivationType]): Boolean =
    compiletime.summonFrom:
      case given (reflection.MirroredMonoType <:< Tuple) => true
      case _                                             => false

  inline def singleton[DerivationType](using reflection: Reflection[DerivationType]): Boolean =
    compiletime.summonFrom:
      case given (reflection.MirroredMonoType <:< Singleton) => true
      case _                                                 => false

  protected transparent inline def complement
      [DerivationType <: Product, FieldType, RequirementType <: Boolean]
      (product: DerivationType)
      (using fieldIndex: Int & FieldIndex[FieldType], reflection: ProductReflection[DerivationType],
          requirement: ContextRequirement)
      : FieldType =
    
    type Labels = reflection.MirroredElemLabels
    
    inline product.asMatchable match
      case product: Product => inline reflection match
        case given ProductReflection[DerivationType & Product] =>
          fold[DerivationType, Labels, Optional[FieldType], false](Tuple.fromProductTyped(product), Unset, 0):
            accumulator => [FieldType2] => field =>
              if index == fieldIndex then field.asInstanceOf[FieldType] else accumulator
          .vouch(using Unsafe)

  protected transparent inline def fields
      [DerivationType <: Product]
      (inline product: DerivationType)
      (using requirement: ContextRequirement)
      (using reflection: ProductReflection[DerivationType])
      [ResultType]
      (inline lambda: [FieldType] => FieldType => (context: requirement.Optionality[TypeclassType[FieldType]],
          label: Text, index: Int & FieldIndex[FieldType]) ?=> ResultType)
      : IArray[ResultType] =
    
    summonInline[ClassTag[ResultType]].contextually:
      inline product.asMatchable match
        case product: Product => inline reflection match
          case given ProductReflection[DerivationType & Product] =>
            val array: Array[ResultType] = new Array(valueOf[Tuple.Size[reflection.MirroredElemTypes]])
            type Labels = reflection.MirroredElemLabels

            fold[DerivationType, Labels, Array[ResultType], requirement.RequiredType](Tuple.fromProductTyped(
                product), array, 0):
              accumulator => [FieldType] => field =>
                
                val typeclass: requirement.Optionality[TypeclassType[FieldType]] = requirement.wrap(context)
                accumulator(index) = lambda[FieldType](field)(using typeclass, label, index)
                accumulator
            .immutable(using Unsafe)

  protected transparent inline def fold
      [DerivationType, LabelsType <: Tuple, AccumulatorType, RequiredType <: Boolean]
      (using requirement: ContextRequirement)
      (inline tuple: Tuple, accumulator: AccumulatorType, index: Int)
      (inline lambda: AccumulatorType => [FieldType] => FieldType =>
          (context: Optional[TypeclassType[FieldType]], label: Text, index: Int & FieldIndex[FieldType]) ?=>
          AccumulatorType)
      : AccumulatorType =

    inline tuple match
      case EmptyTuple =>
        accumulator
      
      case cons: (fieldType *: moreFieldsType) => cons match
        case field *: moreFields => inline erasedValue[LabelsType] match
          case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
            case label: String =>
              val typeclass =
                inline if erasedValue[RequiredType] then summonInline[TypeclassType[`fieldType`]] else
                  summonFrom:
                    case typeclass: TypeclassType[`fieldType`] => typeclass
                    case _                                     => Unset
              
              val typeclass2 = requirement.wrap(typeclass)
              val fieldIndex: Int & FieldIndex[fieldType] = index.asInstanceOf[Int & FieldIndex[fieldType]]
              val accumulator2 = lambda(accumulator)[fieldType](field)(using typeclass2, label.tt, fieldIndex)
              
              fold[DerivationType, moreLabelsType, AccumulatorType, RequiredType](moreFields, accumulator2,
                  index + 1)(lambda)

  private transparent inline def foldErased
      [DerivationType, TupleType <: Tuple, LabelsType <: Tuple, AccumulatorType, RequiredType <: Boolean]
      (using requirement: ContextRequirement)
      (inline accumulator: AccumulatorType, index: Int)
      (inline lambda: AccumulatorType => [FieldType] => requirement.Optionality[TypeclassType[FieldType]] =>
          (label: Text, index: Int & FieldIndex[FieldType]) ?=> AccumulatorType)
      : AccumulatorType =

    inline erasedValue[TupleType] match
      case _: EmptyTuple =>
        accumulator
      
      case _: (fieldType *: moreFieldsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
          case label: String =>
            val typeclass =
              inline if erasedValue[RequiredType] then summonInline[TypeclassType[`fieldType`]] else
                summonFrom:
                  case typeclass: TypeclassType[`fieldType`] => typeclass
                  case _                                     => Unset
            
            val typeclass2 = requirement.wrap(typeclass)
            val fieldIndex: Int & FieldIndex[fieldType] = index.asInstanceOf[Int & FieldIndex[fieldType]]
            val accumulator2 = lambda(accumulator)[fieldType](typeclass2)(using label.tt, fieldIndex)
            
            foldErased[DerivationType, moreFieldsType, moreLabelsType, AccumulatorType, RequiredType](
                accumulator2, index + 1)(lambda)
  
  inline def join[DerivationType <: Product: ProductReflection]: TypeclassType[DerivationType]

erased trait FieldIndex[FieldType]

package derivationContext:
  given required: ContextRequirement.required.type = ContextRequirement.required
  given relaxed: ContextRequirement.relaxed.type = ContextRequirement.relaxed

object ContextRequirement:
  given required: ContextRequirement with
    type Optionality[Type] = Type
    type RequiredType = true
    def wrap[ValueType](optional: Optional[ValueType]): ValueType = optional.vouch(using Unsafe)
  
  object relaxed extends ContextRequirement:
    type Optionality[Type] = Optional[Type]
    type RequiredType = false
    def wrap[ValueType](optional: Optional[ValueType]): Optional[ValueType] = optional
  
trait ContextRequirement:
  type Optionality[Type] <: Optional[Type]
  type RequiredType <: Boolean
  def wrap[ValueType](optional: Optional[ValueType]): Optionality[ValueType]
