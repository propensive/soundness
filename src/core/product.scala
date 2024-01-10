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
      (inline lambda: [FieldType] => TypeclassType[FieldType] => (
        typeclass: TypeclassType[FieldType],
        default: Default[Optional[FieldType]],
        label: Text,
        index: Int & FieldIndex[FieldType]
          ) ?=> FieldType)
      : DerivationType =
    
    reflection.fromProduct:
      fold[DerivationType, reflection.MirroredElemTypes, reflection.MirroredElemLabels, Tuple](EmptyTuple, 0):
        accumulator => [FieldType] => context =>
          val context2 = context.asInstanceOf[TypeclassType[FieldType]]
          lambda[FieldType](context2)(using context2, default, label, index) *: accumulator
      .reverse

  protected transparent inline def contexts
      [DerivationType <: Product]
      (using reflection: ProductReflection[DerivationType], requirement: ContextRequirement)
      [ResultType]
      (inline lambda: [FieldType] => requirement.Optionality[TypeclassType[FieldType]] => (
        typeclass: requirement.Optionality[TypeclassType[FieldType]],
        default:   Default[Optional[FieldType]],
        label:     Text,
        index:     Int & FieldIndex[FieldType]
          ) ?=> ResultType)
      : IArray[ResultType] =
    
    summonInline[ClassTag[ResultType]].contextually:
      IArray.create[ResultType](valueOf[Tuple.Size[reflection.MirroredElemTypes]]): array =>
        fold[DerivationType, reflection.MirroredElemTypes, reflection.MirroredElemLabels, Unit]((), 0):
          accumulator => [FieldType] => context =>
            array(index) = lambda[FieldType](context)(using context, default, label, index)
  
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
    [DerivationType <: Product, FieldType]
    (product: DerivationType)
    (using
      fieldIndex: Int & FieldIndex[FieldType],
      reflection: ProductReflection[DerivationType],
      requirement: ContextRequirement
        )
    : FieldType =
    
    type Labels = reflection.MirroredElemLabels
    type Fields = reflection.MirroredElemTypes
    val tuple: Fields = Tuple.fromProductTyped(product)
    
    fold[DerivationType, Fields, Labels, Optional[FieldType]](tuple, Unset, 0):
      accumulator => [FieldType2] => field =>
        if index == fieldIndex then field.asInstanceOf[FieldType] else accumulator
    .vouch(using Unsafe)

  protected transparent inline def fields
      [DerivationType <: Product]
      (inline product: DerivationType)
      (using requirement: ContextRequirement)
      (using reflection: ProductReflection[DerivationType])
      [ResultType]
      (inline lambda: [FieldType] => FieldType => (
        context: requirement.Optionality[TypeclassType[FieldType]],
        default: Default[Optional[FieldType]],
        label: Text,
        index: Int & FieldIndex[FieldType]
          ) ?=> ResultType)
      : IArray[ResultType] =
    
    summonInline[ClassTag[ResultType]].contextually:
      type Labels = reflection.MirroredElemLabels
      type Fields = reflection.MirroredElemTypes
      val tuple: Fields = Tuple.fromProductTyped(product)
      
      IArray.create[ResultType](tuple.size): array =>
        fold[DerivationType, Fields, Labels, Array[ResultType]](tuple, array, 0):
          array => [FieldType] => field =>
            
            val typeclass: requirement.Optionality[TypeclassType[FieldType]] = requirement.wrap(context)
            array(index) = lambda[FieldType](field)(using typeclass, default, label, index)
            array

  // The two implementations of `fold` are very similar. We would prefer to have a single implementation (closer
  // to the non-erased `fold`), but it's difficult to abstract over the erasedness of the tuple.

  private transparent inline def fold
      [DerivationType, FieldsType <: Tuple, LabelsType <: Tuple, ResultType]
      (using requirement: ContextRequirement)
      (inline tuple: FieldsType, accumulator: ResultType, index: Int)
      (inline lambda: ResultType => [FieldType] => FieldType => (
        context: Optional[TypeclassType[FieldType]],
        default: Default[Optional[FieldType]],
        label: Text,
        index: Int & FieldIndex[FieldType]
          ) ?=> ResultType)
      : ResultType =

    inline tuple match
      case EmptyTuple =>
        accumulator
      
      case tuple: (fieldType *: moreFieldsType) => tuple match
        case field *: moreFields => inline erasedValue[LabelsType] match
          case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
            case label: String =>
              val typeclass = requirement.summon[TypeclassType[fieldType]]
              val fieldIndex: Int & FieldIndex[fieldType] = index.asInstanceOf[Int & FieldIndex[fieldType]]
              val default = Wisteria.default[DerivationType, fieldType](index)
              
              val accumulator2 = lambda(accumulator)[fieldType](field)(using typeclass, Default(default), label.tt,
                  fieldIndex)
              
              fold[DerivationType, moreFieldsType, moreLabelsType, ResultType](moreFields, accumulator2, index + 1)(lambda)

  private transparent inline def fold
      [DerivationType, FieldsType <: Tuple, LabelsType <: Tuple, ResultType]
      (using requirement: ContextRequirement)
      (inline accumulator: ResultType, index: Int)
      (inline lambda: ResultType => [FieldType] => requirement.Optionality[TypeclassType[FieldType]] => (
        default: Default[Optional[FieldType]],
        label: Text,
        index: Int & FieldIndex[FieldType]
          ) ?=> ResultType)
      : ResultType =

    inline erasedValue[FieldsType] match
      case _: EmptyTuple =>
        accumulator
      
      case _: (fieldType *: moreFieldsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
          case label: String =>
            val typeclass = requirement.summon[TypeclassType[fieldType]]
            val fieldIndex: Int & FieldIndex[fieldType] = index.asInstanceOf[Int & FieldIndex[fieldType]]
            val default = Wisteria.default[DerivationType, fieldType](index)
            val accumulator2 = lambda(accumulator)[fieldType](typeclass)(using Default(default), label.tt, fieldIndex)
            
            fold[DerivationType, moreFieldsType, moreLabelsType, ResultType](accumulator2, index + 1)
                (lambda)
  
  inline def join[DerivationType <: Product: ProductReflection]: TypeclassType[DerivationType]

erased trait FieldIndex[FieldType]

package derivationContext:
  given required: ContextRequirement.required.type = ContextRequirement.required
  given relaxed: ContextRequirement.relaxed.type = ContextRequirement.relaxed

object ContextRequirement:
  given required: ContextRequirement with
    type Optionality[Type] = Type
    type Required = true
    def wrap[ValueType](optional: Optional[ValueType]): ValueType = optional.vouch(using Unsafe)
  
  object relaxed extends ContextRequirement:
    type Optionality[Type] = Optional[Type]
    type Required = false
    def wrap[ValueType](optional: Optional[ValueType]): Optional[ValueType] = optional
  
trait ContextRequirement:
  type Optionality[Type] <: Optional[Type]
  type Required <: Boolean
  def wrap[ValueType](optional: Optional[ValueType]): Optionality[ValueType]
  
  inline def summon[ContextualType]: Optionality[ContextualType] =
    inline if erasedValue[Required] then wrap(summonInline[ContextualType]) else summonFrom:
      case contextual: ContextualType => wrap(contextual)
      case _                          => wrap(Unset)
