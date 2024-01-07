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
  protected transparent inline def product
      [DerivationType]
      (using reflection: ProductReflection[DerivationType])
      (inline lambda: [FieldType] => TypeclassType[FieldType] => (label: Text, index: Int & FieldIndex[FieldType]) ?=> FieldType) =
    
    reflection.fromProduct:
      fold[DerivationType, reflection.MirroredElemLabels, Tuple]
          (erasedValue[reflection.MirroredElemTypes], EmptyTuple, 0, false):
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

  protected transparent inline def certainTypeclass
      [DerivationType, FieldType]
      (product: DerivationType)
      (using fieldIndex: Int & FieldIndex[FieldType], reflection: ProductReflection[DerivationType])
      : Optional[TypeclassType[FieldType]] =
    type Labels = reflection.MirroredElemLabels
    
    inline product.asMatchable match
      case product: Product => inline reflection match
        case given ProductReflection[DerivationType & Product] =>
          fold[DerivationType, Labels, Any](Tuple.fromProductTyped(product), Unset, 0, true):
            accumulator => [FieldType2] => field =>
              if index == fieldIndex then typeclass else accumulator
          .asInstanceOf[Option[TypeclassType[FieldType]]].get

  protected transparent inline def optionalTypeclass
      [DerivationType, FieldType]
      (product: DerivationType)
      (using fieldIndex: Int & FieldIndex[FieldType], reflection: ProductReflection[DerivationType])
      : Optional[TypeclassType[FieldType]] =
    
    type Labels = reflection.MirroredElemLabels
    
    inline product.asMatchable match
      case product: Product => inline reflection match
        case given ProductReflection[DerivationType & Product] =>
          fold[DerivationType, Labels, Optional[Any]](Tuple.fromProductTyped(product), Unset, 0, false):
            accumulator => [FieldType2] => field =>
              if index == fieldIndex then
                typeclass.getOrElse(Unset) else accumulator
          .asInstanceOf[Optional[TypeclassType[FieldType]]]

  protected transparent inline def correspondent
      [DerivationType, FieldType]
      (product: DerivationType)
      (using fieldIndex: Int & FieldIndex[FieldType], reflection: ProductReflection[DerivationType])
      : FieldType =
    
    type Labels = reflection.MirroredElemLabels
    
    inline product.asMatchable match
      case product: Product => inline reflection match
        case given ProductReflection[DerivationType & Product] =>
          fold[DerivationType, Labels, Optional[Any]](Tuple.fromProductTyped(product), Unset, 0, false):
            accumulator => [FieldType2] => field => if index == fieldIndex then field else accumulator
          .asInstanceOf[FieldType]

  protected transparent inline def fields
      [DerivationType]
      (inline product: DerivationType)
      (using reflection: ProductReflection[DerivationType])
      [ResultType]
      (inline lambda: [FieldType] => FieldType =>
          (typeclass: Option[TypeclassType[FieldType]], label: Text, index: Int & FieldIndex[FieldType]) ?=> ResultType)
      : IArray[ResultType] =
    
    summonInline[ClassTag[ResultType]].contextually:
      inline product.asMatchable match
        case product: Product => inline reflection match
          case given ProductReflection[DerivationType & Product] =>
            val array: Array[ResultType] = new Array(valueOf[Tuple.Size[reflection.MirroredElemTypes]])
            type Labels = reflection.MirroredElemLabels

            fold[DerivationType, Labels, Array[ResultType]](Tuple.fromProductTyped(product), array, 0, false):
              accumulator => [FieldType] => field =>

                accumulator(index) = lambda[FieldType](field)
                accumulator

            .immutable(using Unsafe)

  private transparent inline def fold
      [DerivationType, LabelsType <: Tuple, AccumulatorType]
      (inline tuple: Tuple, accumulator: AccumulatorType, index: Int, required: Boolean)
      (inline lambda: AccumulatorType => [FieldType] => FieldType =>
          (typeclass: Option[TypeclassType[FieldType]], label: Text, index: Int & FieldIndex[FieldType]) ?=>
          AccumulatorType)
      : AccumulatorType =

    inline tuple match
      case EmptyTuple =>
        accumulator
      
      case cons: (fieldType *: moreFieldsType) => cons match
        case field *: moreFields => inline erasedValue[LabelsType] match
          case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
            case label: String =>
              val typeclass = inline if required then Some(summonInline[TypeclassType[`fieldType`]]) else
                summonFrom:
                  case typeclass: TypeclassType[`fieldType`] => Some(typeclass)
                  case _                                     => None
              val fieldIndex: Int & FieldIndex[fieldType] = index.asInstanceOf[Int & FieldIndex[fieldType]]
              val accumulator2 = lambda(accumulator)[fieldType](field)(using typeclass, label.tt, fieldIndex)
              
              fold[DerivationType, moreLabelsType, AccumulatorType](moreFields, accumulator2, index + 1, required)(lambda)

  inline def join[DerivationType: ProductReflection]: TypeclassType[DerivationType]

transparent erased trait FieldIndex[FieldType]