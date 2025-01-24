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
import rudiments.*
import vacuous.*

import scala.deriving.*
import scala.compiletime.*

trait ProductDerivationMethods[TypeclassType[_]]:
  protected transparent inline def construct[DerivationType <: Product]
     (using reflection: ProductReflection[DerivationType], requirement: ContextRequirement)
     (inline lambda: [FieldType] =>
                          requirement.Optionality[TypeclassType[FieldType]] =>
                           (typeclass: requirement.Optionality[TypeclassType[FieldType]],
                            default:   Default[Optional[FieldType]],
                            label:    Text,
                            index:    Int & FieldIndex[FieldType]) ?=> FieldType)
  :     DerivationType =

    type Fields = reflection.MirroredElemTypes
    type Labels = reflection.MirroredElemLabels

    reflection.fromProduct:
      fold[DerivationType, Fields, Labels, Tuple](EmptyTuple, 0): accumulator =>
        [FieldType] => context ?=> lambda[FieldType](context) *: accumulator

      . reverse

  protected transparent inline def constructWith[ConstructorType[_]]
     (using requirement: ContextRequirement)
     [DerivationType <: Product]
     (using reflection: ProductReflection[DerivationType])
     (inline bind:   [InputType, OutputType] => ConstructorType[InputType] =>
                       (InputType => ConstructorType[OutputType]) => ConstructorType[OutputType],
      inline pure:   [MonadicType] => MonadicType => ConstructorType[MonadicType],
      inline lambda: [FieldType] =>
                       requirement.Optionality[TypeclassType[FieldType]] =>
                        (typeclass: requirement.Optionality[TypeclassType[FieldType]],
                         default:   Default[Optional[FieldType]],
                         label:    Text,
                         index:    Int & FieldIndex[FieldType]) ?=>
                           ConstructorType[FieldType])
  :     ConstructorType[DerivationType] =

    type Fields = reflection.MirroredElemTypes
    type Labels = reflection.MirroredElemLabels

    val tuple: ConstructorType[Tuple] =
      fold[DerivationType, Fields, Labels, ConstructorType[Tuple]](pure(EmptyTuple), 0):
        accumulator =>
          [FieldType] => context ?=>
            bind(accumulator): accumulator2 =>
              bind(lambda[FieldType](context)): result =>
                pure(result *: accumulator2)

    bind(tuple): tuple =>
      pure(reflection.fromProduct(tuple.reverse))

  protected transparent inline def contexts[DerivationType <: Product]
     (using reflection: ProductReflection[DerivationType], requirement: ContextRequirement)
     [ResultType]
     (inline lambda: [FieldType] =>
                       requirement.Optionality[TypeclassType[FieldType]] =>
                        (typeclass:   requirement.Optionality[TypeclassType[FieldType]],
                         default:    Default[Optional[FieldType]],
                         label:      Text,
                         dereference: DerivationType => FieldType,
                         index:      Int & FieldIndex[FieldType]) ?=> ResultType)
  :     IArray[ResultType] =

    type Fields = reflection.MirroredElemTypes
    type Labels = reflection.MirroredElemLabels

    summonInline[ClassTag[ResultType]].give:
      IArray.create[ResultType](valueOf[Tuple.Size[Fields]]): array =>
        fold[DerivationType, Fields, Labels, Unit]((), 0): accumulator =>
          [FieldType] => context ?=> array(index) = lambda[FieldType](context)

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

  protected transparent inline def complement[DerivationType <: Product, FieldType]
     (product: DerivationType)
     (using fieldIndex:  Int & FieldIndex[FieldType],
            reflection:  ProductReflection[DerivationType],
            requirement: ContextRequirement)
  :     FieldType =

    type Labels = reflection.MirroredElemLabels
    type Fields = reflection.MirroredElemTypes
    val tuple: Fields = Tuple.fromProductTyped(product)

    fold[DerivationType, Fields, Labels, Optional[FieldType]](tuple, Unset, 0):
      accumulator => [FieldType2] => field =>
        if index == fieldIndex then field.asInstanceOf[FieldType] else accumulator

    . vouch

  protected transparent inline def fields[DerivationType <: Product](inline product: DerivationType)
     (using requirement: ContextRequirement)
     (using reflection: ProductReflection[DerivationType])
     [ResultType]
     (inline lambda: [FieldType] =>
                       FieldType =>
                        (context: requirement.Optionality[TypeclassType[FieldType]],
                         default: Default[Optional[FieldType]],
                         label:   Text,
                         index:   Int & FieldIndex[FieldType]) ?=> ResultType)
  :     IArray[ResultType] =

    summonInline[ClassTag[ResultType]].give:
      type Labels = reflection.MirroredElemLabels
      type Fields = reflection.MirroredElemTypes
      val tuple: Fields = Tuple.fromProductTyped(product)

      IArray.create[ResultType](tuple.size):
        array =>
          fold[DerivationType, Fields, Labels, Unit](tuple, (), 0):
            unit =>
              [FieldType] => field =>
                given typeclass: requirement.Optionality[TypeclassType[FieldType]] =
                  requirement.wrap(context)

                array(index) = lambda[FieldType](field)

  // The two implementations of `fold` are very similar. We would prefer to have a single
  // implementation (closer to the non-erased `fold`), but it's difficult to abstract over the
  // erasedness of the tuple.

  private transparent inline def fold
     [DerivationType <: Product, FieldsType <: Tuple, LabelsType <: Tuple, ResultType]
     (using requirement: ContextRequirement)
     (inline tuple: FieldsType, accumulator: ResultType, index: Int)
     (inline lambda: ResultType =>
                         [FieldType] =>
                           FieldType =>
                            (context: Optional[TypeclassType[FieldType]],
                             default: Default[Optional[FieldType]],
                             label:   Text,
                             index:   Int & FieldIndex[FieldType]) ?=> ResultType)
  :     ResultType =

    inline tuple match
      case EmptyTuple => accumulator

      case tuple: (fieldType *: moreFieldsType) => tuple match
        case field *: moreFields => inline erasedValue[LabelsType] match
          case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
            case label: String =>
              val typeclass = requirement.summon[TypeclassType[fieldType]]

              val fieldIndex: Int & FieldIndex[fieldType] =
                index.asInstanceOf[Int & FieldIndex[fieldType]]

              val default = Default(Wisteria.default[DerivationType, fieldType](index))

              val accumulator2 =
                lambda(accumulator)[fieldType](field)
                 (using typeclass, default, label.tt, fieldIndex)

              fold
                [DerivationType, moreFieldsType, moreLabelsType, ResultType]
                (moreFields, accumulator2, index + 1)
                (lambda)

  private transparent inline def fold
     [DerivationType <: Product, FieldsType <: Tuple, LabelsType <: Tuple, ResultType]
     (using requirement: ContextRequirement)
     (inline accumulator: ResultType, index: Int)
     (inline lambda: ResultType =>
                      [FieldType] =>
                        requirement.Optionality[TypeclassType[FieldType]] =>
                         (default:    Default[Optional[FieldType]],
                          label:      Text,
                          dereference: DerivationType => FieldType,
                          index:      Int & FieldIndex[FieldType]) ?=> ResultType)
  :     ResultType =

    inline erasedValue[FieldsType] match
      case _: EmptyTuple => accumulator

      case _: (fieldType *: moreFieldsType) => inline erasedValue[LabelsType] match
        case _: (labelType *: moreLabelsType) => inline valueOf[labelType].asMatchable match
          case label: String =>
            val typeclass = requirement.summon[TypeclassType[fieldType]]

            val fieldIndex: Int & FieldIndex[fieldType] =
              index.asInstanceOf[Int & FieldIndex[fieldType]]

            val default = Default(Wisteria.default[DerivationType, fieldType](index))

            val dereference: DerivationType => fieldType =
              _.productElement(fieldIndex).asInstanceOf[fieldType]

            val accumulator2 =
              lambda(accumulator)[fieldType](typeclass)
               (using default, label.tt, dereference, fieldIndex)

            fold[DerivationType, moreFieldsType, moreLabelsType, ResultType]
             (accumulator2, index + 1)
             (lambda)

  inline def join[DerivationType <: Product: ProductReflection]: TypeclassType[DerivationType]
