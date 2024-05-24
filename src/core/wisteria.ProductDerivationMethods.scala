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
  protected transparent inline def construct[DerivationType <: Product]
      (using reflection: ProductReflection[DerivationType], requirement: ContextRequirement)
      (inline lambda: [FieldType] =>
                          requirement.Optionality[TypeclassType[FieldType]] =>
                              (typeclass: requirement.Optionality[TypeclassType[FieldType]],
                               default:   Default[Optional[FieldType]],
                               label:     Text,
                               index:     Int & FieldIndex[FieldType]) ?=>
                                  FieldType)
          : DerivationType =

    type Fields = reflection.MirroredElemTypes
    type Labels = reflection.MirroredElemLabels

    reflection.fromProduct:
      fold[DerivationType, Fields, Labels, Tuple](EmptyTuple, 0): accumulator =>
        [FieldType] => context ?=> lambda[FieldType](context) *: accumulator
      .reverse

  // WIP
  protected transparent inline def constructWith[DerivationType <: Product, F[_]]
      (using reflection: ProductReflection[DerivationType], requirement: ContextRequirement)
      (inline flatMap: [A, B] => F[A] => (A => F[B]) => F[B],
       inline pure: [T] => T => F[T],
       inline lambda: [FieldType] =>
                          requirement.Optionality[TypeclassType[FieldType]] =>
                              (typeclass: requirement.Optionality[TypeclassType[FieldType]],
                               default:   Default[Optional[FieldType]],
                               label:     Text,
                               index:     Int & FieldIndex[FieldType]) ?=>
                                  F[FieldType])
          : F[DerivationType] =

    type Fields = reflection.MirroredElemTypes
    type Labels = reflection.MirroredElemLabels

    val resultingTuple: F[Tuple] = fold[DerivationType, Fields, Labels, F[Tuple]](pure(EmptyTuple), 0): accumulator =>
      [FieldType] => context ?=>
        flatMap[FieldType, Tuple](lambda[FieldType](context)) { (result: FieldType) =>
          flatMap[Tuple, Tuple](accumulator)((acc: Tuple) => pure(result *: acc))
        }

    flatMap[Tuple, DerivationType](resultingTuple)((v: Tuple) => pure(reflection.fromProduct(v.reverse)))

  protected transparent inline def constructWithV2[DerivationType <: Product, F[_]]
      (using reflection: ProductReflection[DerivationType], requirement: ContextRequirement)
      (inline pure: [T] => T => F[T],
       inline lambda: [FieldType] =>
                          requirement.Optionality[TypeclassType[FieldType]] =>
                              (typeclass: requirement.Optionality[TypeclassType[FieldType]],
                               default:   Default[Optional[FieldType]],
                               label:     Text,
                               index:     Int & FieldIndex[FieldType],
                               specify:   FieldType => F[FieldType]) ?=>
                                  F[FieldType])
          : F[DerivationType] =

    type Fields = reflection.MirroredElemTypes
    type Labels = reflection.MirroredElemLabels

    val foldResult = fold[DerivationType, Fields, Labels, Either[F[DerivationType], Tuple]](Right(EmptyTuple), 0): accumulator =>
        [FieldType] => context ?=> 
          var specified: Option[FieldType] = None
          // TODO: can this be a given?
          // type Specify = FieldType => F[FieldType]
          // given specify: Specify = value =>
          implicit val specify: FieldType => F[FieldType] = value =>
            specified = Some(value)
            pure(value)

          accumulator match
            case Left(value) => Left(value)
            case Right(accumulator) => 
              val lambdaResult = lambda[FieldType](context)
              specified match
                case None => 
                  Left(lambdaResult.asInstanceOf[F[DerivationType]])
                case Some(result) =>
                  specified = None
                  Right(result *: accumulator)

    foldResult match {
      case Left(err) => err
      case Right(value) => pure(reflection.fromProduct(value.reverse))
    }

  protected transparent inline def contexts[DerivationType <: Product]
      (using reflection: ProductReflection[DerivationType], requirement: ContextRequirement)
      [ResultType]
      (inline lambda: [FieldType] =>
                          requirement.Optionality[TypeclassType[FieldType]] =>
                              (typeclass:   requirement.Optionality[TypeclassType[FieldType]],
                               default:     Default[Optional[FieldType]],
                               label:       Text,
                               dereference: DerivationType => FieldType,
                               index:       Int & FieldIndex[FieldType]) ?=>
                                  ResultType)
          : IArray[ResultType] =

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
          : FieldType =

    type Labels = reflection.MirroredElemLabels
    type Fields = reflection.MirroredElemTypes
    val tuple: Fields = Tuple.fromProductTyped(product)

    fold[DerivationType, Fields, Labels, Optional[FieldType]](tuple, Unset, 0):
      accumulator => [FieldType2] => field =>
        if index == fieldIndex then field.asInstanceOf[FieldType] else accumulator
    .vouch(using Unsafe)

  protected transparent inline def fields[DerivationType <: Product](inline product: DerivationType)
      (using requirement: ContextRequirement)
      (using reflection: ProductReflection[DerivationType])
      [ResultType]
      (inline lambda: [FieldType] =>
                          FieldType =>
                              (context: requirement.Optionality[TypeclassType[FieldType]],
                               default: Default[Optional[FieldType]],
                               label:   Text,
                               index:   Int & FieldIndex[FieldType]) ?=>
                                  ResultType)
          : IArray[ResultType] =

    summonInline[ClassTag[ResultType]].give:
      type Labels = reflection.MirroredElemLabels
      type Fields = reflection.MirroredElemTypes
      val tuple: Fields = Tuple.fromProductTyped(product)

      IArray.create[ResultType](tuple.size): array =>
        fold[DerivationType, Fields, Labels, Unit](tuple, (), 0): unit =>
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
                                   index:   Int & FieldIndex[FieldType]) ?=>
                                      ResultType)
          : ResultType =

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
                                  (default:     Default[Optional[FieldType]],
                                   label:       Text,
                                   dereference: DerivationType => FieldType,
                                   index:       Int & FieldIndex[FieldType]) ?=>
                                      ResultType)
          : ResultType =

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

  private transparent inline def foldWith
    [DerivationType <: Product, FieldsType <: Tuple, LabelsType <: Tuple, ResultType, F[_]]
    (using requirement: ContextRequirement)
    (inline accumulator: F[ResultType], index: Int)
    (inline lambda: F[ResultType] =>
                        [FieldType] =>
                            requirement.Optionality[TypeclassType[FieldType]] =>
                                (default:     Default[Optional[FieldType]],
                                 label:       Text,
                                 dereference: DerivationType => FieldType,
                                 index:       Int & FieldIndex[FieldType]) ?=>
                                    F[ResultType])
        : F[ResultType] =

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

          foldWith[DerivationType, moreFieldsType, moreLabelsType, ResultType, F]
            (accumulator2, index + 1)
            (lambda)
              
          

          

  inline def join[DerivationType <: Product: ProductReflection]: TypeclassType[DerivationType]
