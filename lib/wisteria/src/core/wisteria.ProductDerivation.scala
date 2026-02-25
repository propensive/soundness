                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.54.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package wisteria

import scala.compiletime.*

import anticipation.*
import proscenium.*
import rudiments.*
import vacuous.*

object ProductDerivation:
  trait Methods[typeclass[_]]:
    protected transparent inline def construct[derivation <: Product]
      ( using reflection: ProductReflection[derivation], requirement: ContextRequirement )
      ( inline lambda:  [field] => requirement.Optionality[typeclass[field]]
                        =>  ( typeclass: requirement.Optionality[typeclass[field]],
                              default:   Default[Optional[field]],
                              label:     Text,
                              index:     Int & FieldIndex[field] ) ?=> field )
    :   derivation =

        type Fields = reflection.MirroredElemTypes
        type Labels = reflection.MirroredElemLabels

        reflection.fromProduct:
          fold[derivation, Fields, Labels, Tuple](Zero, 0): accumulator =>
            [field] => context ?=> lambda[field](context) *: accumulator

          . reverse


    protected transparent inline
    def constructWith[constructor[_]]
      ( using requirement: ContextRequirement )
      [ derivation <: Product ]
      ( using reflection: ProductReflection[derivation] )
      ( inline bind:  [input, output] => constructor[input] => (input => constructor[output])
                      =>  constructor[output],
        inline pure:    [monadic] => monadic => constructor[monadic],
        inline lambda:  [field] => requirement.Optionality[typeclass[field]]
                        =>  ( typeclass: requirement.Optionality[typeclass[field]],
                              default:   Default[Optional[field]],
                              label:     Text,
                              index:     Int & FieldIndex[field] )
                        ?=> constructor[field] )
    :   constructor[derivation] =

        type Fields = reflection.MirroredElemTypes
        type Labels = reflection.MirroredElemLabels

        val tuple: constructor[Tuple] =
          fold[derivation, Fields, Labels, constructor[Tuple]](pure(Zero), 0):
            accumulator => [field] => context ?=>
              bind(accumulator): accumulator2 =>
                bind(lambda[field](context)): result => pure(result *: accumulator2)

        bind(tuple): tuple => pure(reflection.fromProduct(tuple.reverse))


    protected transparent inline def contexts[derivation <: Product]
      ( using reflection:  ProductReflection[derivation], requirement: ContextRequirement )
      [ result ]
      ( inline lambda:  [field] => requirement.Optionality[typeclass[field]]
                        =>  ( typeclass:   requirement.Optionality[typeclass[field]],
                              default:     Default[Optional[field]],
                              label:       Text,
                              dereference: derivation => field,
                              index:       Int & FieldIndex[field] ) ?=> result )
    :   IArray[result] =

        type Fields = reflection.MirroredElemTypes
        type Labels = reflection.MirroredElemLabels

        provide[ClassTag[result]]:
          IArray.create[result](valueOf[Tuple.Size[Fields]]): array =>
            fold[derivation, Fields, Labels, Unit]((), 0): accumulator =>
              [field] => context ?=> array(index) = lambda[field](context)


    inline def typeName[derivation](using reflection: Reflection[derivation]): Text =
      valueOf[reflection.MirroredLabel].tt

    inline def tuple[derivation](using reflection: Reflection[derivation]): Boolean =
      compiletime.summonFrom:
        case given (reflection.MirroredMonoType <:< Tuple) => true
        case _                                             => false

    inline def singleton[derivation](using reflection: Reflection[derivation]): Boolean =
      compiletime.summonFrom:
        case given (reflection.MirroredMonoType <:< Singleton) => true
        case _                                                 => false

    protected transparent inline def complement[derivation <: Product, field]
      ( product: derivation )
      ( using fieldIndex:  Int & FieldIndex[field],
              reflection:  ProductReflection[derivation],
              requirement: ContextRequirement)
    :   field =

        type Labels = reflection.MirroredElemLabels
        type Fields = reflection.MirroredElemTypes
        val tuple: Fields = Tuple.fromProductTyped(product)

        fold[derivation, Fields, Labels, Optional[field]](tuple, Unset, 0):
          accumulator => [field2] => field =>
            if index == fieldIndex then field.asInstanceOf[field] else accumulator

        . vouch


    protected transparent inline def fields[derivation <: Product](inline product: derivation)
      ( using requirement: ContextRequirement )
      ( using reflection: ProductReflection[derivation] )
      [ result ]
      ( inline lambda:  [field] => field
                        =>  ( context: requirement.Optionality[typeclass[field]],
                              default: Default[Optional[field]],
                              label:   Text,
                              index:   Int & FieldIndex[field] ) ?=> result )
    :   IArray[result] =

        provide[ClassTag[result]]:
          type Labels = reflection.MirroredElemLabels
          type Fields = reflection.MirroredElemTypes
          val tuple: Fields = Tuple.fromProductTyped(product)

          IArray.create[result](tuple.size): array =>
            fold[derivation, Fields, Labels, Unit](tuple, (), 0): unit =>
              [field] => field =>
                given typeclass: requirement.Optionality[typeclass[field]] =
                  requirement.wrap(context)

                array(index) = lambda[field](field)


    // The two implementations of `fold` are very similar. We would prefer to have a single
    // implementation (closer to the non-erased `fold`), but it's difficult to abstract over the
    // erasedness of the tuple.

    private transparent inline def fold
      [ derivation <: Product, fields <: Tuple, labels <: Tuple, result ]
      ( using requirement: ContextRequirement )
      ( inline tuple: fields, accumulator: result, index: Int )
      ( inline lambda:  result => [field] => field
                        =>  ( context: Optional[typeclass[field]],
                              default: Default[Optional[field]],
                              label:   Text,
                              index:   Int & FieldIndex[field] ) ?=> result )
    :   result =

        inline tuple match
          case Zero => accumulator

          case tuple: (fieldType *: moreFields) => tuple match
            case field *: moreFields => inline !![labels] match
              case _: (label *: moreLabels) => inline valueOf[label].asMatchable match
                case label: String =>
                  val typeclass = requirement.summon[typeclass[fieldType]]

                  val fieldIndex: Int & FieldIndex[fieldType] =
                    index.asInstanceOf[Int & FieldIndex[fieldType]]

                  val default = Default(Wisteria.default[derivation, fieldType](index))

                  val accumulator2 =
                    lambda(accumulator)[fieldType](field)
                      ( using typeclass, default, label.tt, fieldIndex )

                  fold[derivation, moreFields, moreLabels, result]
                    ( moreFields, accumulator2, index + 1 )
                    ( lambda )


    private transparent inline def fold
      [ derivation <: Product, fields <: Tuple, labels <: Tuple, result ]
      ( using requirement: ContextRequirement )
      ( inline accumulator: result, index: Int )
      ( inline lambda:  result => [field] => requirement.Optionality[typeclass[field]]
                        =>  ( default:     Default[Optional[field]],
                              label:       Text,
                              dereference: derivation => field,
                              index:       Int & FieldIndex[field] ) ?=> result )
    :   result =

        inline !![fields] match
          case _: Zero => accumulator

          case _: (fieldType *: moreFields) => inline !![labels] match
            case _: (label *: moreLabels) => inline valueOf[label].asMatchable match
              case label: String =>
                val typeclass = requirement.summon[typeclass[fieldType]]

                val fieldIndex: Int & FieldIndex[fieldType] =
                  index.asInstanceOf[Int & FieldIndex[fieldType]]

                val default = Default(Wisteria.default[derivation, fieldType](index))

                val dereference: derivation => fieldType =
                  _.productElement(fieldIndex).asInstanceOf[fieldType]

                val accumulator2 =
                  lambda(accumulator)[fieldType](typeclass)
                    ( using default, label.tt, dereference, fieldIndex )

                fold[derivation, moreFields, moreLabels, result](accumulator2, index + 1)(lambda)


    inline def join[derivation <: Product: ProductReflection]: typeclass[derivation]

trait ProductDerivation[typeclass[_]] extends ProductDerivation.Methods[typeclass]:
  inline given derived: [derivation] => Reflection[derivation] => typeclass[derivation] =
    inline summon[Reflection[derivation]] match
      case reflection: ProductReflection[derivationType] =>
        join[derivationType](using reflection).asMatchable match
          case typeclass: typeclass[`derivation`] => typeclass
