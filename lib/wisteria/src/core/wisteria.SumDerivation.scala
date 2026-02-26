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
import scala.deriving.*

import anticipation.*
import contingency.*
import fulminate.*
import proscenium.*
import rudiments.*
import vacuous.*

object SumDerivation:
  trait Methods[typeclass[_]]:

    @deprecated("This method has been renamed to `choice`")
    transparent inline def allSingletons[derivation: SumReflection]: Boolean = choice[derivation]

    transparent inline def choice[derivation: SumReflection]: Boolean =
      inline !![derivation.MirroredElemTypes] match
        case _: (variant *: variants) => all[variant, variants]

    private transparent inline def all[variant, variants <: Tuple]: Boolean = summonFrom:
      case given (variant <:< Singleton) =>
        inline !![variants] match
          case _: Zero                           => true
          case _: (variant *: variants)  => all[variant, variants]

      case _ =>
        false


    protected transparent inline def complement[derivation, variant](sum: derivation)
      ( using variantIndex: Int & VariantIndex[variant],
              reflection:   SumReflection[derivation] )
    :   Optional[variant] =

      type Labels = reflection.MirroredElemLabels
      type Variants = reflection.MirroredElemTypes

      val size: Int = valueOf[Tuple.Size[reflection.MirroredElemTypes]]

      fold[derivation, Variants, Labels](sum, size, 0, false)(index == reflection.ordinal(sum)):
        [variant2 <: derivation] => field =>
          if index == variantIndex then field.asInstanceOf[variant] else Unset


    protected inline def variantLabels[derivation](using reflection: SumReflection[derivation])
    :   List[Text] =

      constValueTuple[reflection.MirroredElemLabels].toList.map(_.toString.tt)


    protected transparent inline def singleton[derivation](input: Text)
      ( using reflection: SumReflection[derivation] )
    :   derivation =

      type Variants = reflection.MirroredElemTypes
      type Labels = reflection.MirroredElemLabels

      singletonFold[derivation, Variants, Labels](_ == input).or:
        provide[Tactic[VariantError]]:
          abort(VariantError[derivation](input))


    private transparent inline def singletonFold[derivation, variants <: Tuple, labels <: Tuple]
      ( using reflection: SumReflection[derivation] )
      ( predicate: Text => Boolean )
    :   Optional[derivation] =

      inline !![variants] match
        case _: (variant *: variants) =>
          inline !![labels] match
            case _: (label *: labelsType) =>
              type variant0 = variant & derivation

              if predicate(valueOf[label & String].tt)
              then infer[Mirror.ProductOf[variant0]].fromProduct(Zero)
              else singletonFold[derivation, variants, labelsType](predicate)

        case _ =>
          Unset


    protected transparent inline def delegate[derivation](label: Text)
      ( using reflection: SumReflection[derivation], requirement: ContextRequirement )
      [ result ]
      ( inline lambda:  [variant <: derivation] => requirement.Optionality[typeclass[variant]]
                        =>  ( context: requirement.Optionality[typeclass[variant]],
                              label:   Text,
                              index:   Int & VariantIndex[variant] ) ?=> result )
    :   result =

      type Labels = reflection.MirroredElemLabels
      type Variants = reflection.MirroredElemTypes

      val size: Int = valueOf[Tuple.Size[reflection.MirroredElemTypes]]
      val variantLabel = label

      // Here label comes from context of fold's predicate
      fold[derivation, Variants, Labels](variantLabel, size, 0, true)(label == variantLabel):
        [variant <: derivation] => context => lambda[variant](context)

      . vouch


    protected transparent inline def variant[derivation](sum: derivation)
      ( using reflection:  SumReflection[derivation], requirement: ContextRequirement )
      [ result ]
      ( inline lambda:  [variant <: derivation]
                        =>  variant
                        =>  ( context: requirement.Optionality[typeclass[variant]],
                              label:   Text,
                              index:   Int & VariantIndex[variant] ) ?=> result )
    :   result =

      type Labels = reflection.MirroredElemLabels
      type Variants = reflection.MirroredElemTypes

      val size: Int = valueOf[Tuple.Size[reflection.MirroredElemTypes]]

      fold[derivation, Variants, Labels](sum, size, 0, false)(index == reflection.ordinal(sum)):
        [variant <: derivation] => variant => lambda[variant](variant)

      . vouch


    private transparent inline def fold[derivation, variants <: Tuple, labels <: Tuple]
      ( inline inputLabel: Text, size: Int, index: Int, fallible: Boolean )
      ( using reflection: SumReflection[derivation], requirement: ContextRequirement )
      ( inline predicate: (label: Text, index: Int & VariantIndex[derivation]) ?=> Boolean )
      [ result ]
      ( inline lambda:  [variant <: derivation]
                        =>  requirement.Optionality[typeclass[variant]]
                        =>  ( context: requirement.Optionality[typeclass[variant]],
                              label:   Text,
                              index:   Int & VariantIndex[variant] ) ?=> result )
    :   Optional[result] =

      inline !![variants] match
        case _: (variant *: variants) =>
          inline !![labels] match
            case _: (label *: moreLabels) =>
              type variant0 = variant & derivation
              if index >= size then Unset else
                (valueOf[label].asMatchable: @unchecked) match
                  case label: String =>
                    val index2: Int & VariantIndex[derivation] = VariantIndex[derivation](index)

                    if predicate(using label.tt, index2)
                    then
                      val index3: Int & VariantIndex[variant0] = VariantIndex[variant0](index)
                      val context = requirement.wrap(infer[typeclass[variant0]])
                      lambda[variant0](context)(using context, label.tt, index3)
                    else
                      fold
                        [ derivation, variants, moreLabels ]
                        ( inputLabel, size, index + 1, fallible )
                        ( predicate )
                        ( lambda )

        case _ =>
          inline if fallible
          then provide[Tactic[VariantError]]:
            raise(VariantError[derivation](inputLabel)) yet Unset
          else panic(m"Should be unreachable")


    private transparent inline def fold[derivation, variants <: Tuple, labels <: Tuple]
      ( inline sum: derivation, size: Int, index: Int, fallible: Boolean )
      ( using reflection:  SumReflection[derivation], requirement: ContextRequirement )
      ( inline predicate: (label: Text, index: Int & VariantIndex[derivation]) ?=> Boolean )
      [ result ]
      ( inline lambda:  [variant <: derivation]
                        =>  variant
                        =>  ( context: requirement.Optionality[typeclass[variant]],
                              label: Text,
                              index: Int & VariantIndex[variant] ) ?=> result )
    :   Optional[result] =

      inline !![variants] match
        case _: (variant *: variants) =>
          inline !![labels] match
            case _: (label *: moreLabels) =>
              type variant0 = variant & derivation
              if index >= size then Unset else
                (valueOf[label].asMatchable: @unchecked) match
                  case label: String =>
                    val index2: Int & VariantIndex[derivation] = VariantIndex[derivation](index)

                    if predicate(using label.tt, index2)
                    then
                      val index3: Int & VariantIndex[variant0] = VariantIndex[variant0](index)
                      val variant: variant0 = sum.asInstanceOf[variant0]
                      val context = requirement.wrap(infer[typeclass[variant0]])
                      lambda[variant0](variant)(using context, label.tt, index3)
                    else
                      fold[derivation, variants, moreLabels](sum, size, index + 1, fallible)
                        ( predicate )
                        ( lambda )

        case _ =>
          inline if fallible
          then provide[Tactic[VariantError]](raise(VariantError[derivation]("".tt)) yet Unset)
          else panic(m"Should be unreachable")


    inline def split[derivation: SumReflection]: typeclass[derivation]

trait SumDerivation[typeclass[_]] extends SumDerivation.Methods[typeclass]:
  inline given derived: [derivation] => Reflection[derivation] => typeclass[derivation] =
    inline summon[Reflection[derivation]] match
      case reflection: SumReflection[derivationType] =>
        split[derivationType](using reflection).asMatchable match
          case typeclass: typeclass[`derivation`] => typeclass
