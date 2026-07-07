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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import scala.deriving.Mirror
import scala.reflect.ClassTag

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import rudiments.*
import vacuous.*

object SumDerivation:
  trait Methods[typeclass[_]]:

    transparent inline def choice[derivation]: Boolean =
      ${wisteria.internal.isChoice[derivation]}


    protected transparent inline def complement[derivation, variant](sum: derivation)
      ( using variantIndex: Int & VariantIndex[variant] aka "index",
              reflection:   SumReflection[derivation] )
    :   Optional[variant] =

      type Labels = reflection.MirroredElemLabels
      type Variants = reflection.MirroredElemTypes

      val size: Int = valueOf[Tuple.Size[reflection.MirroredElemTypes]]

      fold[derivation, Variants, Labels](sum, size, 0, false)(index == reflection.ordinal(sum)):
        [variant2 <: derivation] => field =>
          if index == variantIndex() then field.asInstanceOf[variant] else Unset


    protected inline def variantLabels[derivation](using reflection: SumReflection[derivation])
    :   List[Text] =

      constValueTuple[reflection.MirroredElemLabels].toList.map(_.toString.tt)


    // A value-less fold over every variant of the sum — the sum analogue of `contexts` — yielding
    // the `lambda`'s result for each, with its typeclass instance, label and index in scope. Used
    // for schemas and other instance-free derivations needing a variant without a dispatch value.
    protected transparent inline def choices[derivation]
      ( using reflection: SumReflection[derivation] )
      [ result ]
      ( inline lambda:  [variant <: derivation] => typeclass[variant]
                        =>  ( typeclass[variant] aka "contextual",
                              Text aka "label",
                              Int & VariantIndex[variant] aka "index" ) ?=> result )
    :   IArray[result] =

      type Labels = reflection.MirroredElemLabels
      type Variants = reflection.MirroredElemTypes

      provide[ClassTag[result]]:
        val array = new Array[result](valueOf[Tuple.Size[Variants]])

        choicesFold[derivation, Variants, Labels, Unit]((), 0): accumulator =>
          [variant <: derivation] => context ?=>
            array(index) = lambda[variant](context)

        array.immutable(using Unsafe)


    private transparent inline def choicesFold
      [ derivation, variants <: Tuple, labels <: Tuple, result ]
      ( inline accumulator: result, index: Int )
      ( inline lambda:  result => [variant <: derivation] => typeclass[variant]
                        =>  ( typeclass[variant] aka "contextual",
                              Text aka "label",
                              (Int & VariantIndex[variant]) aka "index" ) ?=> result )
    :   result =

      inline !![variants] match
        case _: Zero => accumulator

        case _: (variant *: moreVariants) =>
          inline !![labels] match
            case _: (label *: moreLabels) => inline valueOf[label].asMatchable match
              case label: String =>
                type variant0 = variant & derivation

                val typeclass = wisteria.internal.field[typeclass, variant0]

                val variantIndex: Int & VariantIndex[variant0] =
                  VariantIndex[variant0](index)

                val accumulator2 =
                  lambda(accumulator)[variant0](typeclass)
                    ( using typeclass.aka["contextual"],
                            label.tt.aka["label"],
                            variantIndex.aka["index"] )

                choicesFold[derivation, moreVariants, moreLabels, result]
                  ( accumulator2, index + 1 )
                  ( lambda )


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


    protected transparent inline def delegate[derivation](delegation: Text)
      ( using reflection: SumReflection[derivation] )
      [ result ]
      ( inline lambda:  [variant <: derivation] => typeclass[variant]
                        =>  ( typeclass[variant] aka "contextual",
                              Text aka "label",
                              Int & VariantIndex[variant] aka "index" ) ?=> result )
    :   result =

      type Labels = reflection.MirroredElemLabels
      type Variants = reflection.MirroredElemTypes

      val size: Int = valueOf[Tuple.Size[reflection.MirroredElemTypes]]

      // Here label comes from context of fold's predicate
      fold[derivation, Variants, Labels](delegation, size, 0, true)(label == delegation):
        [variant <: derivation] =>
          lambda[variant](_)

      . vouch


    protected transparent inline def variant[derivation](sum: derivation)
      ( using reflection: SumReflection[derivation] )
      [ result ]
      ( inline lambda:  [variant <: derivation]
                        =>  variant
                        =>  ( typeclass[variant] aka "contextual",
                              Text aka "label",
                              Int & VariantIndex[variant] aka "index" ) ?=> result )
    :   result =

      type Labels = reflection.MirroredElemLabels
      type Variants = reflection.MirroredElemTypes

      val size: Int = valueOf[Tuple.Size[reflection.MirroredElemTypes]]

      fold[derivation, Variants, Labels](sum, size, 0, false)(index == reflection.ordinal(sum)):
        [variant <: derivation] => variant =>
          lambda[variant](variant)

      . vouch


    private transparent inline def fold[derivation, variants <: Tuple, labels <: Tuple]
      ( inline inputLabel: Text, size: Int, index: Int, fallible: Boolean )
      ( using reflection: SumReflection[derivation] )
      ( inline predicate:
          (Text aka "label", Int & VariantIndex[derivation] aka "index") ?=> Boolean )
      [ result ]
      ( inline lambda:  [variant <: derivation]
                        =>  typeclass[variant]
                        =>  ( typeclass[variant] aka "contextual",
                              Text aka "label",
                              Int & VariantIndex[variant] aka "index" ) ?=> result )
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

                    if predicate(using label.tt.aka["label"], index2.aka["index"])
                    then
                      val index3: Int & VariantIndex[variant0] = VariantIndex[variant0](index)
                      val context = wisteria.internal.field[typeclass, variant0]

                      lambda[variant0](context)
                        ( using context.aka["contextual"],
                                label.tt.aka["label"],
                                index3.aka["index"] )
                    else
                      fold
                        [ derivation, variants, moreLabels ]
                        ( inputLabel, size, index + 1, fallible )
                        ( predicate )
                        ( lambda )

        case _ =>
          inline if fallible
          then provide[Tactic[VariantError]]:
            abort(VariantError[derivation](inputLabel))
          else panic(m"Should be unreachable")


    private transparent inline def fold[derivation, variants <: Tuple, labels <: Tuple]
      ( inline sum: derivation, size: Int, index: Int, fallible: Boolean )
      ( using reflection: SumReflection[derivation] )
      ( inline predicate: ( label: Text aka "label",
                            index: Int & VariantIndex[derivation] aka "index" )
                          ?=> Boolean )
      [ result ]
      ( inline lambda:  [variant <: derivation]
                        =>  variant
                        =>  ( typeclass[variant] aka "contextual",
                              Text aka "label",
                              Int & VariantIndex[variant] aka "index" ) ?=> result )
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

                    if predicate(using label.tt.aka["label"], index2.aka["index"]) then
                      val index3: Int & VariantIndex[variant0] = VariantIndex[variant0](index)
                      val variant: variant0 = sum.asInstanceOf[variant0]
                      val context = wisteria.internal.field[typeclass, variant0]

                      lambda[variant0](variant)
                        ( using context.aka["contextual"],
                                label.tt.aka["label"],
                                index3.aka["index"] )
                    else
                      fold[derivation, variants, moreLabels](sum, size, index + 1, fallible)
                        ( predicate )
                        ( lambda )

        case _ =>
          inline if fallible
          then provide[Tactic[VariantError]](abort(VariantError[derivation]("".tt)))
          else panic(m"Should be unreachable")


    inline def disjunction[derivation: SumReflection]: typeclass[derivation]

trait SumDerivation[typeclass[_]] extends SumDerivation.Methods[typeclass]:
  inline def derivedOne[derivation]: typeclass[derivation] =
    disjunction[derivation](using summonInline[SumReflection[derivation]]).asMatchable match
      case typeclass: typeclass[`derivation`] => typeclass

  inline given derived: [derivation] => Reflection[derivation] => typeclass[derivation] =
    ${wisteria.internal.deriveGraph[typeclass, derivation]('this)}
