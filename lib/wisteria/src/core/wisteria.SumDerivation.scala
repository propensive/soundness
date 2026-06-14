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
import denominative.*
import vacuous.*

object SumDerivation:
  trait Methods[typeclass[_]]:

    transparent inline def choice[derivation: SumReflection]: Boolean =
      ${wisteria.internal.isChoice[derivation]}


    protected transparent inline def complement[derivation, variant](sum: derivation)
      ( using variantIndex: Int & VariantIndex[variant] aka "index",
              reflection:   SumReflection[derivation] )
    :   Optional[variant] =

      ${wisteria.internal.complementVariant[derivation, variant]('sum, 'variantIndex)}


    protected inline def variantLabels[derivation](using reflection: SumReflection[derivation])
    :   List[Text] =

      ${wisteria.internal.variantLabels[derivation]}


    protected transparent inline def singleton[derivation](input: Text)
      ( using reflection: SumReflection[derivation] )
    :   derivation =

      ${wisteria.internal.singletonByLabel[derivation]('input)}


    protected transparent inline def delegate[derivation](delegation: Text)
      ( using reflection: SumReflection[derivation] )
      [ result ]
      ( inline lambda:  [variant <: derivation] => typeclass[variant]
                        =>  ( typeclass[variant] aka "contextual",
                              Text aka "label",
                              Int & VariantIndex[variant] aka "index" ) ?=> result )
    :   result =

      ${wisteria.internal.delegateDispatch[typeclass, derivation, result]('delegation, 'lambda)}


    protected transparent inline def variant[derivation](sum: derivation)
      ( using reflection:  SumReflection[derivation] )
      [ result ]
      ( inline lambda:  [variant <: derivation]
                        =>  variant
                        =>  ( typeclass[variant] aka "contextual",
                              Text aka "label",
                              Int & VariantIndex[variant] aka "index" ) ?=> result )
    :   result =

      ${wisteria.internal.variantDispatch[typeclass, derivation, result]('sum, 'lambda)}


    inline def disjunction[derivation: SumReflection]: typeclass[derivation]

trait SumDerivation[typeclass[_]] extends SumDerivation.Methods[typeclass]:
  inline given derived: [derivation] => Reflection[derivation] => typeclass[derivation] =
    inline summon[Reflection[derivation]] match
      case reflection: SumReflection[derivationType] =>
        disjunction[derivationType](using reflection).asMatchable match
          case typeclass: typeclass[`derivation`] => typeclass
