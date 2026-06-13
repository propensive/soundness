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

object ProductDerivation:
  trait Methods[typeclass[_]]:
    protected transparent inline def build[derivation <: Product]
      ( using reflection: ProductReflection[derivation], requirement: ContextRequirement )
      ( inline lambda:  [field] => requirement.Optionality[typeclass[field]]
                        =>  ( requirement.Optionality[typeclass[field]] aka "contextual",
                              Default[Optional[field]],
                              Text aka "label",
                              Int & FieldIndex[field] aka "index" ) ?=> field )
    :   derivation =

      ${wisteria.internal.buildProduct[typeclass, derivation]('lambda, 'requirement)}


    protected transparent inline def construct[constructor[_]]
      ( using requirement: ContextRequirement )
      [ derivation <: Product ]
      ( using reflection: ProductReflection[derivation] )
      ( inline bind:  [input, output] => constructor[input] => (input => constructor[output])
                      =>  constructor[output],
        inline pure:    [monadic] => monadic => constructor[monadic],
        inline lambda:  [field] => requirement.Optionality[typeclass[field]]
                        =>  ( requirement.Optionality[typeclass[field]] aka "contextual",
                              Default[Optional[field]],
                              Text aka "label",
                              Int & FieldIndex[field] aka "index" )
                        ?=> constructor[field] )
    :   constructor[derivation] =

      ${wisteria.internal.constructMonadic[typeclass, constructor, derivation]('bind, 'pure,
          'lambda, 'requirement)}


    protected transparent inline def contexts[derivation <: Product]
      ( using reflection:  ProductReflection[derivation], requirement: ContextRequirement )
      [ result ]
      ( inline lambda:  [field] => requirement.Optionality[typeclass[field]]
                        =>  ( requirement.Optionality[typeclass[field]] aka "contextual",
                              Default[Optional[field]],
                              Text aka "label",
                              (derivation => field) aka "dereference",
                              Int & FieldIndex[field] aka "index" ) ?=> result )
    :   IArray[result] =

      ${wisteria.internal.contextsProduct[typeclass, derivation, result]('lambda, 'requirement)}


    inline def typeName[derivation](using reflection: Reflection[derivation]): Text =
      ${wisteria.internal.typeName[derivation]}

    inline def tuple[derivation](using reflection: Reflection[derivation]): Boolean =
      ${wisteria.internal.isTuple[derivation]}

    inline def singleton[derivation](using reflection: Reflection[derivation]): Boolean =
      ${wisteria.internal.isSingleton[derivation]}


    protected transparent inline def complement[derivation <: Product, field]
      ( product: derivation )
      ( using fieldIndex:  Int & FieldIndex[field] aka "index",
              reflection:  ProductReflection[derivation],
              requirement: ContextRequirement )
    :   field =

      product.productElement(fieldIndex.asInstanceOf[Int]).asInstanceOf[field]


    protected transparent inline def fields[derivation <: Product](inline product: derivation)
      ( using requirement: ContextRequirement )
      ( using reflection: ProductReflection[derivation] )
      [ result ]
      ( inline lambda:  [field] => field
                        =>  ( requirement.Optionality[typeclass[field]] aka "contextual",
                              Default[Optional[field]],
                              Text aka "label",
                              (Int & FieldIndex[field]) aka "index" ) ?=> result )
    :   IArray[result] =

      ${wisteria.internal.fieldsProduct[typeclass, derivation, result]('product, 'lambda,
          'requirement)}


    inline def conjunction[derivation <: Product: ProductReflection]: typeclass[derivation]

trait ProductDerivation[typeclass[_]] extends ProductDerivation.Methods[typeclass]:
  inline given derived: [derivation] => Reflection[derivation] => typeclass[derivation] =
    inline summon[Reflection[derivation]] match
      case reflection: ProductReflection[derivationType] =>
        conjunction[derivationType](using reflection).asMatchable match
          case typeclass: typeclass[`derivation`] => typeclass
