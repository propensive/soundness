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

import anticipation.*
import denominative.*
import vacuous.*

object ProductDerivation:
  trait Methods[typeclass[_]]:
    private transparent inline def buildFields[derivation <: Product]
      ( inline lambda:  [field] => typeclass[field]
                        =>  ( typeclass[field] aka "contextual",
                              Default[Optional[field]],
                              Text aka "label",
                              Int & FieldIndex[field] aka "index" ) ?=> field )
    :   Tuple =

      ${wisteria.internal.buildProduct[typeclass, derivation]('lambda)}


    // Construction is inline `Mirror.fromProduct`, using the `Mirror` threaded in via the
    // `ProductReflection` context bound (from the caller's `summonFrom`) — NOT a fresh
    // `summonInline` and NOT macro-emitted. This is the only construction form that works for
    // method-local and object-nested case classes: `fromProduct` captures the outer pointer from
    // the surrounding frame. The macro (`buildFields`) supplies only the field-value tuple;
    // `fromProduct` returns the `Mirror`'s `MirroredMonoType` (= `derivation`), so no cast is
    // needed — and a cast to a method-local `derivation` would re-introduce a `This`-ref that
    // crashes erasure ("missing outer accessor").
    protected transparent inline def build[derivation <: Product]
      ( using reflection: ProductReflection[derivation] )
      ( inline lambda:  [field] => typeclass[field]
                        =>  ( typeclass[field] aka "contextual",
                              Default[Optional[field]],
                              Text aka "label",
                              Int & FieldIndex[field] aka "index" ) ?=> field )
    :   derivation =

      reflection.fromProduct(buildFields[derivation](lambda))


    protected transparent inline def construct[constructor[_], derivation <: Product]
      ( inline bind:  [input, output] => constructor[input] => (input => constructor[output])
                      =>  constructor[output],
        inline pure:    [monadic] => monadic => constructor[monadic],
        inline lambda:  [field] => typeclass[field]
                        =>  ( typeclass[field] aka "contextual",
                              Default[Optional[field]],
                              Text aka "label",
                              Int & FieldIndex[field] aka "index" )
                        ?=> constructor[field] )
    :   constructor[derivation] =

      ${wisteria.internal.constructMonadic[typeclass, constructor, derivation]('bind, 'pure,
          'lambda)}


    protected transparent inline def contexts[derivation <: Product]
      ()
      [ result ]
      ( inline lambda:  [field] => typeclass[field]
                        =>  ( typeclass[field] aka "contextual",
                              Default[Optional[field]],
                              Text aka "label",
                              (derivation => field) aka "dereference",
                              Int & FieldIndex[field] aka "index" ) ?=> result )
    :   IArray[result] =

      ${wisteria.internal.contextsProduct[typeclass, derivation, result]('lambda)}


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
      ( using fieldIndex:  Int & FieldIndex[field] aka "index" )
    :   field =

      product.productElement(fieldIndex.asInstanceOf[Int]).asInstanceOf[field]


    protected transparent inline def fields[derivation <: Product](inline product: derivation)
      [ result ]
      ( inline lambda:  [field] => field
                        =>  ( typeclass[field] aka "contextual",
                              Default[Optional[field]],
                              Text aka "label",
                              (Int & FieldIndex[field]) aka "index" ) ?=> result )
    :   IArray[result] =

      ${wisteria.internal.fieldsProduct[typeclass, derivation, result]('product, 'lambda)}


    inline def conjunction[derivation <: Product: ProductReflection]: typeclass[derivation]

trait ProductDerivation[typeclass[_]] extends ProductDerivation.Methods[typeclass]:
  inline def derivedOne[derivation]: typeclass[derivation] =
    conjunction[derivation & Product](using summonInline[ProductReflection[derivation & Product]])
    . asMatchable.match
      case typeclass: typeclass[`derivation`] => typeclass

  inline given derived: [derivation] => Reflection[derivation] => typeclass[derivation] =
    ${wisteria.internal.deriveGraph[typeclass, derivation]('this)}
