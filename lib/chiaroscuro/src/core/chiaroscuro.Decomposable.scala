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
┃    Soundness, version 0.27.0.                                                                    ┃
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
package chiaroscuro

import anticipation.*
import gossamer.*
import prepositional.*
import proscenium.*
import spectacular.*
import vacuous.*
import wisteria.*

import scala.deriving.*
import scala.reflect.*

trait Decomposable:
  type Self
  def decompose(value: Self): Decomposition

object Decomposable extends Derivable[Decomposable], Decomposable2:

  def primitive[ValueType]: ValueType is Decomposable =
    value => Decomposition.Primitive(t"", value.toString.tt, value)

  inline def join[DerivationType <: Product: ProductReflection]: DerivationType is Decomposable =
    value =>
      val map: Map[Text, Decomposition] =
        import derivationContext.relaxed
        fields(value):
          [FieldType] => field =>
            val value =
              context.let(_.decompose(field)).or(Decomposition.Primitive(typeName, t"?", field))

            label -> value
        . to(Map)

      Decomposition.Product(typeName, map, value)

  inline def split[DerivationType: SumReflection]: DerivationType is Decomposable =
    value =>
      import derivationContext.relaxed
      variant(value):
        [VariantType <: DerivationType] => variant =>
          Decomposition.Sum
           (typeName,
            context.let(_.decompose(variant)).or:
              Decomposition.Primitive(typeName, t"?", variant),
            variant)

  given text: Text is Decomposable = value => Decomposition.Primitive(t"Text", value, value)

  given optional: [ValueType: Decomposable] => Optional[ValueType] is Decomposable = value =>
    value.let: value =>
      Decomposition.Sum(t"Optional", value.decompose, value)
    . or(Decomposition.Sum(t"Optional", Decomposition.Primitive(t"Unset", t"∅", value), value))

trait Decomposable2:
  inline given textual: [ValueType] => ValueType is Decomposable = compiletime.summonFrom:
    case given (ValueType is Showable) =>
      value => Decomposition.Primitive(t"Showable", value.show, value)

    case given (ValueType is Encodable in Text) =>
      value => Decomposition.Primitive(t"Encodable", value.encode, value)

    case _ =>
      value => Decomposition.Primitive(t"Any", value.toString.tt, value)

  given collection: [CollectionType <: Iterable, ValueType: Decomposable]
  =>    CollectionType[ValueType] is Decomposable =
    collection =>
      Decomposition.Sequence(IArray.from(collection.map(_.decompose)), collection)
