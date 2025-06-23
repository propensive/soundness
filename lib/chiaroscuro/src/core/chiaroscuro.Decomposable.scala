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
┃    Soundness, version 0.34.0.                                                                    ┃
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
import fulminate.*
import gossamer.*
import kaleidoscope.*
import larceny.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*
import wisteria.*

import scala.compiletime.*
import scala.deriving.*
import scala.reflect.*

trait Decomposable extends Typeclass:
  def decomposition(value: Self): Decomposition

object Decomposable:
  inline given derived: [entity] => entity is Decomposable = summonFrom:
    case decomposable: (`entity` is Decomposable.Foundation) => decomposable
    case given ProductReflection[`entity`] => Derivation.derived[entity]
    case given SumReflection[`entity`]     => Derivation.split[entity]
    case given (AnyRef <:< `entity`)       => any[entity]

    case given (Unset.type <:< `entity`) => inline !![entity] match
      case _: Optional[inner] => summonFrom:
        case decomposable: (`inner` is Decomposable) =>
          value =>
            val inside = value match
              case Unset => Decomposition.Primitive(t"Unset", t"∅", Unset)
              case other => decomposable.decomposition(other.asInstanceOf[inner])

            Decomposition.Sum(t"Optional", inside, value)

        case _ =>
          any[entity]

      case _ =>
        any[entity]


    case given (`entity` is Showable) =>
      value => Decomposition.Primitive(shortName[entity], value.show, value)

    case given (`entity` is Encodable in Text) =>
      value => Decomposition.Primitive(shortName[entity], value.encode, value)

    case _ =>
      any[entity]

  inline def primitive[value](name: Text): value is Decomposable =
    value => Decomposition.Primitive(name, value.toString.tt, value)

  inline def any[value]: value is Decomposable =
    value => Decomposition.Primitive(t"Any", value.toString.tt, value)

  trait Foundation extends Decomposable:
    type Self
    def decomposition(value: Self): Decomposition

  object Foundation:
    given text: Text is Decomposable.Foundation =
      value => Decomposition.Primitive(t"Text", value, value)

    given int: Int is Decomposable.Foundation =
      value => Decomposition.Primitive(t"Int", value.show, value)

    given string: String is Decomposable.Foundation =
      value => Decomposition.Primitive("String", value, value)

    given compileError: CompileError is Decomposable.Foundation =
      value => Decomposition.Primitive("CompileError", value.toString.tt, value)

    given decomposition: Decomposition is Decomposable.Foundation = identity(_)

    given sequence: [element: Decomposable, collection <: Iterable[element]]
          => collection is Decomposable.Foundation =
      collection =>
        Decomposition.Sequence(collection.map(_.decompose).to(List), collection)

  private val pattern = r"(.*\.)*"
  private inline def shortName[entity]: Text = typeName[entity].sub(pattern, t"")

  object Derivation extends Derivable[Decomposable]:
    inline def join[derivation <: Product: ProductReflection]: derivation is Decomposable =
      value =>
        val map =
          fields(value) { [field] => field => label -> context.decomposition(field) }.to(Map)

        Decomposition.Product(typeName, map, value)

    inline def split[derivation: SumReflection]: derivation is Decomposable =
      value =>
        variant(value):
          [variant <: derivation] => variant =>
            Decomposition.Sum(typeName, context.decomposition(variant), variant)
