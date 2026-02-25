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
package chiaroscuro

import anticipation.*
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
import scala.reflect.*

object Decomposable extends Decomposable2:
  trait Base extends Decomposable:
    def decomposition(value: Self): Decomposition


  given list: [element, collection <: List[element]]
  =>  ( decomposable: => element is Decomposable )
  =>  collection is Decomposable =

    list =>
      Decomposition.Sequence(t"List", list.map(decomposable.decomposition(_)), list)


  given trie: [element, collection <: Trie[element]]
  =>  ( decomposable: => element is Decomposable )
  =>  collection is Decomposable =

    trie =>
      Decomposition.Sequence(t"Trie", trie.map(decomposable.decomposition(_)).to(List), trie)


  given iarray: [element]
  =>  ( decomposable: => element is Decomposable )
  =>  IArray[element] is Decomposable =

    iarray =>
      Decomposition.Sequence
        ( t"IArray", iarray.map(decomposable.decomposition(_)).to(List), iarray )


  object Base:
    given text: Text is Base =
      value => Decomposition.Primitive(t"Text", value, value)

    given int: Int is Base =
      value => Decomposition.Primitive(t"Int", value.show, value)

    given string: String is Base =
      value => Decomposition.Primitive("String", value, value)

    given compileError: CompileError is Base =
      value => Decomposition.Primitive("CompileError", value.toString.tt, value)

    given decomposition: Decomposition is Base = identity(_)

trait Decomposable extends Typeclass:
  def decomposition(value: Self): Decomposition


trait Decomposable2 extends Decomposable3:
  inline given derived: [entity] => entity is Decomposable = summonFrom:
    case decomposable: (`entity` is Decomposable.Base) => decomposable
    case given ProductReflection[`entity`]             => Derivation.derived[entity]
    case given SumReflection[`entity`]                 => Derivation.split[entity]
    case given (AnyRef <:< `entity`)                   => any[entity]

    case given (Unset.type <:< `entity`) =>
      inline !![entity] match
        case _: Optional[inner] => summonFrom:
          case decomposable: (`inner` is Decomposable) =>
            value =>
              val inside = value match
                case Unset => Decomposition.Primitive(t"Unset", t"∅", Unset)
                case other => decomposable.decomposition(other.asInstanceOf[inner])

              Decomposition.Sum(t"Optional", inside, value)

    case given (`entity` is Showable) =>
      value => Decomposition.Primitive(shortName[entity], value.show, value)

    case given (`entity` is Encodable in Text) =>
      value => Decomposition.Primitive(shortName[entity], value.encode, value)

    case _ =>
      value => Decomposition.Primitive(t"Any", value.toString.tt, value)

  def primitive[value](name: Text): value is Decomposable =
    value => Decomposition.Primitive(name, value.toString.tt, value)

  def any[value]: value is Decomposable =
    value => Decomposition.Primitive(t"Any", value.toString.tt, value)


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

  protected inline def shortName[entity]: Text = rewrite(typeName[entity])
  private def rewrite(text: Text): Text = text match
    case r"(.*\.)*$basic([^\]]*)(\[.*\])?" => basic
    case other                             => other

trait Decomposable3:
  given fallback: [value] => value is Decomposable =
    value => Decomposition.Primitive(t"Any", value.toString.tt, value)
