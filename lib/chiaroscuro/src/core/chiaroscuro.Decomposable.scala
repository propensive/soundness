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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import scala.caps
import proscenium.compat.*

import scala.compiletime.*
import scala.reflect.*

import anticipation.*
import gossamer.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*
import wisteria.*

object Decomposable extends Decomposable2:
  object Base:
    given text: Text is Base =
      value => Decomposition.Primitive(t"Text", value, value)

    given int: Int is Base =
      value => Decomposition.Primitive(t"Int", value.show, value)

    given string: String is Base =
      value => Decomposition.Primitive("String", value, value)

    given decomposition: Decomposition is Base = identity(_)

  trait Base extends Decomposable:
    def decomposition(value: Self): Decomposition

  // The collection instances below retain their resolution-scoped element decomposers,
  // which share each instance's given-resolution lifetime; laundered pure per the
  // codec-thunk seal pattern (see rep/DECISIONS.md).
  given list: [element, collection <: List[element]]
  =>  ( decomposable: => element is Decomposable )
  =>  collection is Decomposable =

    caps.unsafe.unsafeAssumePure: list =>
        Decomposition.Sequence(t"List", list.stdlib.map(decomposable.decomposition(_)), list)


  given sequence: [element, collection <: Sequence[element]]
  =>  ( decomposable: => element is Decomposable )
  =>  collection is Decomposable =

    caps.unsafe.unsafeAssumePure: sequence =>
        Decomposition.Sequence(t"Sequence", sequence.stdlib.map(decomposable.decomposition(_)), sequence)

  given iarray: [element]
  =>  ( decomposable: => element is Decomposable )
  =>  (Array[element]^{}) is Decomposable =

    caps.unsafe.unsafeAssumePure: iarray =>
        Decomposition.Sequence
          ( t"Array",
            iarray.toSeq.map(decomposable.decomposition(_)),
            iarray )

trait Decomposable extends Typeclass:
  def decomposition(value: Self): Decomposition

trait Decomposable2 extends Decomposable3:
  inline given derived: [entity] => entity is Decomposable = summonFrom:
    case decomposable: (`entity` is Decomposable.Base) => decomposable
    case given ProductReflection[`entity`]             => Derivation.derived[entity]
    case given SumReflection[`entity`]                 => Derivation.disjunction[entity]
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
    inline def conjunction[derivation <: Product: ProductReflection]: derivation is Decomposable =
      value =>
        val map =
          Map.from((fields(value) { [field] => field => label -> contextual.decomposition(field) }).readable)

        Decomposition.Product(typeName, map, value)

    inline def disjunction[derivation: SumReflection]: derivation is Decomposable =
      value =>
        variant(value):
          [variant <: derivation] => variant =>
            Decomposition.Sum(typeName, contextual.decomposition(variant), variant)

  protected inline def shortName[entity]: Text = rewrite(typeName[entity])

  private def rewrite(text: Text): Text = text match
    case r"(.*\.)*$basic([^\]]*)(\[.*\])?" => basic
    case other                             => other

trait Decomposable3:
  given fallback: [value] => value is Decomposable =
    value => Decomposition.Primitive(t"Any", value.toString.tt, value)
