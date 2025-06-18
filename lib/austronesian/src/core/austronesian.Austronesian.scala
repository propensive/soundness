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
package austronesian

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import probably.*
import rudiments.*
import wisteria.*

object Austronesian:
  opaque type Pojo <: Object =
    Array[Object] | String | java.lang.Boolean | java.lang.Byte | java.lang.Character
    | java.lang.Short | java.lang.Integer | java.lang.Long | java.lang.Float | java.lang.Double

  object Pojo extends Pojo2:
    def apply
         (pojo: Array[Object] | String | java.lang.Boolean | java.lang.Byte | java.lang.Character
                | java.lang.Short | java.lang.Integer | java.lang.Long | java.lang.Float
                | java.lang.Double)
    : Pojo =

        pojo


    given text: Text is Encodable in Pojo = _.s
    given string: String is Encodable in Pojo = identity(_)
    given int: Int is Encodable in Pojo = identity(_)
    given long: Long is Encodable in Pojo = identity(_)
    given float: Float is Encodable in Pojo = identity(_)
    given double: Double is Encodable in Pojo = identity(_)
    given char: Char is Encodable in Pojo = identity(_)
    given boolean: Boolean is Encodable in Pojo = identity(_)
    given byte: Byte is Encodable in Pojo = identity(_)

    given list: [collection <: Iterable, element: Encodable in Pojo]
          =>  collection[element] is Encodable in Pojo =
      iterable => Array.from[Object](iterable.map(_.encode.asInstanceOf[Object]))

    given text2: Tactic[PojoError] => Text is Decodable in Pojo =
      case string: String => string.tt
      case _              => raise(PojoError()) yet "".tt

    given string2: Tactic[PojoError] => String is Decodable in Pojo =
      case string: String => string
      case _              => raise(PojoError()) yet ""

    given int2: Tactic[PojoError] => Int is Decodable in Pojo =
      case int: Int => int
      case _        => raise(PojoError()) yet 0

    given long2: Tactic[PojoError] => Long is Decodable in Pojo =
      case long: Long => long
      case _          => raise(PojoError()) yet 0L

    given float2: Tactic[PojoError] => Float is Decodable in Pojo =
      case float: Float => float
      case _            => raise(PojoError()) yet 0.0f

    given double2: Tactic[PojoError] => Double is Decodable in Pojo =
      case double: Double => double
      case _              => raise(PojoError()) yet 0.0

    given char2: Tactic[PojoError] => Char is Decodable in Pojo =
      case char: Char => char
      case _          => raise(PojoError()) yet '\u0000'

    given boolean2: Tactic[PojoError] => Boolean is Decodable in Pojo =
      case boolean: Boolean => boolean
      case _                => raise(PojoError()) yet false

    given collection: [collection <: Iterable, element: Decodable in Pojo]
          =>  Tactic[PojoError]
          => (factory: scala.collection.Factory[element, collection[element]])
          =>  collection[element] is Decodable in Pojo =

      case array: Array[Pojo @unchecked] =>
        factory.newBuilder.pipe: builder =>
          array.each(builder += _.decode)
          builder.result()

      case other =>
        raise(PojoError()) yet factory.newBuilder.result()

    extension (pojo: Pojo)
      def as[entity: Decodable in Pojo]: entity = entity.decoded(pojo)

  trait Pojo2:
    given checkable: Pojo is Checkable against Pojo = (left, right) =>
      left match
        case left: Array[?] => right match
          case right: Array[?] =>
            left.length == right.length && left.indices.forall: index =>
              left(index) match
                case left: Pojo @unchecked => right(index) match
                  case right: Pojo @unchecked => checkable.check(left, right)
                  case _                      => false
                case _                     => false
          case _               => false

        case left           =>
          left == right

    inline given encodable: [value: Reflection] => value is Encodable in Pojo =
      Austronesian2.EncodableDerivation.derived

    inline given decodable: [value: Reflection] => value is Decodable in Pojo =
      Austronesian2.DecodableDerivation.derived
