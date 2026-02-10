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
package austronesian

import anticipation.*
import contingency.*
import distillate.*
import prepositional.*
import probably.*
import proscenium.*
import rudiments.*
import wisteria.*

object Austronesian:
  opaque type Pojo <: Object =
    Array[Object] | String | java.lang.Boolean | java.lang.Byte | java.lang.Character
    | java.lang.Short | java.lang.Integer | java.lang.Long | java.lang.Float | java.lang.Double

  object Pojo extends Pojo2:
    def apply
      ( pojo: Array[Object] | String | java.lang.Boolean | java.lang.Byte | java.lang.Character
              | java.lang.Short | java.lang.Integer | java.lang.Long | java.lang.Float
              | java.lang.Double )
    : Pojo =

        pojo


    inline given text: Text is Encodable in Pojo = _.s
    inline given string: String is Encodable in Pojo = identity(_)
    inline given int: Int is Encodable in Pojo = identity(_)
    inline given long: Long is Encodable in Pojo = identity(_)
    inline given float: Float is Encodable in Pojo = identity(_)
    inline given double: Double is Encodable in Pojo = identity(_)
    inline given char: Char is Encodable in Pojo = identity(_)
    inline given boolean: Boolean is Encodable in Pojo = identity(_)
    inline given byte: Byte is Encodable in Pojo = identity(_)

    // Check whether these should be kept, or the `inline given` below
    // given list: [list <: List, element: Encodable in Pojo] => list[element] is Encodable in Pojo =
    //   list => IArray.from(list.map(_.encode))

    // given trie: [trie <: Trie, element: Encodable in Pojo] => trie[element] is Encodable in Pojo =
    //   trie => IArray.from(trie.map(_.encode))

    inline given list: [collection <: Iterable, element: Encodable in Pojo]
    =>  collection[element] is Encodable in Pojo =

        iterable => Array.from[Object](iterable.map(_.encode.asInstanceOf[Object]))

    inline given text2: Text is Decodable in Pojo = _.asInstanceOf[String].tt
    inline given string2: String is Decodable in Pojo = _.asInstanceOf[String]
    inline given int2: Int is Decodable in Pojo = _.asInstanceOf[Int]
    inline given long2: Long is Decodable in Pojo = _.asInstanceOf[Long]
    inline given float2: Float is Decodable in Pojo = _.asInstanceOf[Float]
    inline given double2: Double is Decodable in Pojo = _.asInstanceOf[Double]
    inline given char2: Char is Decodable in Pojo = _.asInstanceOf[Char]
    inline given boolean2: Boolean is Decodable in Pojo = _.asInstanceOf[Boolean]

    inline given collection
    : [ collection <: Iterable, element: Decodable in Pojo ]
    =>  Tactic[PojoError]
    =>  ( factory: scala.collection.Factory[element, collection[element]] )
    =>  collection[element] is Decodable in Pojo =

      case array: Array[Pojo @unchecked] =>
        factory.newBuilder.pipe: builder =>
          array.each(builder += _.decode)
          builder.result()

      case other =>
        raise(PojoError()) yet factory.newBuilder.result()

    extension (pojo: Pojo)
      inline def as[entity: Decodable in Pojo]: entity = entity.decoded(pojo)

  trait Pojo2:
    inline given checkable: Pojo is Checkable against Pojo = (left, right) =>
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
